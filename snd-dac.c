#include "snd.h"

/* this was sound-oriented; changed to be channel-oriented 31-Aug-00 */
/* removed reverb-control-procedures (with freeverb and fcomb) and contrast-control-procedure 13-Dec-01 */

/*
 * each channel currently being played has an associated dac_info struct
 *   all active dac_info structs are held in a play_list
 *   channels can come and go as a play is in progress
 */

#ifndef DEFAULT_NEVER_SPED
  #define DEFAULT_NEVER_SPED 1
#endif
/* the default case is to bypass the sample-rate converter if the user hasn't changed it from 1.0;
 *   since the default src method is sinc-interpolation, this can save tons of cycles during most
 *   normal plays, but introduces a click if the user subsequently moves the speed slider.  To
 *   have the src process running all the time (the default before 5-Feb-01), pass the compiler
 *   -DDEFAULT_NEVER_SPED = 0
 */

/* -------------------------------- per-channel control-panel state -------------------------------- */

typedef struct {
  mus_any *gen;
  struct dac__info *dp;
  int speeding;
  Float sr;
} spd_info;

typedef struct dac__info {
  Float cur_index;
  Float cur_amp;
  Float cur_srate;
  Float cur_exp;
  Float cur_rev;       /* rev scaler -- len is set at initialization */
  Float contrast_amp;
  int expanding, reverbing, filtering; /* these need lots of preparation, so they're noticed only at the start */
  int audio_chan;      /* where channel's output is going (wrap-around if not enough audio output channels) */
  int slot;
  Float lst, nxt, x;   /* used if linear interp for src */
  Float *a;            /* filter coeffs */
  snd_fd *chn_fd;      /* sample reader */
  spd_info *spd;
  mus_any *flt;
  int region, selection; /* to reset region-browser play button upon completion */
  src_state *src;
  snd_info *sp;        /* needed to see button callback changes etc */
  chan_info *cp;
  snd_state *ss;
  int end, no_scalers, never_sped, expand_ring_frames;
} dac_info;

#define AMP_CONTROL(sp, dp) ((dp->cp->amp_control) ? (dp->cp->amp_control[0]) : sp->amp_control)
/* an experiment */


/* -------- filter -------- */
static mus_any *make_flt(dac_info *dp, int order, Float *env)
{
  if (order <= 0) return(NULL);
  dp->a = (Float *)CALLOC(order, sizeof(Float));
  if (env) mus_make_fir_coeffs(order, env, dp->a);
  return(mus_make_fir_filter(order, dp->a, NULL));
}


/* -------- sample-rate conversion -------- */
static Float speed(dac_info *dp, Float sr)
{
  int move, i;
  Float result = 0.0;
  if (dp->never_sped)
    return(next_sample_to_float(dp->chn_fd));
  if ((use_sinc_interp((dp->ss))) && (dp->src))
    result = run_src(dp->src, sr);
  else
    {
      if (sr > 0.0) 
	{
	  result = dp->lst + dp->x * (dp->nxt - dp->lst);
	  dp->x += sr;
	  move = (int)(dp->x);
	  if (move != 0)
	    {
	      dp->x -= move;
	      for (i = 0; i < move; i++)
		{
		  dp->lst = dp->nxt;
		  dp->nxt = next_sample_to_float(dp->chn_fd);
		}
	    }
	}
      else
	{
	  result = dp->lst + dp->x * (dp->nxt - dp->lst);
	  dp->x -= sr;
	  move = (int)(dp->x);
	  if (move != 0)
	    {
	      dp->x -= move;
	      for (i = 0; i < move; i++)
		{
		  dp->lst = dp->nxt;
		  dp->nxt = previous_sample_to_float(dp->chn_fd);
		}
	    }
	}
    }
  return(result);
}

/* -------- granular synthesis -------- */
static Float expand_input_as_needed(void *arg, int dir) 
{
  spd_info *spd = (spd_info *)arg;
  dac_info *dp;
  dp = spd->dp;
  if (spd->speeding)
    return(speed(dp, spd->sr));
  else return(next_sample_to_float(dp->chn_fd));
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
				.6, sp->expand_control_hop, sp->expand_control_ramp, .1,
				max_expand_control_len(sp), (void *)spd);
  spd->dp = dp;
  spd->speeding = 0;
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
  int speeding;
  snd_info *sp;
  spd_info *spd;
  sp = dp->sp;
  speeding = ((sp->speed_control_direction != 1) || (sp->speed_control != 1.0) || (dp->cur_srate != 1.0));
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

static int prime (int num)
{
  int lim, i;
  if (num == 2) return(1);
  if ((num % 2) == 1)
    {
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

static void reverb(rev_info *r, Float **rins, MUS_SAMPLE_TYPE **outs, int ind)
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
  int i;
  rev_info *r;
  r = global_rev;
  if (r)
    {
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
      r->combs[i] = mus_make_comb(comb_factors[i] * sp->reverb_control_feedback, dly_len[i], NULL, dly_len[i]);
    }
  r->onep = mus_make_one_pole(sp->reverb_control_lowpass, sp->reverb_control_lowpass - 1.0);
  for (i = 0, j = r->num_combs; i < 4; i++, j++) 
    r->allpasses[i] = mus_make_all_pass(-0.700, 0.700, dly_len[j], NULL, dly_len[j]);
  for (i = 0, j = 10; i < chans; i++)
    {
      if (j < BASE_DLY_LEN) 
	len = dly_len[j]; 
      else len = get_prime((int)(40 + mus_random(20.0)));
      r->allpasses[i + 4] = mus_make_all_pass(-0.700, 0.700, len, NULL, len);
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
      mus_set_a0(global_rev->onep, newval);
      mus_set_b1(global_rev->onep, 1.0 - newval);
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


Float list_interp(Float x, Float *e, int pts)
{
  if (pts == 0) return(0.0);
  if ((x <= e[0]) || (pts == 1)) return(e[1]);
  if (e[2] > x)
    {
      if (e[1] == e[3]) return(e[1]);
      return(e[1] + (x - e[0]) * (e[3] - e[1]) / (e[2] - e[0]));
    }
  return(list_interp(x, (Float *)(e + 2), pts - 1));
}

static Float *sample_linear_env(env *e, int order)
{
  Float *data;
  Float last_x, step, x;
  int i, j;
  data = (Float *)CALLOC(order, sizeof(Float));
  last_x = e->data[(e->pts - 1) * 2];
  step = 2 * last_x / ((Float)order - 1);
  for (i = 0, x = 0.0; i < order / 2; i++, x += step) 
    data[i] = list_interp(x, e->data, e->pts);
  for (j = order / 2 - 1, i = order / 2; (i < order) && (j >= 0); i++, j--) 
    data[i] = data[j];
  return(data);
}

static dac_info *make_dac_info(chan_info *cp, snd_info *sp, snd_fd *fd)
{
  dac_info *dp;
  Float *data = NULL;
  dp = (dac_info *)CALLOC(1, sizeof(dac_info));
  dp->region = -1;
  dp->a = NULL;
  dp->no_scalers = no_ed_scalers(cp);
  dp->audio_chan = cp->chan;
  dp->never_sped = DEFAULT_NEVER_SPED;
  if (sp)
    {
      dp->expanding = sp->expand_control_p;
      dp->filtering = ((sp->filter_control_p) && (sp->filter_control_order > 0));
      dp->reverbing = sp->reverb_control_p;
      dp->contrast_amp = sp->contrast_control_amp;
      if ((use_sinc_interp(sp->state)) && 
	  ((sp->speed_control * sp->speed_control_direction) != 1.0))
	dp->src = make_src(sp->state, 0.0, fd, sp->speed_control * sp->speed_control_direction);
      /* that is, if user wants fancy src, he needs to say so before we start */
      if (dp->expanding) 
	{
	  dp->spd = (spd_info *)make_expand(sp, sp->expand_control, dp);
	  dp->expand_ring_frames = (int)(SND_SRATE(sp) * sp->expand_control * sp->expand_control_length * 2);
	}
      if (dp->filtering)
	{
	  sp->filter_control_changed = 0;
	  if (!(sp->filter_control_env)) 
	    dp->filtering = 0;
	  else
	    {
	      data = sample_linear_env(sp->filter_control_env, sp->filter_control_order);
	      dp->flt = make_flt(dp, sp->filter_control_order, data);
	      FREE(data);
	    }
	}
    }
  dp->ss = get_global_state();
  dp->chn_fd = fd;
  dp->sp = sp;
  dp->cp = cp;
  return(dp);
}

static void free_dac_info (dac_info *dp)
{
  if (dp->a) {FREE(dp->a); dp->a = NULL;}
  free_snd_fd(dp->chn_fd);
  dp->chn_fd = NULL;
  if (dp->spd) free_expand(dp->spd);
  if (dp->src) free_src(dp->src);
  if (dp->flt) mus_free(dp->flt);
  FREE(dp);
}


static int dac_max_sounds = 0;
static dac_info **play_list = NULL;
#define INITIAL_MAX_SOUNDS 16
static int play_list_members = 0;
static int max_active_slot = -1;


/* -------------------------------- special "hidden" control panel variables -------------------------------- */

enum {DAC_EXPAND, DAC_EXPAND_RAMP, DAC_EXPAND_LENGTH, DAC_EXPAND_HOP, DAC_EXPAND_SCALER, DAC_CONTRAST_AMP, DAC_REVERB_FEEDBACK, DAC_REVERB_LOWPASS};

static void dac_set_field(snd_info *sp, Float newval, int field)
{
  /* if sp == NULL, sets globally */
  int i, val;
  dac_info *dp;
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
	      for (i = 0; i <= max_active_slot; i++)
		{
		  dp = play_list[i];
		  if ((dp) && ((sp == NULL) || (sp == dp->sp)))
		    {
		      switch (field)
			{
			case DAC_EXPAND: 
			  if (dp->spd)
			    mus_set_increment((dp->spd)->gen, newval); 
			  break;
			case DAC_EXPAND_LENGTH: /* segment length */
			  if (dp->spd)
			    {
			      val = (int)(SND_SRATE(sp) * newval);
			      mus_set_length((dp->spd)->gen, val);
			      mus_set_ramp((dp->spd)->gen, (int)(val * sp->expand_control_ramp));
			    }
			  break;
			case DAC_EXPAND_RAMP: 
			  if (dp->spd)
			    {
			      val = (int)(newval * sp->expand_control_length * SND_SRATE(sp));
			      mus_set_ramp((dp->spd)->gen, val); 
			    }
			  break;
			case DAC_EXPAND_HOP: /* output hop */
			  if (dp->spd)
			    {
			      val = (int)(SND_SRATE(sp) * newval);
			      mus_set_hop((dp->spd)->gen, val); 
			      mus_set_increment((dp->spd)->gen, sp->expand_control);
			    }
			  break;
			case DAC_EXPAND_SCALER:
			  if (dp->spd)
			    mus_set_scaler((dp->spd)->gen, newval); 
			  break;
			case DAC_CONTRAST_AMP:
			  dp->contrast_amp = newval;
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
void dac_set_expand_scaler(snd_info *sp, Float newval) {dac_set_field(sp, newval, DAC_EXPAND_SCALER);} /* not currently accessible */
void dac_set_contrast_amp(snd_info *sp, Float newval) {dac_set_field(sp, newval, DAC_CONTRAST_AMP);}
void dac_set_reverb_feedback(snd_info *sp, Float newval) {dac_set_field(sp, newval, DAC_REVERB_FEEDBACK);}
void dac_set_reverb_lowpass(snd_info *sp, Float newval) {dac_set_field(sp, newval, DAC_REVERB_LOWPASS);}



/* -------------------------------- stop playing (remove from play-list) -------------------------------- */

static int dac_running = 0;
static XEN play_hook;
static XEN start_playing_hook;
static XEN stop_playing_hook;
static XEN stop_playing_region_hook;
static XEN stop_playing_channel_hook;
static XEN stop_playing_selection_hook;

#define MAX_DEVICES 8
static int dev_fd[MAX_DEVICES];
static void cleanup_dac_hook(void);

void cleanup_dac(void)
{
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
  dac_running = 0;
}

static void reflect_play_stop (snd_info *sp) 
{
#if (!USE_NO_GUI)
  set_control_panel_play_button(sp, FALSE);
#endif
  set_file_browser_play_button(sp->short_filename, 0);
  set_open_file_play_button(0);
  reflect_play_stop_in_popup_menu();
}

static void free_player(snd_info *sp);

static void stop_playing_with_toggle(dac_info *dp, int toggle)
{
  snd_info *sp = NULL;
  int sp_stopping = 0;
  chan_info *cp;
  if ((dp == NULL) || (play_list == NULL)) return;
  sp = dp->sp;
  cp = dp->cp;
  if (sp) 
    {
      sp->playing_mark = NULL;
      if (sp->playing > 0) sp->playing--;
      if (sp->playing == 0) sp_stopping = 1;
      if ((sp->inuse) && (sp->cursor_follows_play != DONT_FOLLOW))
	cursor_moveto(cp, cp->original_cursor);
      if ((sp_stopping) && (sp->cursor_follows_play == FOLLOW_ONCE)) 
	sp->cursor_follows_play = DONT_FOLLOW;
      /* if ctrl-click play, c-t, c-q -> this flag is still set from aborted previous play, so clear at c-t (or c-g) */
    }
  play_list[dp->slot] = NULL;
  play_list_members--;
  if (toggle) 
    {
      if ((sp) && (sp->playing <= 0)) 
	reflect_play_stop(sp);
      else 
	if (dp->region >= 0) 
	  reflect_play_region_stop(dp->region);
    }
  if (dp->slot == max_active_slot) max_active_slot--;
  if (dp->region >= 0)
    {
      if (XEN_HOOKED(stop_playing_region_hook))
	g_c_run_progn_hook(stop_playing_region_hook,
			   XEN_LIST_1(C_TO_SMALL_XEN_INT(dp->region)),
			   S_stop_playing_region_hook);
    }
  else
    {
      if (dp->selection)
	{
	  reflect_play_selection_stop();
	  if (XEN_HOOKED(stop_playing_selection_hook))
	    g_c_run_progn_hook(stop_playing_selection_hook, 
			       XEN_EMPTY_LIST, 
			       S_stop_playing_selection_hook);
	}
      else
	{
	  if ((sp_stopping) && (XEN_HOOKED(stop_playing_hook)))
	    g_c_run_progn_hook(stop_playing_hook,
			       XEN_LIST_1(C_TO_SMALL_XEN_INT(sp->index)),
			       S_stop_playing_hook);
	  if (XEN_HOOKED(stop_playing_channel_hook))
	    g_c_run_progn_hook(stop_playing_channel_hook,
			       XEN_LIST_2(C_TO_SMALL_XEN_INT(sp->index),
					  C_TO_SMALL_XEN_INT(cp->chan)),
			       S_stop_playing_channel_hook);
	  if (sp->index < 0) {free_player(sp); sp = NULL;}
	}
    }
  free_dac_info(dp);
  if ((sp) && (sp_stopping) && (sp->delete_me)) 
    completely_free_snd_info(sp); /* dummy snd_info struct for (play "filename") in snd-xen.c */
}

static void stop_playing(dac_info *dp) {stop_playing_with_toggle(dp, TRUE);}

static void stop_playing_sound_with_toggle(snd_info *sp, int toggle)
{
  /* this needs to scan all current play_list members and remove any that are referring
   * to sp, even indirectly (as through the current selection)
   */
  int i;
  if ((sp) && (play_list))
    for (i = 0; i < dac_max_sounds; i++)
      if ((play_list[i]) && 
	  (sp == (play_list[i]->sp)))
	{
	  stop_playing_with_toggle(play_list[i], toggle);
	  play_list[i] = NULL;
	}
}

void stop_playing_sound(snd_info *sp) {stop_playing_sound_with_toggle(sp, TRUE);}
void stop_playing_sound_no_toggle(snd_info *sp) {stop_playing_sound_with_toggle(sp, FALSE);}

void stop_playing_all_sounds (void)
{
  int i;
  if (play_list)
    for (i = 0; i < dac_max_sounds; i++)
      {
	stop_playing(play_list[i]);
	play_list[i] = NULL;
      }
}

void stop_playing_region(int n)
{
  int i;
  if (play_list)
    for (i = 0; i < dac_max_sounds; i++)
      if ((play_list[i]) &&
	  (play_list[i]->region == n))
	{
	  stop_playing(play_list[i]);
	  play_list[i] = NULL;
	}
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

static dac_info *init_dp(int slot, chan_info *cp, snd_info *sp, snd_fd *fd, int beg, int end)
{
  dac_info *dp;
  play_list_members++;
  dp = make_dac_info(cp, sp, fd);
  dp->end = end;
  if (end != NO_END_SPECIFIED) 
    {
      dp->end -= beg; 
      if (dp->end < 0)
	dp->end = -(dp->end);
    }
  play_list[slot] = dp;
  dp->slot = slot;
  if (max_active_slot < slot) max_active_slot = slot;
  if (sp)
    {
      dp->cur_srate = sp->speed_control * sp->speed_control_direction;
      if (dp->cur_srate != 1.0) dp->never_sped = 0;
      dp->cur_amp = AMP_CONTROL(sp, dp);
      dp->cur_index = sp->contrast_control;
      dp->cur_exp = sp->expand_control;
      dp->cur_rev = sp->reverb_control_scale;
    }
  return(dp);
}

typedef struct {
  int srate;               /* output srate */
  int channels;            /* total output channels currently active */
  int frames;              /* samples per channel per output block */
  int devices;             /* output devices active */
  int *chans_per_device;   /* channels sent to each active device */
  int out_format;          /* output data format */
  int slice;               /* background process state (i.e. starting, running, quitting) */
  int reverb_ring_frames;  /* how long the reverb rings after the end (if reverb, of course) */
  snd_state *ss;
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

static BACKGROUND_TYPE dac_in_background(GUI_POINTER ptr);

#if DEBUGGING
static int disable_play = 0;
static XEN g_disable_play(void) {disable_play = 1; return(XEN_FALSE);}
#endif

static void start_dac(snd_state *ss, int srate, int channels, int background)
{
  dac_info *dp;
  int i;
  /* look for channel folding cases etc */
  /* channels = how many output audio chans we have; dac_combines_channels sets whether to wrap or muffle chans outside this limit */
  for (i = 0; i <= max_active_slot; i++)
    {
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
	      else stop_playing(dp);
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
      snd_dacp->ss = ss;
      snd_dacp->srate = srate;
      snd_dacp->out_format = MUS_COMPATIBLE_FORMAT;
      if (snd_dacp->srate <= 0) snd_dacp->srate = 44100;
#if MAC_OSX
      snd_dacp->channels = 2;
#else
      snd_dacp->channels = channels;
#endif
      snd_dacp->frames = 256; /* just a first guess */
      snd_dacp->devices = 1;  /* just a first guess */
      snd_dacp->reverb_ring_frames = (int)(srate * reverb_control_decay(ss));
#if DEBUGGING
      if (disable_play) 
	{
	  stop_playing_all_sounds();
	  play_list_members = 0; 
	  max_active_slot = -1;
	  free_dac_state();
	}
      else {
#endif
      if (background == IN_BACKGROUND) 
	BACKGROUND_ADD(ss, dac_in_background, NULL);
      else
	{
	  /* here we want to play as an atomic (not background) action */
	  while (dac_in_background(NULL) == BACKGROUND_CONTINUE)
	    {
	      check_for_event(ss); /* need to be able to C-g out of this */
	      /* if ((sp) && (!(sp->inuse))) break; */
	    }
	}
#if DEBUGGING
      }
#endif
    }
}


static dac_info *add_channel_to_play_list(chan_info *cp, snd_info *sp, int start, int end, XEN edpos, const char *caller, int arg_pos)
{
  /* if not sp, control panel is ignored */
  int slot, beg = 0, direction = READ_FORWARD, pos;
  snd_fd *sf;
  pos = to_c_edit_position(cp, edpos, caller, arg_pos);
  if (start >= cp->samples[pos]) return(NULL);
  if (sp)
    {
      if (sp->speed_control_direction == 1) 
	{
	  direction = READ_FORWARD; 
	  beg = start;
	}
      else 
	{
	  direction = READ_BACKWARD;
	  if (start == 0) 
	    beg = cp->samples[pos] - 1;
	  else beg = start;
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
	  if (sp->cursor_follows_play != DONT_FOLLOW)
	    {
	      cp->original_cursor = cp->cursor;
	      cp->cursor_on = 1;
	      cursor_moveto(cp, start);
	    }
	}
      return(init_dp(slot, cp, sp, sf, start, end));
    }
  return(NULL);
}

static dac_info *add_region_channel_to_play_list(int region, int chan, int beg, int end)
{
  int slot;
  snd_fd *fd;
  slot = find_slot_to_play();
  if (slot == -1) return(NULL);
  fd = init_region_read(beg, region, chan, READ_FORWARD);
  if (fd)
    return(init_dp(slot, fd->cp, NULL, fd, beg, end));
  return(NULL);
}

void play_region(snd_state *ss, int region, int background)
{
  /* just plays region (not current selection) -- no control panel etc */
  int chans, i;
  dac_info *dp = NULL;
  if ((background == NOT_IN_BACKGROUND) && (play_list_members > 0)) return;
  if (!(region_ok(region))) return;
  chans = region_chans(region);
  if (chans == 0) return;
  for (i = 0; i < chans; i++) 
    {
      dp = add_region_channel_to_play_list(region, i, 0, NO_END_SPECIFIED);
      if (dp) dp->region = region;
    }
  if (dp) start_dac(ss, region_srate(region), chans, background);
}

void play_channel(chan_info *cp, int start, int end, int background, XEN edpos, const char *caller, int arg_pos)
{
  /* just plays one channel (ignores possible sync) */
  snd_info *sp = NULL;
  dac_info *dp;
  if ((background == NOT_IN_BACKGROUND) && (play_list_members > 0)) return;
  sp = cp->sound;
  if (!(sp->inuse)) return;
  dp = add_channel_to_play_list(cp, sp, start, end, edpos, caller, arg_pos);
  if (dp) start_dac(dp->ss, SND_SRATE(sp), 1, background);
}

void play_sound(snd_info *sp, int start, int end, int background, XEN edpos, const char *caller, int arg_pos)
{
  /* just plays one sound (ignores possible sync) */
  int i;
  dac_info *dp = NULL;
  if ((background == NOT_IN_BACKGROUND) && 
      (play_list_members > 0)) 
    return;
  if (!(sp->inuse)) return;
  if ((XEN_HOOKED(start_playing_hook)) &&
      (XEN_TRUE_P(g_c_run_or_hook(start_playing_hook,
				  XEN_LIST_1(C_TO_SMALL_XEN_INT(sp->index)),
				  S_start_playing_hook))))
    {
      reflect_play_stop(sp);           /* turns off buttons */
      if (sp->delete_me) 
	completely_free_snd_info(sp);  /* dummy snd_info struct for (play "filename") in snd-xen.c */
      return;
    }
  for (i = 0; i < sp->nchans; i++) 
    dp = add_channel_to_play_list(sp->chans[i], sp, start, end, edpos, caller, arg_pos);
  if (dp) start_dac(sp->state, SND_SRATE(sp), sp->nchans, background);
}

void play_channels(chan_info **cps, int chans, int *starts, int *ur_ends, int background, XEN edpos, const char *caller, int arg_pos, int selection)
{
  /* ends can be NULL */
  int i;
  snd_info *sp = NULL;
  dac_info *dp = NULL;
  int *ends;
  if ((background == NOT_IN_BACKGROUND) && 
      (play_list_members > 0)) 
    return;
  if (chans <= 0) return;
  if (ur_ends)
    ends = ur_ends;
  else
    {
      ends = (int *)CALLOC(chans, sizeof(int));
      for (i = 0; i < chans; i++) 
	ends[i] = NO_END_SPECIFIED;
    }
  for (i = 0; i < chans; i++) 
    dp = add_channel_to_play_list(cps[i], 
				  sp = (cps[i]->sound), 
				  starts[i], ends[i],
				  edpos, caller, arg_pos);
  if ((dp) && (selection)) dp->selection = 1;
  if (ur_ends == NULL) FREE(ends);
  if ((sp) && (dp)) start_dac(sp->state, SND_SRATE(sp), chans, background);
}

void play_selection(int background, XEN edpos, const char *caller, int arg_pos)
{
  /* just plays the current selection */
  int i;
  int *ends;
  snd_info *sp;
  sync_info *si = NULL;
  if (selection_is_active())
    {
      si = selection_sync();
      if (si)
	{
	  ends = (int *)CALLOC(si->chans, sizeof(int));
	  for (i = 0; i < si->chans; i++) 
	    {
	      sp = si->cps[i]->sound;
	      if ((sp) && (sp->speed_control != 1.0) && (sp->speed_control > 0.0))
		ends[i] = si->begs[i] + (int)(((Float)selection_len() / (Float)(sp->speed_control)));
	      /* user might move speed control while playing selection, so ideally we'd watch dp->chn_fd here */
	      else ends[i] = si->begs[i] + selection_len();
	    }
	  play_channels(si->cps, si->chans, si->begs, ends, background, edpos, caller, arg_pos, TRUE);
	  si = free_sync_info(si); /* does not free sample readers */
	  FREE(ends);
	}
    }
}


/* -------------------------------- process samples and write to DAC -------------------------------- */

#define NO_CHANGE 0
#define JUST_AMP 1
#define JUST_SPEED 2
#define ALL_CHANGES 3
#define NO_CHANGE_AND_NO_SCALING 4

static int choose_dac_op (dac_info *dp, snd_info *sp)
{
  if (!sp) return(NO_CHANGE);
  if ((dp->expanding) || (dp->filtering) || (dp->reverbing) || (sp->contrast_control_p)) 
    return(ALL_CHANGES);
  else
    {
      if ((sp->speed_control_direction != 1) || (sp->speed_control != 1.0) || (dp->cur_srate != 1.0))
	return(JUST_SPEED);
      else
	{
	  if ((AMP_CONTROL(sp, dp) == dp->cur_amp) && (AMP_CONTROL(sp, dp) == 1.0))
	    {
	      if (dp->no_scalers)
		return(NO_CHANGE_AND_NO_SCALING);
	      else return(NO_CHANGE);
	    }
	  else return(JUST_AMP);
	}
    }
}

#define CURSOR_UPDATE_INTERVAL 1024
static int cursor_time;
/* can't move cursor on each dac buffer -- causes clicks */

static int dac_pausing = 0;
void toggle_dac_pausing(snd_state *ss) {dac_pausing = (!dac_pausing); play_button_pause(ss, dac_pausing);}
int play_in_progress(void) {return(play_list_members > 0);}

static unsigned char **audio_bytes = NULL;
static int audio_bytes_size = 0;
static int audio_bytes_devices = 0;

static MUS_SAMPLE_TYPE **dac_buffers = NULL;
static int dac_buffer_size = 0;
static int dac_buffer_chans = 0; /* chans allocated */
static Float **rev_ins;

#define WRITE_TO_DAC 1
#define WRITE_TO_FILE 0

static void clear_dac_buffers(dac_state *dacp)
{
  int i, frames;
  frames = dacp->frames;
  for (i = 0; i < dacp->channels; i++) 
    memset(dac_buffers[i], 0, frames * sizeof(MUS_SAMPLE_TYPE));
  if (global_rev)
    for (i = 0; i < dacp->channels; i++) 
      memset(rev_ins[i], 0, frames * sizeof(Float));
}

static MUS_SAMPLE_TYPE local_next_sample_unscaled(snd_fd *sf)
{
  if (sf->view_buffered_data > sf->last)
    return(next_sound(sf));
  else return(*sf->view_buffered_data++);
}

static XEN dac_hook;
static XEN stop_dac_hook;
static XEN sdobj = XEN_FALSE;
static void cleanup_dac_hook(void)
{
  if (XEN_HOOKED(stop_dac_hook))
    g_c_run_progn_hook(stop_dac_hook, 
		       XEN_EMPTY_LIST,
		       S_stop_dac_hook);
  if (!(XEN_FALSE_P(sdobj)))
    {
      snd_unprotect(sdobj);
      sdobj = XEN_FALSE;
    }
}

static int fill_dac_buffers(dac_state *dacp, int write_ok)
{
  int i, j, cursor_change;
  int bytes, frames;
  Float *revin;
  Float amp, incr, sr, sincr, ind, indincr, ex, exincr, rev, revincr, fval;
  dac_info *dp;
  snd_info *sp;
  Float *data = NULL;
  snd_state *ss;
  MUS_SAMPLE_TYPE *buf;
#if (HAVE_OSS || HAVE_ALSA)
  MUS_SAMPLE_TYPE **dev_bufs;
#endif

  frames = dacp->frames;
  ss = dacp->ss;
  clear_dac_buffers(dacp);

  if (dac_pausing) 
    cursor_change = 0;
  else
    {
      if (XEN_HOOKED(play_hook))
	g_c_run_progn_hook(play_hook, 
			   XEN_LIST_1(C_TO_XEN_INT(frames)),
			   S_play_hook);
      cursor_time += frames;
      cursor_change = (cursor_time >= CURSOR_UPDATE_INTERVAL);
      for (i = 0; i <= max_active_slot; i++)
	{
	  dp = play_list[i];
	  if (dp)
	    {

	      /* check for moving cursor */
	      sp = dp->sp; /* can be nil if region playing */
	      if ((sp) && ((!(sp->inuse)) || (sp->playing == 0))) 
		{
		  stop_playing(dp); 
		  return(frames);
		}
	      if ((sp) && 
		  (cursor_change) && 
		  (sp->cursor_follows_play != DONT_FOLLOW) &&
		  (!(read_sample_eof(dp->chn_fd))) &&
		  (dp->chn_fd->cb))
		cursor_moveto(dp->cp, current_location(dp->chn_fd));
	      /* TODO: make this more accurate by taking fragments (soundcard buffers) into account */

	      /* add a buffer's worth from the current source into dp->audio_chan */
	      buf = dac_buffers[dp->audio_chan];
	      if (buf == NULL) return(-1);
	      revin = rev_ins[dp->audio_chan];
	      switch (choose_dac_op(dp, sp))
		{
		case NO_CHANGE:
		  /* simplest case -- no changes at all */
		  for (j = 0; j < frames; j++)
		    buf[j] += next_sample(dp->chn_fd);
		  break;

		case NO_CHANGE_AND_NO_SCALING:
		  for (j = 0; j < frames; j++)
		    buf[j] += local_next_sample_unscaled(dp->chn_fd);
		  break;

		case JUST_AMP:
		  /* AMP_CONTROL(sp, dp) is current UI value, dp->cur_amp is current local value */
		  amp = dp->cur_amp;
		  incr = (AMP_CONTROL(sp, dp) - amp) / (Float)(frames);
		  for (j = 0; j < frames; j++, amp += incr) 
		    buf[j] += MUS_FLOAT_TO_SAMPLE(next_sample_to_float(dp->chn_fd) * amp);
		  dp->cur_amp = amp;
		  break;

		case JUST_SPEED:
		  /* includes amp changes */
		  /* sp->speed_control is current UI value, dp->cur_srate is current local value */
		  dp->never_sped = 0;
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
		  if ((sincr != 0.0) || (sr != 1.0)) dp->never_sped = 0;
		  ind = dp->cur_index;
		  indincr = (sp->contrast_control - ind) / (Float)(frames);
		  rev = dp->cur_rev;
		  revincr = (sp->reverb_control_scale - rev) / (Float)(frames);
		  if ((dp->filtering) && (sp->filter_control_changed))
		    {
		      data = sample_linear_env(sp->filter_control_env, sp->filter_control_order);
		      mus_make_fir_coeffs(sp->filter_control_order, data, dp->a); /* since dp->a is used directly, this might work */
		      FREE(data);
		      sp->filter_control_changed = 0;
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
				      fval = amp * next_sample_to_float(dp->chn_fd);
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

	      /* check for EOF or specified end point */
	      if (dp->end != NO_END_SPECIFIED)
		{
		  dp->end -= frames;
		  if (dp->end < 0) dp->end = 0;
		}
	      if (write_ok == WRITE_TO_DAC)
		{
		  if (dp->end == 0)
		    stop_playing(dp);
		  else
		    {
		      if (read_sample_eof(dp->chn_fd))
			{
			  if (!(dp->expanding))
			    stop_playing(dp);
			  else
			    {
			      dp->expand_ring_frames -= frames;
			      if (dp->expand_ring_frames <= 0)
				stop_playing(dp);
			    }
			}
		    }
		}
	      else /* apply always sets the end point explicitly */
		{
		  if (dp->end == 0)
		    {
		      stop_playing_all_sounds();
		      play_list_members = 0; 
		      max_active_slot = -1;
		    }
		}
	    }
	} /* loop through max_active_slot */
      if (global_rev) 
	{
	  for (i = 0; i < frames; i++)
	    {
	      reverb(global_rev, rev_ins, dac_buffers, i);
	    }
	  if (play_list_members == 0)
	    {
	      dacp->reverb_ring_frames -= frames;
	      if (dacp->reverb_ring_frames <= 0) 
		{
		  free_reverb();
		}
	    }
	}
    }

  /* now parcel these buffers out to the available devices */

  if (XEN_HOOKED(dac_hook))
    {
      if (XEN_FALSE_P(sdobj))
	{
	  sdobj = wrap_sound_data(dacp->channels, dacp->frames, dac_buffers);
	  snd_protect(sdobj);
	}
    g_c_run_progn_hook(dac_hook, 
		       XEN_LIST_1(sdobj),
		       S_dac_hook);
    }

#if (HAVE_OSS || HAVE_ALSA)
  if (write_ok == WRITE_TO_DAC) 
    {
      dev_bufs = dac_buffers;
      for (i = 0; i < dacp->devices; i++)
	if (dev_fd[i] != -1)
	  {
	    mus_file_write_buffer(dacp->out_format,
				  0, frames - 1,
				  dacp->chans_per_device[i],
				  dev_bufs,
				  (char *)(audio_bytes[i]),
				  data_clipped(ss));
	    dev_bufs += dacp->chans_per_device[i];
	  }
      for (i = 0; i < dacp->devices; i++)
	if (dev_fd[i] != -1)
	  {
	    bytes = dacp->chans_per_device[i] * frames * mus_data_format_to_bytes_per_sample(dacp->out_format);
	    mus_audio_write(dev_fd[i], (char *)(audio_bytes[i]), bytes);
	  }
    }
#else
  if (write_ok == WRITE_TO_DAC) 
    {
      mus_file_write_buffer(dacp->out_format, 0, frames - 1, dacp->channels, dac_buffers, (char *)(audio_bytes[0]), data_clipped(ss));
      bytes = dacp->channels * frames * mus_data_format_to_bytes_per_sample(dacp->out_format);
      mus_audio_write(dev_fd[0], (char *)(audio_bytes[0]), bytes);
    }
#endif
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

static char *describe_dac(int error_type)
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

static void dac_error(const char *file, int line, const char *function)
{
  stop_playing_all_sounds();
  snd_error("can't play %s\n  (%s)\n  [%s[%d] %s]",
	    describe_dac(0),
	    (last_print) ? last_print : "reason not known",
	    file, line, function);
}


/* -------------------------------- initialize DAC -------------------------------- */

static void make_dac_buffers(dac_state *dacp)
{
  /* make the per-channel buffers and audio output buffers */
  int bytes, i;
  if ((dac_buffers == NULL) || 
      (dac_buffer_chans < dacp->channels) || 
      (dac_buffer_size < dacp->frames))
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
      dac_buffers = (MUS_SAMPLE_TYPE **)CALLOC(dacp->channels, sizeof(MUS_SAMPLE_TYPE *));
      rev_ins = (Float **)CALLOC(dacp->channels, sizeof(Float *));
      for (i = 0; i < dacp->channels; i++) 
	{
	  dac_buffers[i] = (MUS_SAMPLE_TYPE *)CALLOC(dacp->frames, sizeof(MUS_SAMPLE_TYPE));
	  rev_ins[i] = (Float *)CALLOC(dacp->frames, sizeof(Float));
	}
      dac_buffer_chans = dacp->channels;
      dac_buffer_size = dacp->frames;
      if (r_outs) FREE(r_outs);
      if (r_ins) FREE(r_ins);
      r_outs = (Float *)CALLOC(dacp->channels, sizeof(Float));
      r_ins = (Float *)CALLOC(dacp->channels, sizeof(Float));
    }
  bytes = dacp->channels * dac_buffer_size * mus_data_format_to_bytes_per_sample(dacp->out_format);
  if ((audio_bytes_size < bytes) || 
      (audio_bytes_devices < dacp->devices))
    {
      if (audio_bytes)
	{
	  for (i = 0; i < audio_bytes_devices; i++) FREE(audio_bytes[i]);
	  FREE(audio_bytes);
	}
      audio_bytes = (unsigned char **)CALLOC(dacp->devices, sizeof(unsigned char *));
      for (i = 0; i < dacp->devices; i++) 
	audio_bytes[i] = (unsigned char *)CALLOC(bytes, sizeof(unsigned char));
      audio_bytes_size = bytes;
      audio_bytes_devices = dacp->devices;
    }
}

static void stop_audio_output (dac_state *dacp);

#if (HAVE_ALSA || HAVE_OSS)


/* Controls behavior of device selection logic below. No amount of logic
 * can make everybody happy all the time. The [i]logic below cannot always
 * produce the desired result but deleting it altogether will break the
 * systems that currently rely on it. Not wise without an external api
 * in place designed to select whatever the user _really_ wants. Till 
 * then set this to "1" to always send to the first device. */

int feed_first_device = 0;

#define ALSA_MAX_DEVICES 64
static int alsa_devices[ALSA_MAX_DEVICES];
static int alsa_available_chans[ALSA_MAX_DEVICES];
static int alsa_max_chans[ALSA_MAX_DEVICES];
static int alsa_min_chans[ALSA_MAX_DEVICES];

static int alsa_max_chans_value = 0;
static int alsa_max_chans_dev = 0;
static int audio_devices_scanned = 0;
static int alsa_devices_available = 0;

static void scan_audio_devices(void)
{
  int err, cards, card, devs, dev, d;
  int index = 0;
  float direction;
  float val[ALSA_MAX_DEVICES];
  if (!audio_devices_scanned)
    {
      audio_devices_scanned = 1;
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
	      stop_playing_all_sounds();
	      snd_error("%s[%d] %s: mus_audio_mixer_read", 
			__FILE__, __LINE__, __FUNCTION__);
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
		  stop_playing_all_sounds();
		  snd_error("%s: can't read direction, ignoring device %d", 
			    __FUNCTION__, dev);
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
  alsa_devices_available = index;
}

static int start_audio_output_1 (dac_state *dacp)
{
  int err;
  snd_state *ss;
  int i, d;
  int samples_per_channel = 256;
  float val[ALSA_MAX_DEVICES];
  static int out_dev[ALSA_MAX_DEVICES];
  int alloc_devs = 0;
  int alloc_chans = 0;
  int oss_available_chans = 2;

  ss = dacp->ss;
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
	  if (alsa_max_chans_value >= dacp->channels) 
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
	      if ((alloc_devs != 0) && (alloc_chans < dacp->channels))
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
      dacp->out_format = mus_audio_compatible_format(alsa_devices[out_dev[0]]);
      if (alloc_devs < 2) 
	{
	  /* see if we have a minimum sized frame to fill 
	   * FIXME: could this happen in more than one device? */
	  int c;
	  c = alsa_min_chans[out_dev[0]];
	  if (c > dacp->channels) 
	    {
	      dacp->channels = c;
	    }
	}
      /* see if we have to fold channels */
      if (alloc_chans < dacp->channels) 
	{
	  if (dac_combines_channels(ss)) 
	    snd_warning("folding %d chans into %d ", 
			dacp->channels, alloc_chans);
	  dacp->channels = alloc_chans;
	}
      /* read the number of samples per channel the device wants buffered */
      if ((err = mus_audio_mixer_read(alsa_devices[out_dev[0]], 
				      MUS_AUDIO_SAMPLES_PER_CHANNEL, 
				      2, val)) != -1) 
	{
	  samples_per_channel = (int)(val[0]);
	}
      dacp->frames = samples_per_channel;
      set_dac_size(ss, dacp->frames * mus_data_format_to_bytes_per_sample(dacp->out_format));
      /* open all allocated devices */
      for (d = 0; d < alloc_devs; d++) 
	{
	  int channels;
	  channels = alsa_available_chans[out_dev[d]];
	  if (alloc_chans <= alsa_available_chans[out_dev[d]]) 
	    {
	      if (dacp->channels < alsa_min_chans[out_dev[d]]) 
		{
		  channels = alsa_min_chans[out_dev[d]];
		} 
	      else 
		{
		  channels = dacp->channels;
		}
	    }
	  /* FIXME: assumes devices are same size... */
	  set_dac_print();
	  dev_fd[d] = mus_audio_open_output(alsa_devices[out_dev[d]], 
					    dacp->srate,
					    channels, 
					    dacp->out_format, 
					    dacp->frames * channels * mus_data_format_to_bytes_per_sample(dacp->out_format));
	  unset_dac_print();
      
	  if (dev_fd[d] == -1) 
	    {
	      /* could not open a device, close all others and quit playback */
	      int i;
	      for (i = 0; i < d; i++) 
		{
		  mus_audio_close(alsa_devices[out_dev[i]]);
		}
	      dac_error(__FILE__, __LINE__, __FUNCTION__);
	      dac_running = 0;
	      cleanup_dac_hook();
	      unlock_recording_audio();
	      if (global_rev) free_reverb();
	      max_active_slot = -1;
	      return(FALSE);
	    }
	}
      dacp->devices = alloc_devs;
      /* for now assume all are same number of chans */
      dacp->chans_per_device = (int *)CALLOC(dacp->devices, sizeof(int));
      for (i = 0; i < dacp->devices; i++) 
	dacp->chans_per_device[i] = dacp->channels / dacp->devices;
      make_dac_buffers(dacp);
    } 
  else 
    {
      /* api == OSS_API */
      if (dacp->channels > 2)
	{
	  err = mus_audio_mixer_read(audio_output_device(ss), MUS_AUDIO_CHANNEL, 0, val);
	  if (err != -1) oss_available_chans = (int)(val[0]);
	}
      for (i = 0; i < MAX_DEVICES; i++) dev_fd[i] = -1;
      /* see if we can play 16 bit output */
      dacp->out_format = mus_audio_compatible_format(audio_output_device(ss));
  #ifndef PPC
      /* check for chans > def chans, open 2 cards if available */
      if ((oss_available_chans < dacp->channels) && (dacp->channels == 4))
	{
	  if (mus_audio_systems() > 1)
	    {
	      set_dac_print();
	      dev_fd[0] = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss), 
						dacp->srate, 2, 
						dacp->out_format, 
						dac_size(ss));
	      unset_dac_print();
	      if (dev_fd[0] != MUS_ERROR) 
		dev_fd[1] = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(1) | audio_output_device(ss), 
						  dacp->srate, 2, 
						  dacp->out_format, 
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
						dacp->srate, 2, 
						dacp->out_format, 
						dac_size(ss));
	      unset_dac_print();
	      if (dev_fd[0] != MUS_ERROR) 
		dev_fd[1] = mus_audio_open_output(audio_output_device(ss), 
						  dacp->srate, 2, 
						  dacp->out_format,
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
      if (oss_available_chans < dacp->channels) 
	{
	  if (dac_combines_channels(ss)) 
	    snd_warning("folding %d chans into %d ", 
			dacp->channels, oss_available_chans);
	  dacp->channels = oss_available_chans;
	}
      set_dac_print();
      if (dev_fd[0] == MUS_ERROR)
	dev_fd[0] = mus_audio_open_output(audio_output_device(ss), 
					  dacp->srate, dacp->channels, 
					  dacp->out_format, 
					  dac_size(ss));
      unset_dac_print();
      if (dev_fd[0] == MUS_ERROR)
	{
	  dac_error(__FILE__, __LINE__, __FUNCTION__);
	  stop_audio_output(dacp);
	  return(FALSE);
	}
      dacp->devices = (dev_fd[1] != -1) ? 2 : 1;
      dacp->chans_per_device = (int *)CALLOC(dacp->devices, sizeof(int));
      for (i = 0; i < dacp->devices; i++) 
	dacp->chans_per_device[i] = dacp->channels / dacp->devices;
      make_dac_buffers(dacp);
    }
  return(TRUE);
}
#else /* not ALSA or OSS */

static int start_audio_output_1 (dac_state *dacp)
{
  int err;
  snd_state *ss;
  int i;
  int available_chans = 2;
  float val[32];

  ss = dacp->ss;
  if (dacp->channels > 2)
    {
      err = mus_audio_mixer_read(audio_output_device(ss), MUS_AUDIO_CHANNEL, 0, val);
      if (err != MUS_ERROR) 
	available_chans = (int)(val[0]);
      else 
	{
	  stop_playing_all_sounds();
	  snd_error("can't get audio output chans? (%d) ", audio_output_device(ss));
	  return(FALSE);
	}
    }
  for (i = 0; i < MAX_DEVICES; i++) dev_fd[i] = -1;
  dacp->out_format = MUS_COMPATIBLE_FORMAT;
  if (available_chans < dacp->channels) 
    {
      if (dac_combines_channels(ss)) 
	snd_warning("folding %d chans into %d ", 
		    dacp->channels, available_chans);
      dacp->channels = available_chans;
    }
  
  set_dac_print();
  if (dev_fd[0] == MUS_ERROR)
    dev_fd[0] = mus_audio_open_output(audio_output_device(ss), 
				      dacp->srate, dacp->channels, 
				      dacp->out_format, dac_size(ss));
  unset_dac_print();
  if (dev_fd[0] == MUS_ERROR)
    {
      dac_error(__FILE__, __LINE__, __FUNCTION__);
      stop_audio_output(dacp);
      return(FALSE);
    }
  dacp->devices = 1;
  dacp->chans_per_device = (int *)CALLOC(dacp->devices, sizeof(int));
  for (i = 0; i < dacp->devices; i++) 
    dacp->chans_per_device[i] = available_chans / dacp->devices;
  make_dac_buffers(dacp);
  return(TRUE);
}
#endif

static int start_audio_output (dac_state *dacp)
{
  /* at this point the desired output srate and chans are set in dacp (via start_dac) */
  dac_info *dp;
  int i;
  cursor_time = 0;
  lock_recording_audio();
  if (start_audio_output_1(dacp)) /* number of channels may be less than requested initially */
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
		make_reverb(dp->sp, dacp->channels);

	      if (dp->audio_chan >= dacp->channels)
		{
		  if (dac_combines_channels(dacp->ss))
		    dp->audio_chan %= dacp->channels;
		  else stop_playing(dp);
		}
	    }
	}
      dac_running = 1;
      fill_dac_buffers(dacp, WRITE_TO_DAC);
      lock_apply(dacp->ss, NULL);
      return(TRUE);
    }
  return(FALSE);
}
 
static void stop_audio_output (dac_state *dacp)
{
   int i;
   for (i = 0; i < MAX_DEVICES; i++)
     if (dev_fd[i] != -1) 
       {
	 mus_audio_close(dev_fd[i]);
	 dev_fd[i] = -1;
       }
   dac_running = 0;
   cleanup_dac_hook();
   unlock_recording_audio();
   dac_pausing = 0;
   if (global_rev) free_reverb();
   max_active_slot = -1;
   unlock_apply(dacp->ss, NULL);
}

static BACKGROUND_TYPE dac_in_background(GUI_POINTER ptr)
{
  /* slice 0: try to open audio output devices and get ready to send samples
   *       1: loop sending data until the play_list is empty or some error (C-g) happens
   *       2: try to close all active outputs and remove background procedure
   */
  if (snd_dacp == NULL) return(BACKGROUND_QUIT);
  switch (snd_dacp->slice)
    {
    case 0:
      if (start_audio_output(snd_dacp))
	{
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
      fill_dac_buffers(snd_dacp, WRITE_TO_DAC);
      if ((global_rev == NULL) && (play_list_members == 0)) snd_dacp->slice = 2;
      return(BACKGROUND_CONTINUE);
      break;
     case 2:
       stop_audio_output(snd_dacp);
       free_dac_state();
       return(BACKGROUND_QUIT);
       break;
     }
  return(BACKGROUND_QUIT);
}
 

/* ---------------- support for Apply button (snd-apply.c) ---------------- */

void initialize_apply(snd_info *sp, int chans, int beg, int dur)
{
  int curchan = 0;
  snd_state *ss;
  ss = sp->state;
  stop_playing_all_sounds();
  if (chans <= 0) return;
  max_active_slot = -1;
  play_list_members = 0;

  dac_running = 1; /* this keeps start_dac from actually starting the dac */
  if (snd_dacp) free_dac_state();
  snd_dacp = (dac_state *)CALLOC(1, sizeof(dac_state));
  snd_dacp->slice = 0;
  snd_dacp->ss = ss;
  snd_dacp->srate = SND_SRATE(sp);
  snd_dacp->out_format = MUS_COMPATIBLE_FORMAT;
  if (snd_dacp->srate <= 0) snd_dacp->srate = 44100;
  snd_dacp->channels = chans;
  snd_dacp->frames = 8192;
  snd_dacp->devices = 1;
  snd_dacp->chans_per_device = (int *)CALLOC(1, sizeof(int));
  snd_dacp->chans_per_device[0] = chans;
  snd_dacp->reverb_ring_frames = (int)(snd_dacp->srate * reverb_control_decay(ss));
  make_dac_buffers(snd_dacp);
  switch (ss->apply_choice)
    {
    case APPLY_TO_SOUND: 
      play_sound(sp, beg, beg + dur, IN_BACKGROUND, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), S_apply_controls, 0); 
      break;
    case APPLY_TO_SELECTION: 
      play_selection(IN_BACKGROUND, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), S_apply_controls, 0); 
      break;
    case APPLY_TO_CHANNEL: 
      if (sp->selected_channel != NO_SELECTION)
	curchan = sp->selected_channel;
      play_channel(sp->chans[curchan], beg, beg + dur, IN_BACKGROUND, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), S_apply_controls, 0); 
      break;
    }
}

void finalize_apply(snd_info *sp)
{
  /* if no reverb, these need to be cleaned up */
  stop_playing_all_sounds();
  max_active_slot = -1;
  play_list_members = 0;
  sp->playing = 0;
  dac_running = 0;
  cleanup_dac_hook();
  if (snd_dacp) free_dac_state();
  if (global_rev) free_reverb();
}

int run_apply(int ofd)
{
  int len;
  len = fill_dac_buffers(snd_dacp, WRITE_TO_FILE);
  mus_file_write(ofd, 0, len - 1, snd_dacp->channels, dac_buffers);
  return(len);
}


/* -------------------------------- scheme connection -------------------------------- */

static XEN g_play_1(XEN samp_n, XEN snd_n, XEN chn_n, int background, int syncd, XEN end_n, XEN edpos, const char *caller, int arg_pos) 
{
  /* all chans if chn_n omitted, arbitrary file if snd_n is name */
  snd_info *sp;
  chan_info *cp;
  sync_info *si = NULL;
  char *name = NULL;
  int i, samp = 0;
  int end = NO_END_SPECIFIED;
  int *ends = NULL;
  if (XEN_INTEGER_P(end_n)) end = XEN_TO_C_INT(end_n);
#if USE_NO_GUI
  background = 0;
#endif

  /* if even samp_n is XEN_UNDEFINED, start_dac? */

  if (XEN_STRING_P(samp_n))
    {
      /* filename beg end background syncd ignored */
      samp = XEN_TO_C_INT_OR_ELSE(snd_n, 0);
      if (samp < 0) XEN_ERROR(NO_SUCH_SAMPLE,
			      XEN_LIST_2(C_TO_XEN_STRING(caller),
					 snd_n));
      name = mus_expand_filename(XEN_TO_C_STRING(samp_n));
      if (!(mus_file_probe(name)))
	{
	  FREE(name);
	  return(snd_no_such_file_error(caller, samp_n));
	}
      if (!(MUS_HEADER_TYPE_OK(mus_sound_header_type(name))))
	{
	  FREE(name);
	  mus_misc_error(caller, "can't read header", 
			 XEN_LIST_2(samp_n, 
				    C_TO_XEN_STRING(mus_header_type_name(mus_header_type()))));
	}
      if (!(MUS_DATA_FORMAT_OK(mus_sound_data_format(name))))
	{
	  FREE(name);
	  mus_misc_error(caller, "can't read data", 
			 XEN_LIST_2(samp_n, 
				    C_TO_XEN_STRING(mus_header_original_format_name(mus_sound_original_format(name),
										    mus_sound_header_type(name)))));
	}
      sp = make_sound_readable(get_global_state(), name, FALSE);
      sp->short_filename = filename_without_home_directory(name);
      sp->filename = NULL;
      sp->delete_me = 1;
      if (XEN_INTEGER_P(chn_n)) end = XEN_TO_C_INT(chn_n);
      play_sound(sp, samp, end, background, C_TO_SMALL_XEN_INT(0), caller, arg_pos);
      if (name) FREE(name);
    }
  else
    {
      XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(samp_n), samp_n, XEN_ARG_1, caller, "a number");
      ASSERT_CHANNEL(caller, snd_n, chn_n, 2);
      samp = XEN_TO_C_INT_OR_ELSE(samp_n, 0);
      if (samp < 0) XEN_ERROR(NO_SUCH_SAMPLE,
			      XEN_LIST_2(C_TO_XEN_STRING(caller),
					 samp_n));
      sp = get_sp(snd_n);
      if (sp == NULL) 
	return(snd_no_such_sound_error(caller, snd_n));
      if ((syncd) && (sp->sync != 0))
	{
	  si = snd_sync(sp->state, sp->sync);
	  if (end != NO_END_SPECIFIED)
	    {
	      ends = (int *)CALLOC(si->chans, sizeof(int));
	      for (i = 0; i < si->chans; i++) ends[i] = end;
	    }
	  play_channels(si->cps, si->chans, si->begs, ends, background, edpos, caller, arg_pos, FALSE);
	  si = free_sync_info(si);
	  FREE(ends);
	}
      else
	{
	  if (!(XEN_INTEGER_P(chn_n)))
	    play_sound(sp, samp, end, background, edpos, caller, arg_pos);
	  else 
	    {
	      cp = get_cp(snd_n, chn_n, caller);
	      if (cp) 
		play_channel(cp, samp, end, background, edpos, caller, arg_pos);
	      else snd_no_such_channel_error(caller, snd_n, chn_n);
	    }
	}
    }
  return(XEN_TRUE);
}

#define TO_C_BOOLEAN_OR_F(a) ((XEN_TRUE_P(a) || ((XEN_INTEGER_P(a)) && (XEN_TO_SMALL_C_INT(a) == 1))) ? 1 : 0)

static XEN g_play(XEN samp_n, XEN snd_n, XEN chn_n, XEN syncd, XEN end_n, XEN edpos) 
{
  #define H_play "(" S_play " &optional (start 0) snd chn sync end pos) plays snd or snd's channel chn starting at start. \
'start' can also be a filename: (" S_play " \"oboe.snd\").  If 'sync' is true, all sounds syncd to snd are played. \
if 'end' is not given, it plays to the end of the sound.  If 'pos' is -1 or not given, the current edit position is \
played."

  return(g_play_1(samp_n, snd_n, chn_n, TRUE, TO_C_BOOLEAN_OR_F(syncd), end_n, edpos, S_play, 6));
}

static XEN g_play_channel(XEN beg, XEN dur, XEN snd_n, XEN chn_n, XEN edpos) 
{
  #define H_play_channel "(" S_play_channel " &optional beg dur snd chn pos) plays snd or snd's channel chn starting at beg for dur samps."
  XEN end = XEN_FALSE;
  int len;
  if (XEN_INTEGER_P(dur))
    {
      len = XEN_TO_C_INT(dur);
      if (len <= 0) return(XEN_FALSE);
      end = C_TO_XEN_INT(XEN_TO_C_INT_OR_ELSE(beg, 0) + len);
    }
  return(g_play_1(beg, snd_n, chn_n, TRUE, FALSE, end, edpos, S_play_channel, 5));
}

static XEN g_play_selection(XEN wait, XEN edpos) 
{
  #define H_play_selection "(" S_play_selection " &optional (wait #f) pos) plays the current selection"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(wait), wait, XEN_ARG_1, S_play_selection, "a boolean");
  if (selection_is_active())
    {
      play_selection(!(TO_C_BOOLEAN_OR_F(wait)), edpos, S_play_selection, 2);
      return(XEN_TRUE);
    }
  snd_no_active_selection_error(S_play_selection);
  return(wait);
}

static XEN g_play_and_wait(XEN samp_n, XEN snd_n, XEN chn_n, XEN syncd, XEN end_n, XEN edpos) 
{
  #define H_play_and_wait "(" S_play_and_wait " &optional (start 0) snd chn end pos) plays snd or snd's channel chn starting at start \
and waiting for the play to complete before returning.  'start' can also be a filename: (" S_play_and_wait " \"oboe.snd\")"

  return(g_play_1(samp_n, snd_n, chn_n, FALSE, TO_C_BOOLEAN_OR_F(syncd), end_n, edpos, S_play_and_wait, 6));
}

static XEN g_stop_playing(XEN snd_n)
{
  #define H_stop_playing "(" S_stop_playing " &optional snd) stops play in progress"
  snd_info *sp = NULL;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(snd_n), snd_n, XEN_ARG_1, S_stop_playing, "an integer");
  if (XEN_INTEGER_P(snd_n)) sp = get_sp(snd_n);
  if (sp) 
    stop_playing_sound(sp); 
  else stop_playing_all_sounds();
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
  if ((index < 0) && ((-index) < players_size))
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
  FREE(sp->chans);
  FREE(sp);
}

void clear_players(void)
{
  int i, j;
  snd_info *sp;
  for (i = 0; i < players_size; i++)
    {
      sp = players[i];
      if (sp)
	for (j = 0; j < sp->nchans; j++)
	  if ((sp->chans[j] == NULL) ||
	      (sp->chans[j]->active != 1) ||
	      (sp->chans[j]->sound == NULL))
	    {
	      free_player(sp);
	      break;
	    }
    }
}

/* add-player make-player stop-player start-playing */

static XEN snd_no_such_player_error(const char *caller, XEN index)
{
  XEN_ERROR(NO_SUCH_PLAYER,
	    XEN_LIST_2(C_TO_XEN_STRING(caller),
		       index));
  return(XEN_FALSE);
}

static XEN g_make_player(XEN snd, XEN chn)
{
  #define H_make_player "(" S_make_player " &optional snd chn) prepares snd's channel chn for " S_add_player
  snd_info *true_sp, *new_sp;
  chan_info *cp;
  ASSERT_CHANNEL(S_make_player, snd, chn, 1);
  true_sp = get_sp(snd);
  if (true_sp == NULL) 
    return(snd_no_such_sound_error(S_make_player, snd));
  cp = get_cp(snd, chn, S_make_player);
  if (cp)
    {
      new_sp = make_snd_info(NULL, get_global_state(), "wrapper", true_sp->hdr, new_player_index(), TRUE);
      FREE(new_sp->sgx); /* no built-in GUI */
      new_sp->sgx = NULL;
      new_sp->chans[cp->chan] = cp;
      return(C_TO_XEN_INT(make_player(new_sp, cp)));
    }
  return(snd_no_such_channel_error(S_make_player, snd, chn));  
}

static XEN g_player_home(XEN snd_chn)
{
  #define H_player_home "(" S_player_home " player) returns a list of the sound index and channel number associated with player"
  int index;
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(snd_chn), snd_chn, XEN_ARG_1, S_player_home, "an integer");
  index = -XEN_TO_SMALL_C_INT(snd_chn);
  if ((index > 0) && 
      (index < players_size) && 
      (players[index]) &&
      (players[index]->chans) &&
      (player_chans[index] < players[index]->nchans))
    {
      cp = players[index]->chans[player_chans[index]]; /* trying to get back to the original sound index (not the player index) */
      if ((cp->sound) && (cp->sound->active))
	return(XEN_LIST_2(C_TO_SMALL_XEN_INT(cp->sound->index),
			  C_TO_SMALL_XEN_INT(cp->chan)));
      else return(XEN_LIST_2(NO_SUCH_SOUND,
			     C_TO_SMALL_XEN_INT(cp->chan)));
    }
  return(snd_no_such_player_error(S_player_home, snd_chn));
}

static XEN g_add_player(XEN snd_chn, XEN start, XEN end, XEN edpos)
{
  #define H_add_player "(" S_add_player " &optional player start end pos) starts playing snd's channel chn"
  snd_info *sp = NULL;
  chan_info *cp;
  dac_info *dp = NULL;
  int index;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(snd_chn), snd_chn, XEN_ARG_1, S_add_player, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(start), start, XEN_ARG_2, S_add_player, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(end), end, XEN_ARG_3, S_add_player, "a number");
  index = -XEN_TO_SMALL_C_INT(snd_chn);
  if ((index > 0) && (index < players_size)) sp = players[index];
  if (sp)
    {
      cp = sp->chans[player_chans[index]];
      dp = add_channel_to_play_list(cp, sp,
				    XEN_TO_C_INT_OR_ELSE(start, 0),
				    XEN_TO_C_INT_OR_ELSE(end, NO_END_SPECIFIED),
				    edpos,
				    S_add_player,
				    4);
      if (dp == NULL) return(XEN_FALSE);
    }
  else snd_no_such_player_error(S_add_player, snd_chn);
  return(snd_chn);
}

static XEN g_start_playing(XEN Chans, XEN Srate, XEN In_Background)
{

  /* need some way to distinguish XEN from C vars that represent the same thing -- trying Caps here as an experiment */

  #define H_start_playing "(" S_start_playing " &optional chans srate in-background)"
  int chans, srate;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(Chans), Chans, XEN_ARG_1, S_start_playing, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(Srate), Srate, XEN_ARG_2, S_start_playing, "a number");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(In_Background), In_Background, XEN_ARG_3, S_start_playing, "a boolean");
  chans = XEN_TO_C_INT_OR_ELSE(Chans, 1);
  if (chans <= 0)
    mus_misc_error(S_start_playing, "invalid chans arg", Chans);
  srate = XEN_TO_C_INT_OR_ELSE(Srate, 44100);
  if (srate <= 0)
    mus_misc_error(S_start_playing, "invalid srate arg", Srate);
  start_dac(get_global_state(), srate, chans, XEN_TO_C_BOOLEAN_OR_TRUE(In_Background));
  return(XEN_FALSE);
}

static XEN g_stop_player(XEN snd_chn)
{
  #define H_stop_player "(" S_stop_player " player) stops player"
  int index;
  snd_info *sp = NULL;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(snd_chn), snd_chn, XEN_ARG_1, S_stop_player, "an integer");
  index = -XEN_TO_SMALL_C_INT(snd_chn);
  if ((index > 0) && (index < players_size)) sp = players[index];
  if (sp) 
    stop_playing_sound(sp);
  else snd_no_such_player_error(S_stop_player, snd_chn);
  return(snd_chn);
}

/* also the dac filler needs to run on empty buffers in this case? */

static XEN g_player_p(XEN snd_chn)
{
  #define H_player_p "(" S_player_p " obj) -> is obj an active player"
  int index;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(snd_chn), snd_chn, XEN_ARG_1, S_player_p, "an integer");
  index = -XEN_TO_SMALL_C_INT(snd_chn);
  return(C_TO_XEN_BOOLEAN((index > 0) && 
			  (index < players_size) && 
			  (players[index])));
}


#ifdef XEN_ARGIFY_1
XEN_ARGIFY_6(g_play_w, g_play)
XEN_ARGIFY_5(g_play_channel_w, g_play_channel)
XEN_ARGIFY_2(g_play_selection_w, g_play_selection)
XEN_ARGIFY_6(g_play_and_wait_w, g_play_and_wait)
XEN_ARGIFY_1(g_stop_playing_w, g_stop_playing)
XEN_ARGIFY_2(g_make_player_w, g_make_player)
XEN_ARGIFY_4(g_add_player_w, g_add_player)
XEN_NARGIFY_1(g_player_home_w, g_player_home)
XEN_ARGIFY_3(g_start_playing_w, g_start_playing)
XEN_NARGIFY_1(g_stop_player_w, g_stop_player)
XEN_NARGIFY_1(g_player_p_w, g_player_p)
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
#define g_player_p_w g_player_p
#endif

void g_init_dac(void)
{
  XEN_DEFINE_PROCEDURE(S_play,           g_play_w, 0, 6, 0,           H_play);
  XEN_DEFINE_PROCEDURE(S_play_channel,   g_play_channel_w, 0, 5, 0,   H_play_channel);
  XEN_DEFINE_PROCEDURE(S_play_selection, g_play_selection_w, 0, 2, 0, H_play_selection);
  XEN_DEFINE_PROCEDURE(S_play_and_wait,  g_play_and_wait_w, 0, 6, 0,  H_play_and_wait);
  XEN_DEFINE_PROCEDURE(S_stop_playing,   g_stop_playing_w, 0, 1, 0,   H_stop_playing);

  XEN_DEFINE_PROCEDURE(S_make_player,    g_make_player_w, 0, 2, 0,    H_make_player);
  XEN_DEFINE_PROCEDURE(S_add_player,     g_add_player_w, 1, 3, 0,     H_add_player);
  XEN_DEFINE_PROCEDURE(S_player_home,    g_player_home_w, 1, 0, 0,    H_player_home);
  XEN_DEFINE_PROCEDURE(S_start_playing,  g_start_playing_w, 0, 3, 0,  H_start_playing);
  XEN_DEFINE_PROCEDURE(S_stop_player,    g_stop_player_w, 1, 0, 0,    H_stop_player);
  XEN_DEFINE_PROCEDURE(S_player_p,       g_player_p_w, 1, 0, 0,       H_player_p);

  #define H_stop_playing_hook S_stop_playing_hook " (snd) is called when a sound finishes playing."
  #define H_stop_playing_channel_hook S_stop_playing_channel_hook " (snd chn) is called when a channel finishes playing."
  #define H_stop_playing_region_hook S_stop_playing_region_hook " (reg) is called when a region finishes playing."
  #define H_play_hook S_play_hook " (samps) is called each time a buffer is sent to the DAC."
  #define H_start_playing_hook S_start_playing_hook " (snd) is called when a play request is triggered. \
If it returns #t, the sound is not played."
  #define H_dac_hook S_dac_hook " (sdobj) called just before data is sent to DAC passing data as sound-data object"
  #define H_stop_dac_hook S_stop_dac_hook " () called upon mus_audio_close (when DAC is turned off)"
  #define H_stop_playing_selection_hook S_stop_playing_selection_hook " () called when the selection stops playing"

  XEN_DEFINE_HOOK(stop_playing_hook, S_stop_playing_hook, 1, H_stop_playing_hook);                         /* arg = sound */
  XEN_DEFINE_HOOK(stop_playing_channel_hook, S_stop_playing_channel_hook, 2, H_stop_playing_channel_hook); /* args = sound channel */
  XEN_DEFINE_HOOK(stop_playing_region_hook, S_stop_playing_region_hook, 1, H_stop_playing_region_hook);    /* arg = region number */
  XEN_DEFINE_HOOK(start_playing_hook, S_start_playing_hook, 1, H_start_playing_hook);                      /* arg = sound */
  XEN_DEFINE_HOOK(play_hook, S_play_hook, 1, H_play_hook);                                                 /* args = size */
  XEN_DEFINE_HOOK(dac_hook, S_dac_hook, 1, H_dac_hook);                                                    /* args = data as sound_data obj */
  XEN_DEFINE_HOOK(stop_dac_hook, S_stop_dac_hook, 0, H_stop_dac_hook);                                     /* no args */
  XEN_DEFINE_HOOK(stop_playing_selection_hook, S_stop_playing_selection_hook, 0, H_stop_playing_selection_hook); /* no args */

#if DEBUGGING
  XEN_DEFINE_PROCEDURE("disable-play", g_disable_play, 0, 0, 0, NULL);
#endif
}
