/* TODO  make revlen follow slider in "real-time":
 * TODO    set up line_size in mus_make_comb to 5.0*srate/25641, then
 * TODO    then as running, at each block reset to initial - new scaled
 * TODO    (negative pm = longer delay)
 * TODO  play with expand is cutoff too soon (and reverb?)
 */

/* this was sound-oriented; changed to be channel-oriented 31-Aug-00 */
/*
 * each channel currently being played has an associated dac_info struct
 *   all active dac_info structs are held in a play_list
 *   channels can come and go as a play is in progress
 */

#include "snd.h"


static int prime (int num)
{
  int lim,i;
  if (num == 2) return(1);
  if ((num%2) == 1)
    {
      lim = (int)(sqrt(num));
      for (i=3;i<lim;i+=2)
	{
	  if ((num%i) == 0) return(0);
	}
      return(1);
    }
  return(0);
}

static int get_prime(int num)
{
  int i;
  if ((num%2) == 1)
    i=num;
  else i=num+1;
  while (!(prime(i))) {i+=2;}
  return(i);
}

Float list_interp(Float x, Float *e, int pts)
{
  if (pts == 0) return(0.0);
  if ((x <= e[0]) || (pts == 1)) return(e[1]);
  if (e[2] > x)
    {
      if (e[1] == e[3]) return(e[1]);
      return(e[1]+(x-e[0])*(e[3]-e[1])/(e[2]-e[0]));
    }
  return(list_interp(x,(Float *)(e+2),pts-1));
}

static Float *sample_linear_env(env *e, int order)
{
  Float *data;
  Float last_x,step,x;
  int i,j;
  data = (Float *)CALLOC(order,sizeof(Float));
  last_x = e->data[(e->pts-1)*2];
  step = 2*last_x/((Float)order-1);
  for (i=0,x=0.0;i<order/2;i++,x+=step) data[i] = list_interp(x,e->data,e->pts);
  for (j=order/2-1,i=order/2;(i<order) && (j>=0);i++,j--) data[i] = data[j];
  return(data);
}



static Float reverb_factor = DEFAULT_REVERB_FEEDBACK;
static Float reverb_length = DEFAULT_REVERB_LENGTH;
static Float lp_coeff = DEFAULT_REVERB_LOWPASS;
static int revchans = 0;
static int revdecay = 0;


/* -------------------------------- user-defined control-panel functions -------------------------------- */
#if HAVE_GUILE

/* user hooks into reverb */
static SCM g_make_reverb = SCM_UNDEFINED,g_reverb = SCM_UNDEFINED, g_free_reverb = SCM_UNDEFINED;
static int use_g_reverb = 0;
static SCM g_reverb_funcs(void) {return(SCM_LIST3(g_reverb,g_make_reverb,g_free_reverb));}
static SCM g_set_reverb_funcs(SCM rev, SCM make_rev, SCM free_rev)
{
  #define H_reverb_funcs "(" S_reverb_funcs ") -> list of the 3 reverb funcs (reverb make-reverb free-reverb)"
  #define H_set_reverb_funcs "(" "set-" S_reverb_funcs " reverb make-reverb free-reverb) sets the current reverb functions"
  if (gh_procedure_p(g_reverb)) snd_unprotect(g_reverb);
  if (gh_procedure_p(g_make_reverb)) snd_unprotect(g_make_reverb);
  if (gh_procedure_p(g_free_reverb)) snd_unprotect(g_free_reverb);
  if ((procedure_ok(rev,3,0,"set-" S_reverb_funcs,"reverb",1)) &&
      (procedure_ok(make_rev,3,0,"set-" S_reverb_funcs,"make-reverb",2)) &&
      (procedure_ok(free_rev,1,0,"set-" S_reverb_funcs,"free-reverb",3)))
    {
      g_reverb = rev;
      g_make_reverb = make_rev;
      g_free_reverb = free_rev;
      snd_protect(g_reverb);
      snd_protect(g_make_reverb);
      snd_protect(g_free_reverb);
      use_g_reverb = 1;
    }
  else use_g_reverb = 0;
  return(rev);
}

/* user hook into contrast */
static SCM g_contrast = SCM_UNDEFINED;
static int use_g_contrast = 0;
static SCM g_contrast_func(void) {return(g_contrast);}
static SCM g_set_contrast_func(SCM func)
{
  #define H_contrast_func "(" S_contrast_func ") -> current contrast function"
  #define H_set_contrast_func "(" "set-" S_contrast_func " func) sets the current contrast function"
  if (gh_procedure_p(g_contrast)) snd_unprotect(g_contrast);
  if (procedure_ok(func,2,0,"set-" S_contrast_func,"contrast",1))
    {
      g_contrast = func;
      snd_protect(g_contrast);
      use_g_contrast = 1;
    }
  else use_g_contrast = 0;
  return(func);
}

/* user hooks into expand */
static SCM g_expand = SCM_UNDEFINED, g_make_expand = SCM_UNDEFINED, g_free_expand = SCM_UNDEFINED;
static int use_g_expand = 0;
static SCM g_expand_funcs(void) {return(SCM_LIST3(g_expand,g_make_expand,g_free_expand));}
static SCM g_set_expand_funcs(SCM expnd, SCM make_expnd, SCM free_expnd)
{
  #define H_expand_funcs "(" S_expand_funcs ") -> list of the 3 expand funcs (expand make-expand free-expand)"
  #define H_set_expand_funcs "(" "set-" S_expand_funcs " expand make-expand free-expand) sets the current expand functions"
  if (gh_procedure_p(g_expand)) snd_unprotect(g_expand);
  if (gh_procedure_p(g_make_expand)) snd_unprotect(g_make_expand);
  if (gh_procedure_p(g_free_expand)) snd_unprotect(g_free_expand);
  if ((procedure_ok(expnd,3,0,"set-" S_expand_funcs,"expand",1)) &&
      (procedure_ok(make_expnd,3,0,"set-" S_expand_funcs,"make-expand",2)) &&
      (procedure_ok(free_expnd,1,0,"set-" S_expand_funcs,"free-expand",3)))
    {
      g_expand = expnd;
      g_make_expand = make_expnd;
      g_free_expand = free_expnd;
      snd_protect(g_expand);
      snd_protect(g_make_expand);
      snd_protect(g_free_expand);
      use_g_expand = 1;
    }
  else use_g_expand = 0;
  return(expnd);
}

#if HAVE_HOOKS
  static void call_stop_playing_hook(snd_info *sp);
  static void call_stop_playing_region_hook(int n);
  static void call_stop_playing_channel_hook(snd_info *sp, chan_info *cp);
  static int call_start_playing_hook(snd_info *sp);
#else
  static void call_stop_playing_hook(snd_info *sp) {}
  static void call_stop_playing_region_hook(int n) {}
  static void call_stop_playing_channel_hook(snd_info *sp, chan_info *cp) {}
  static int call_start_playing_hook(snd_info *sp) {return(0);}
#endif

#endif


/* -------------------------------- per-channel control-panel state -------------------------------- */

typedef struct {
  mus_any *gen;
  struct dac__info *dp;
  int speeding;
  Float sr;
} spd_info;

typedef struct {
  int num_combs;
  mus_any **combs;
  int num_allpasses;
  mus_any **allpasses;
  mus_any *onep;
} rev_info;

static void *global_reverb = NULL;
static int global_reverbing = 0;

typedef struct dac__info {
  Float cur_index;
  Float cur_amp;
  Float cur_srate;
  Float cur_exp;
  Float cur_rev;       /* rev scaler -- len is set at initialization */
  Float contrast_amp;
  int expanding,reverbing,filtering; /* these need lots of preparation, so they're noticed only at the start */
  int audio_chan;      /* where channel's output is going (wrap-around if not enough audio output channels) */
  int slot;
  Float lst,nxt,x;     /* used if linear interp for src */
  Float *a;            /* filter coeffs */
  snd_fd *chn_fd;      /* sample reader */
  spd_info *spd;
  mus_any *flt;
  int region;          /* to reset region-browser play button upon completion */
  src_state *src;
  snd_info *sp;        /* needed to see button callback changes etc */
  chan_info *cp;
  snd_state *ss;
  int end,no_scalers;
#if DEBUGGING
  char *desc;
#endif
} dac_info;

#if DEBUGGING
static char dac_info_buf[512];
static char *describe_dac_info(dac_info *dp)
{
  if (dp)
    sprintf(dac_info_buf,"dac_info (%p) %s: slot: %d, sp: %p (%s), reg: %d, cp: %p %d\n",
	    dp,dp->desc,dp->slot,
	    dp->sp,(dp->sp) ? dp->sp->shortname : "none",
	    dp->region,
	    dp->cp, (dp->cp) ? dp->cp->chan : -1);
  else return("null!");
  return(dac_info_buf);
}
#endif


/* -------- filter -------- */
static mus_any *make_flt(dac_info *dp, int order, Float *env)
{
  if (order<=0) return(NULL);
  dp->a = (Float *)CALLOC(order,sizeof(Float));
  if (env) mus_make_fir_coeffs(order,env,dp->a);
  return(mus_make_fir_filter(order,dp->a,NULL));
}


/* -------- sample-rate conversion -------- */
static Float speed(dac_info *dp, Float sr)
{
  int move,i;
  Float result = 0.0;
  if ((use_sinc_interp((dp->ss))) && (dp->src))
    result = run_src(dp->src,sr);
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
	      for (i=0;i<move;i++)
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
	      for (i=0;i<move;i++)
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
    return(speed(dp,spd->sr));
  else return(next_sample_to_float(dp->chn_fd));
}

static int max_expand_len(snd_info *sp)
{
  if (sp->expand_length > .5)
    return(0);
  return((int)(SND_SRATE(sp) * .5));
}

static void *make_expand(snd_info *sp, Float sampling_rate, Float initial_ex, dac_info *dp)
{
  spd_info *spd;
#if HAVE_GUILE
  if (use_g_expand)
    return((void *)g_call3(g_make_expand,gh_int2scm(sp->index),gh_double2scm(sampling_rate),gh_double2scm(initial_ex)));
#endif
  spd = (spd_info *)CALLOC(1,sizeof(spd_info));
  spd->gen = mus_make_granulate(&expand_input_as_needed,
				initial_ex,sp->expand_length,
				.6,sp->expand_hop,sp->expand_ramp,.1,
				max_expand_len(sp),(void *)spd);
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
#if HAVE_GUILE
      if (use_g_expand)
	g_call1(g_free_expand,(SCM)ur_spd);
      else
#endif
	{
	  mus_free(spd->gen);
	  FREE(spd);
	}
    }
}

static Float expand(dac_info *dp, Float sr, Float ex)
{
  /* from mixer.sai, used in "Leviathan", 1986 */
  int speeding;
  snd_info *sp;
  spd_info *spd;
  Float fval;
  sp = dp->sp;
  speeding = ((sp->play_direction != 1) || (sp->srate != 1.0) || (dp->cur_srate != 1.0));
#if HAVE_GUILE
  if (use_g_expand)
    {
      /* if speeding, pick up speed vals first, else read direct */
      if (speeding) 
	fval = speed(dp,sr);
      else fval = next_sample_to_float(dp->chn_fd);
      return(gh_scm2double(g_call3(g_expand,(SCM)(dp->spd),gh_double2scm(fval),gh_double2scm(ex))));
    }
  else
#endif
    {
      spd = dp->spd;
      spd->speeding = speeding;
      spd->sr = sr;
      return(mus_granulate(spd->gen,&expand_input_as_needed));
    }
}


/* -------- reverb -------- */
#define BASE_DLY_LEN 14
static int base_dly_len[BASE_DLY_LEN] = {1433, 1601, 1867, 2053, 2251, 2399, 347, 113, 37, 59, 43, 37, 29, 19};
static int dly_len[BASE_DLY_LEN];
static Float comb_factors[6] = {0.822,0.802,0.773,0.753,0.753,0.733};

static void *make_reverb(snd_info *sp, Float sampling_rate, int chans)
{ 
  /* Mike McNabb's nrev from Mus10 days (ca. 1978) */
  Float srscale;
  int i,j,len;
  rev_info *r;
  revchans = chans;
  revdecay = 0;
  global_reverbing = 1;

#if HAVE_GUILE
  if (use_g_reverb)
    return((void *)g_call3(g_make_reverb,gh_double2scm(reverb_length),gh_double2scm(sampling_rate),gh_int2scm(chans)));
#endif

  reverb_factor = sp->revfb;
  lp_coeff = sp->revlp;
  srscale = reverb_length*sampling_rate/25641.0;
  for (i=0;i<BASE_DLY_LEN;i++) 
    dly_len[i] = get_prime((int)(srscale*base_dly_len[i]));
  r=(rev_info *)CALLOC(1,sizeof(rev_info));
  r->num_combs = 6;
  r->combs = (mus_any **)CALLOC(r->num_combs,sizeof(mus_any *));
  r->num_allpasses = 4+chans;
  r->allpasses = (mus_any **)CALLOC(r->num_allpasses,sizeof(mus_any *));
  for (i=0;i<r->num_combs;i++) 
    r->combs[i] = mus_make_comb(comb_factors[i]*reverb_factor,dly_len[i],NULL,dly_len[i]);
  r->onep = mus_make_one_pole(lp_coeff,lp_coeff-1.0);
  for (i=0,j=r->num_combs;i<4;i++,j++) 
    r->allpasses[i] = mus_make_all_pass(-0.700,0.700,dly_len[j],NULL,dly_len[j]);
  for (i=0,j=10;i<chans;i++)
    {
      if (j<BASE_DLY_LEN) 
	len = dly_len[j]; 
      else len = get_prime((int)(40 + mus_random(20.0)));
      r->allpasses[i+4] = mus_make_all_pass(-0.700,0.700,len,NULL,len);
    }
  return((void *)r);
}

static void free_reverb(void *ur)
{
  int i;
  rev_info *r = (rev_info *)ur;
  global_reverbing = 0;
#if HAVE_GUILE
  if (use_g_reverb)
    g_call1(g_free_reverb,(SCM)ur);
  else
#endif
  if (r)
    {
      for (i=0;i<r->num_combs;i++) if (r->combs[i]) mus_free(r->combs[i]);
      FREE(r->combs);
      mus_free(r->onep);
      for (i=0;i<r->num_allpasses;i++) if (r->allpasses[i]) mus_free(r->allpasses[i]);
      FREE(r->allpasses);
      FREE(r);
    }
}

static void reverb(void *ur, Float rin, MUS_SAMPLE_TYPE **outs, int ind, int chans)
{
  rev_info *r = (rev_info *)ur;
  Float rout;
  int i;
#if HAVE_GUILE
  SCM outputs;
  if (use_g_reverb)
    {
      outputs = g_call3(g_reverb,(SCM)ur,gh_double2scm(rin),gh_int2scm(chans));
      for (i=0;i<chans;i++) 
	outs[i][ind] += MUS_FLOAT_TO_SAMPLE(((Float)(gh_scm2double(gh_vector_ref(outputs,gh_int2scm(i))))));
    }
  else
#endif
    {
      rout = mus_all_pass(r->allpasses[3],
	       mus_one_pole(r->onep,
		 mus_all_pass(r->allpasses[2],
		   mus_all_pass(r->allpasses[1],
		     mus_all_pass(r->allpasses[0],
				  mus_comb(r->combs[0],rin,0.0) + 
				  mus_comb(r->combs[1],rin,0.0) + 
				  mus_comb(r->combs[2],rin,0.0) + 
				  mus_comb(r->combs[3],rin,0.0) + 
				  mus_comb(r->combs[4],rin,0.0) + 
				  mus_comb(r->combs[5],rin,0.0),
				  0.0),
				0.0),
			      0.0)),
			  0.0);
      for (i=0;i<chans;i++)
        outs[i][ind] += MUS_FLOAT_TO_SAMPLE(mus_all_pass(r->allpasses[i+4],rout,0.0));
    }
}

/* -------- contrast-enhancement -------- */
static Float contrast (dac_info *dp, Float amp, Float index, Float inval)
{
#if HAVE_GUILE
  if (use_g_contrast)
    return(amp * gh_scm2double(g_call2(g_contrast,
				       gh_double2scm(dp->contrast_amp * inval),
				       gh_double2scm(index))));
  else
#endif
    return(amp * mus_contrast_enhancement(dp->contrast_amp * inval,index));
}

static dac_info *make_dac_info(chan_info *cp, snd_info *sp, snd_fd *fd)
{
  dac_info *dp;
  Float *data = NULL;
  dp = (dac_info *)CALLOC(1,sizeof(dac_info));
  dp->region = -1;
  dp->a = NULL;
  dp->no_scalers = no_ed_scalers(cp);
  dp->audio_chan = cp->chan;
  if (sp)
    {
      dp->expanding = sp->expanding;
      dp->filtering = ((sp->filtering) && (sp->filter_order > 0));
      dp->reverbing = sp->reverbing;
      dp->contrast_amp = sp->contrast_amp;
      if (use_sinc_interp(sp->state))
	dp->src = make_src(sp->state,0.0,fd);
      if (dp->expanding) 
	dp->spd = (spd_info *)make_expand(sp,(Float)SND_SRATE(sp),sp->expand,dp);
      if (dp->filtering)
	{
	  sp->filter_changed = 0;
	  if (!(sp->filter_env)) 
	    dp->filtering = 0;
	  else
	    {
	      data = sample_linear_env(sp->filter_env,sp->filter_order);
	      dp->flt = make_flt(dp,sp->filter_order,data);
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
#if DEBUGGING
  if (dp->desc) FREE(dp->desc);
#endif
  FREE(dp);
}


static int dac_max_sounds = 0;
static dac_info **play_list = NULL;
#define INITIAL_MAX_SOUNDS 32
static int play_list_members = 0;
static int max_active_slot = -1;


/* -------------------------------- special "hidden" control panel variables -------------------------------- */

enum {DAC_EXPAND,DAC_EXPAND_RAMP,DAC_EXPAND_LENGTH,DAC_EXPAND_HOP,DAC_EXPAND_SCALER,DAC_CONTRAST_AMP,DAC_REVERB_FEEDBACK,DAC_REVERB_LOWPASS};

static void dac_set_field(snd_info *sp, Float newval, int field)
{
  /* if sp == NULL, sets globally */
  int i,j,val;
  dac_info *dp;
  rev_info *r;
  if (play_list)
    {
      if (field == DAC_REVERB_LOWPASS)
	{
	  if ((global_reverbing) && (global_reverb))
	    {
	      r = (rev_info *)global_reverb;
	      lp_coeff = newval;
	      mus_set_a0(r->onep,lp_coeff);
	      mus_set_b1(r->onep,1.0 - lp_coeff);
	    }
	}
      else
	{
	  if (field == DAC_REVERB_FEEDBACK)
	    {
	      if ((global_reverbing) && (global_reverb))
		{
		  r = (rev_info *)global_reverb;
		  for (j=0;j<6;j++)
		    mus_set_feedback(r->combs[j],comb_factors[j]*newval);
		}
	    }
	  else
	    {
	      for (i=0;i<=max_active_slot;i++)
		{
		  dp = play_list[i];
		  if ((dp) && ((sp == NULL) || (sp == dp->sp)))
		    {
		      switch (field)
			{
			case DAC_EXPAND: 
			  if (dp->spd)
			    mus_set_increment((dp->spd)->gen,newval); 
			  break;
			case DAC_EXPAND_LENGTH: /* segment length */
			  if (dp->spd)
			    {
			      val = (int)(SND_SRATE(sp) * newval);
			      mus_set_length((dp->spd)->gen,val);
			      mus_set_ramp((dp->spd)->gen,(int)(val * sp->expand_ramp));
			    }
			  break;
			case DAC_EXPAND_RAMP: 
			  if (dp->spd)
			    {
			      val = (int)(newval * sp->expand_length * SND_SRATE(sp));
			      mus_set_ramp((dp->spd)->gen,val); 
			    }
			  break;
			case DAC_EXPAND_HOP: /* output hop */
			  if (dp->spd)
			    {
			      val = (int)(SND_SRATE(sp) * newval);
			      mus_set_hop((dp->spd)->gen,val); 
			      mus_set_increment((dp->spd)->gen,sp->expand);
			    }
			  break;
			case DAC_EXPAND_SCALER:
			  if (dp->spd)
			    mus_set_scaler((dp->spd)->gen,newval); 
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

void dac_set_expand(snd_info *sp, Float newval) {dac_set_field(sp,newval,DAC_EXPAND);}
void dac_set_expand_length(snd_info *sp, Float newval) {dac_set_field(sp,newval,DAC_EXPAND_LENGTH);}
void dac_set_expand_ramp(snd_info *sp, Float newval) {dac_set_field(sp,newval,DAC_EXPAND_RAMP);}
void dac_set_expand_hop(snd_info *sp, Float newval) {dac_set_field(sp,newval,DAC_EXPAND_HOP);}
void dac_set_expand_scaler(snd_info *sp, Float newval) {dac_set_field(sp,newval,DAC_EXPAND_SCALER);} /* not currently accessible */
void dac_set_contrast_amp(snd_info *sp, Float newval) {dac_set_field(sp,newval,DAC_CONTRAST_AMP);}
void dac_set_reverb_feedback(snd_info *sp, Float newval) {dac_set_field(sp,newval,DAC_REVERB_FEEDBACK);}
void dac_set_reverb_lowpass(snd_info *sp, Float newval) {dac_set_field(sp,newval,DAC_REVERB_LOWPASS);}



/* -------------------------------- stop playing (remove from play-list) -------------------------------- */

static int dac_running = 0;

#define MAX_DEVICES 8
static int dev_fd[MAX_DEVICES];

void cleanup_dac(void)
{
  int i;
  if (dac_running) 
    {
      for (i=0;i<MAX_DEVICES;i++) 
	{
	  if (dev_fd[i] != -1) 
	    {
	      mus_audio_close(dev_fd[i]);
	      dev_fd[i] = -1;
	    }
	}
    }
  for (i=0;i<MAX_DEVICES;i++) dev_fd[i] = -1;
  dac_running = 0;
}

static void reflect_play_stop (snd_info *sp) 
{
  if (w_snd_play(sp)) set_toggle_button(w_snd_play(sp),FALSE,FALSE,sp);
  set_file_browser_play_button(sp->shortname,0);
}

#if HAVE_GUILE
  static void free_player(snd_info *sp);
#endif

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
      if (sp->cursor_follows_play != DONT_FOLLOW)
	handle_cursor(cp,cursor_moveto(cp,cp->original_cursor));
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
#if HAVE_GUILE
  if (dp->region >= 0)
    call_stop_playing_region_hook(dp->region); 
  else
    {
      if (sp_stopping)
	call_stop_playing_hook(sp);
      call_stop_playing_channel_hook(sp,cp);
      if (sp->index < 0) {free_player(sp); sp = NULL;}
    }
#endif
  free_dac_info(dp);
  if ((sp) && (sp_stopping) && (sp->delete_me)) 
    completely_free_snd_info(sp); /* dummy snd_info struct for (play "filename") in snd-scm.c */
}

static void stop_playing(dac_info *dp) {stop_playing_with_toggle(dp,TRUE);}

static void stop_playing_sound_with_toggle(snd_info *sp, int toggle)
{
  /* this needs to scan all current play_list members and remove any that are referring
   * to sp, even indirectly (as through the current selection)
   */
  int i;
  if ((sp) && (play_list))
    {
      for (i=0;i<=max_active_slot;i++)
	{
	  if ((play_list[i]) && (sp == (play_list[i]->sp)))
	    {
	      stop_playing_with_toggle(play_list[i],toggle);
	      play_list[i] = NULL;
	    }
	}
    }
}

void stop_playing_sound(snd_info *sp) {stop_playing_sound_with_toggle(sp,TRUE);}
void stop_playing_sound_no_toggle(snd_info *sp) {stop_playing_sound_with_toggle(sp,FALSE);}

void stop_playing_all_sounds (void)
{
  int i;
  if (play_list)
    {
      for (i=0;i<=max_active_slot;i++)
	{
	  stop_playing(play_list[i]);
	  play_list[i] = NULL;
	}
    }
}

void stop_playing_region(int n)
{
  int i;
  if (play_list)
    {
      for (i=0;i<=max_active_slot;i++)
	{
	  if (play_list[i])
	    {
	      if (play_list[i]->region == n)
		{
		  stop_playing(play_list[i]);
		  play_list[i] = NULL;
		}
	    }
	}
    }
}

/* -------------------------------- play (add to play-list) -------------------------------- */

static int find_slot_to_play(void)
{
  int i,old_size;
  if (play_list == NULL)
    {
      dac_max_sounds = INITIAL_MAX_SOUNDS;
      play_list = (dac_info **)CALLOC(dac_max_sounds,sizeof(dac_info *));
    }
  for (i=0;i<dac_max_sounds;i++) 
    {
      if (!play_list[i]) return(i);
    }
  old_size = dac_max_sounds;
  dac_max_sounds += INITIAL_MAX_SOUNDS;
  play_list = (dac_info **)REALLOC(play_list,dac_max_sounds*sizeof(dac_info *));
  for (i=old_size;i<dac_max_sounds;i++) play_list[i] = NULL;
  return(old_size);
}

static dac_info *init_dp(int slot, chan_info *cp, snd_info *sp, snd_fd *fd, int beg, int end)
{
  dac_info *dp;
  dp = make_dac_info(cp,sp,fd);
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
      dp->cur_srate = sp->srate*sp->play_direction;
      dp->cur_amp = sp->amp;
      dp->cur_index = sp->contrast;
      dp->cur_exp = sp->expand;
      dp->cur_rev = sp->revscl;
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

static void free_dac_state(dac_state *dacp)
{
  if (dacp)
    {
      if (dacp->chans_per_device) FREE(dacp->chans_per_device);
      FREE(dacp);
    }
}

static char *describe_dac(dac_state *dacp, int error_type)
{
  /* TODO: pass back the relevant dac state given the error indication */
  int players=0,i;
  dac_info *ptr=NULL;
  for (i=0;i<dac_max_sounds;i++) 
    if (play_list[i]) {ptr = play_list[i]; players++;}
  if ((players == 1) && (ptr->sp))
    return(ptr->sp->shortname);
  return("");
}


static BACKGROUND_TYPE dac_in_background(GUI_POINTER ptr);

static void start_dac(snd_state *ss, int srate, int channels, int background)
{
  dac_state *dacp = NULL;
  dac_info *dp;
  int i;
  /* look for channel folding cases etc */
  /* channels = how many output audio chans we have; dac_folding sets whether to wrap or muffle chans outside this limit */
  for (i=0;i<=max_active_slot;i++)
    {
      dp = play_list[i];
      if (dp)
	{
	  if ((dp->reverbing) && (dp->sp) && (global_reverb == NULL))
	    {
	      reverb_length = dp->sp->revlen;
	      global_reverb = (void *)make_reverb(dp->sp,(Float)SND_SRATE(dp->sp),channels);
	    }
          if ((dac_running) && (dp->audio_chan >= channels)) /* if dac_running, the number of channels has already been set and won't change */
	    {
	      if (dac_folding(ss))
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
      dacp = (dac_state *)CALLOC(1,sizeof(dac_state));
      dacp->slice = 0;
      dacp->ss = ss;
      dacp->srate = srate;
      dacp->out_format = MUS_COMPATIBLE_FORMAT;
      if (dacp->srate <= 0) dacp->srate = 44100;
      dacp->channels = channels;
      dacp->frames = 256; /* just a first guess */
      dacp->devices = 1;  /* just a first guess */
      dacp->reverb_ring_frames = (int)(srate * reverb_decay(ss));
      if (background == IN_BACKGROUND) 
	BACKGROUND_ADD(ss,dac_in_background,(GUI_POINTER)dacp);
      else
	{
	  /* here we want to play as an atomic (not background) action */
	  while (dac_in_background((GUI_POINTER)dacp) == BACKGROUND_CONTINUE)
	    {
	      check_for_event(ss); /* need to be able to C-g out of this */
	      /* if ((sp) && (!(sp->inuse))) break; */
	    }
	}
    }
}


static dac_info *add_channel_to_play_list(chan_info *cp, snd_info *sp, int start, int end)
{
  /* if not sp, control panel is ignored */
  int slot,beg=0,direction=READ_FORWARD;
  slot = find_slot_to_play();
  if (slot == -1) return(NULL);
  play_list_members++;
  if (sp)
    {
      sp->playing++;
      if (sp->cursor_follows_play != DONT_FOLLOW)
	{
	  cp->original_cursor = cp->cursor;
	  handle_cursor(cp,cursor_moveto(cp,start));
	}
      if (sp->play_direction == 1) 
	{
	  direction = READ_FORWARD; 
	  beg = start;
	}
      else 
	{
	  direction = READ_BACKWARD;
	  if (start == 0) beg = current_ed_samples(cp)-1; else beg = start;
	}
    }
  return(init_dp(slot,cp,sp,init_sample_read(beg,cp,direction),start,end));
}

static dac_info *add_region_channel_to_play_list(int region, int chan, int beg, int end)
{
  int slot;
  snd_fd *fd;
  slot = find_slot_to_play();
  if (slot == -1) return(NULL);
  play_list_members++;
  fd = init_region_read(get_global_state(),beg,region,chan,READ_FORWARD);
  return(init_dp(slot,fd->cp,NULL,fd,beg,end));
}

void play_region(snd_state *ss, int region, int background)
{
  /* just plays region (not current selection) -- no control panel etc */
  int chans,i;
  dac_info *dp;
  if ((background == NOT_IN_BACKGROUND) && (play_list_members > 0)) return;
  if (!(region_ok(region))) return;
  chans = region_chans(region);
  for (i=0;i<chans;i++) 
    {
      dp = add_region_channel_to_play_list(region,i,0,NO_END_SPECIFIED);
      if (dp) dp->region = region;
#if DEBUGGING
      dp->desc = (char *)CALLOC(128,sizeof(char));
      sprintf(dp->desc,"play_region: %d %d %d",region,i,background);
#endif
    }
  start_dac(ss,region_srate(region),chans,background);
}

void play_channel(chan_info *cp, int start, int end, int background)
{
  /* just plays one channel (ignores possible sync) */
  snd_info *sp = NULL;
  dac_info *dp;
  if ((background == NOT_IN_BACKGROUND) && (play_list_members > 0)) return;
  sp = cp->sound;
  if (!(sp->inuse)) return;
  dp = add_channel_to_play_list(cp,sp,start,end);
#if DEBUGGING
  dp->desc = (char *)CALLOC(128,sizeof(char));
  sprintf(dp->desc,"play_channel: %s[%d] %d %d %d",sp->shortname,cp->chan,start,end,background);
#endif
  start_dac(dp->ss,SND_SRATE(sp),1,background);
}

void play_sound(snd_info *sp, int start, int end, int background)
{
  /* just plays one sound (ignores possible sync) */
  int i;
  dac_info *dp;
  if ((background == NOT_IN_BACKGROUND) && (play_list_members > 0)) return;
  if (!(sp->inuse)) return;
#if HAVE_GUILE
  if (call_start_playing_hook(sp))
    {
      reflect_play_stop(sp); /* turns off buttons */
      if (sp->delete_me) completely_free_snd_info(sp); /* dummy snd_info struct for (play "filename") in snd-scm.c */
      return;
    }
#endif
  for (i=0;i<sp->nchans;i++) 
    {
      dp = add_channel_to_play_list(sp->chans[i],sp,start,end);
#if DEBUGGING
      dp->desc = (char *)CALLOC(128,sizeof(char));
      sprintf(dp->desc,"play_sound: %s[%d] %d %d %d",sp->shortname,i,start,end,background);
#endif
    }
  start_dac(sp->state,SND_SRATE(sp),sp->nchans,background);
}

void play_channels(chan_info **cps, int chans, int *starts, int *ur_ends, int background)
{
  /* ends can be NULL */
  int i;
  snd_info *sp = NULL;
  dac_info *dp;
  int *ends;
  if ((background == NOT_IN_BACKGROUND) && (play_list_members > 0)) return;
  if (ur_ends)
    ends = ur_ends;
  else
    {
      ends = (int *)CALLOC(chans,sizeof(int));
      for (i=0;i<chans;i++) ends[i] = NO_END_SPECIFIED;
    }
  for (i=0;i<chans;i++) 
    {
      dp = add_channel_to_play_list(cps[i],sp = (cps[i]->sound),starts[i],ends[i]);
#if DEBUGGING
      dp->desc = (char *)CALLOC(128,sizeof(char));
      sprintf(dp->desc,"play_channels: %s[%d, %d] %d %d %d",
	      sp->shortname,i,cps[i]->chan,starts[i],ends[i],background);
#endif
    }
  if (ur_ends == NULL) FREE(ends);
  if (sp) start_dac(sp->state,SND_SRATE(sp),chans,background);
}

void play_selection(int background)
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
	  ends = (int *)CALLOC(si->chans,sizeof(int));
	  for (i=0;i<si->chans;i++) 
	    {
	      sp = si->cps[i]->sound;
	      if ((sp) && (sp->srate != 1.0) && (sp->srate > 0.0))
		ends[i] = si->begs[i] + (int)(((Float)selection_len() / (Float)(sp->srate))); /* TODO this should use the src->sample counter instead */
	      else ends[i] = si->begs[i] + selection_len();
	    }
	  play_channels(si->cps,si->chans,si->begs,ends,background);
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
  if ((dp->expanding) || (dp->filtering) || (dp->reverbing) || (sp->contrasting)) 
    return(ALL_CHANGES);
  else
    {
      if ((sp->play_direction != 1) || (sp->srate != 1.0) || (dp->cur_srate != 1.0))
	return(JUST_SPEED);
      else
	{
	  if ((sp->amp == dp->cur_amp) && (sp->amp == 1.0))
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
void toggle_dac_pausing(snd_state *ss) {dac_pausing = (!dac_pausing); play_button_pause(ss,dac_pausing);}
int play_in_progress(void) {return(play_list_members > 0);}

static unsigned char **audio_bytes = NULL;
static int audio_bytes_size = 0;
static int audio_bytes_devices = 0;

static MUS_SAMPLE_TYPE **dac_buffers = NULL;
static Float *revin = NULL;
static int dac_buffer_size = 0;
static int dac_buffer_chans = 0; /* chans allocated */
static int revin_size = 0;

#define WRITE_TO_DAC 1
#define WRITE_TO_FILE 0

static int fill_dac_buffers(dac_state *dacp, int write_ok)
{
  int i,j,cursor_change,rchans;
  int bytes,frames;
  Float amp,incr,sr,sincr,ind,indincr,ex,exincr,rev,revincr,fval;
  dac_info *dp;
  snd_info *sp;
  Float *data;
  snd_state *ss;
  MUS_SAMPLE_TYPE *buf;
#if (HAVE_OSS || HAVE_ALSA)
  MUS_SAMPLE_TYPE **dev_bufs;
#endif

  frames = dacp->frames;
  ss = dacp->ss;
#if HAVE_MEMSET
  for (i=0;i<dacp->channels;i++) memset(dac_buffers[i],0,frames * sizeof(MUS_SAMPLE_TYPE));
#else
  for (i=0;i<dacp->channels;i++) 
    for (j=0;j<frames;j++)
      dac_buffers[i][j] = MUS_SAMPLE_0;
#endif  

  if (global_reverb) 
    {
      if ((revin == NULL) || (revin_size < frames))
	{
	  if (revin) FREE(revin);
	  revin = (Float *)CALLOC(frames,sizeof(Float));
	  revin_size = frames;
	}
      else 
	{
#if HAVE_MEMSET
	  memset((char *)revin,0,frames * sizeof(Float));
#else
	  for (j=0;j<frames;j++) revin[i] = 0.0;
#endif
	}
    }

  if (dac_pausing) 
    cursor_change = 0;
  else
    {
      cursor_time += frames;
      cursor_change = (cursor_time >= CURSOR_UPDATE_INTERVAL);
      for (i=0;i<=max_active_slot;i++)
	{
	  dp = play_list[i];
	  if (dp)
	    {

	      /* check for moving cursor */
	      sp = dp->sp; /* can be nil if region playing */
	      if ((sp) && ((!(sp->inuse)) || (sp->playing == 0))) {stop_playing(dp); return(frames);}
	      if ((sp) && (cursor_change) && (sp->cursor_follows_play != DONT_FOLLOW)) 
		handle_cursor(dp->cp,cursor_moveto(dp->cp,current_location(dp->chn_fd)));

	      /* add a buffer's worth from the current source into dp->audio_chan */
	      buf = dac_buffers[dp->audio_chan];
	      switch (choose_dac_op(dp,sp))
		{
		case NO_CHANGE:
		  /* simplest case -- no changes at all */
		  for (j=0;j<frames;j++)
		    buf[j] += next_sample(dp->chn_fd);
		  break;

		case NO_CHANGE_AND_NO_SCALING:
		  for (j=0;j<frames;j++)
		    buf[j] += next_sample_unscaled(dp->chn_fd);
		  break;

		case JUST_AMP:
		  /* sp->amp is current UI value, dp->cur_amp is current local value */
		  amp = dp->cur_amp;
		  incr = (sp->amp - amp) / (Float)(frames);
		  for (j=0;j<frames;j++,amp+=incr) 
		    buf[j] += MUS_FLOAT_TO_SAMPLE(next_sample_to_float(dp->chn_fd) * amp);
		  dp->cur_amp = amp;
		  break;

		case JUST_SPEED:
		  /* includes amp changes */
		  /* sp->srate is current UI value, dp->cur_srate is current local value */
		  amp = dp->cur_amp;
		  incr = (sp->amp - amp) / (Float)(frames);
		  sr = dp->cur_srate;
		  sincr = (sp->srate*sp->play_direction - sr) / (Float)(frames);
		  if ((sr != 0.0) || (sincr != 0.0))
		    {
		      for (j=0;j<frames;j++,amp+=incr,sr+=sincr) 
			buf[j] += MUS_FLOAT_TO_SAMPLE(amp * speed(dp,sr));
		    }
		  dp->cur_srate = sr;
		  dp->cur_amp = amp;
		  break;

		case ALL_CHANGES:
		  amp = dp->cur_amp;
		  incr = (sp->amp - amp) / (Float)(frames);
		  sr = dp->cur_srate;
		  sincr = (sp->srate*sp->play_direction - sr) / (Float)(frames);
		  ind = dp->cur_index;
		  indincr = (sp->contrast - ind) / (Float)(frames);
		  ex = dp->cur_exp;
		  exincr = (sp->expand - ex) / (Float)(frames);
		  rev = dp->cur_rev;
		  revincr = (sp->revscl - rev) / (Float)(frames);
		  if ((dp->filtering) && (sp->filter_changed))
		    {
		      data = sample_linear_env(sp->filter_env,sp->filter_order);
		      mus_make_fir_coeffs(sp->filter_order,data,dp->a); /* since dp->a is used directly, this might work */
		      FREE(data);
		      sp->filter_changed = 0;
		    }
		  for (j=0;j<frames;j++,amp+=incr,sr+=sincr,ind+=indincr,ex+=exincr,rev+=revincr) 
		    {
		      if (dp->expanding) fval = expand(dp,sr,ex); else fval = speed(dp,sr);
		      if (sp->contrasting) fval = contrast(dp,amp,ind,fval); else fval *= amp;
		      if (dp->filtering) fval = mus_fir_filter(dp->flt,fval);
		      if (dp->reverbing) revin[j] += fval*rev;
		      buf[j] += MUS_FLOAT_TO_SAMPLE(fval);
		    }
		  dp->cur_srate = sr;
		  dp->cur_amp = amp;
		  dp->cur_rev = rev;
		  dp->cur_exp = ex;
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
		  if ((dp->end == 0) || (read_sample_eof(dp->chn_fd)))
		    stop_playing(dp);
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
      if (global_reverb) 
	{
	  if (dac_buffer_chans < revchans) rchans = dac_buffer_chans; else rchans = revchans;
	  for (i=0;i<frames;i++)
	    reverb(global_reverb,revin[i],dac_buffers,i,rchans);
	  if (play_list_members == 0)
	    {
	      revdecay += frames;
	      if (revdecay > dacp->reverb_ring_frames) 
		{
		  global_reverbing=0; 
		  revdecay=0;
		}
	    }
	}
    }

  /* now parcel these buffers out to the available devices */
#if (HAVE_OSS || HAVE_ALSA)
  if (write_ok == WRITE_TO_DAC) 
    {
      dev_bufs = dac_buffers;
      for (i=0;i<dacp->devices;i++)
	if (dev_fd[i] != -1)
	  {
	    mus_file_write_buffer(dacp->out_format,
				  0,frames-1,
				  dacp->chans_per_device[i],
				  dev_bufs,
				  (char *)(audio_bytes[i]),
				  data_clipped(ss));
	    dev_bufs += dacp->chans_per_device[i];
	  }
      for (i=0;i<dacp->devices;i++)
	if (dev_fd[i] != -1)
	  {
	    bytes = dacp->chans_per_device[i] * frames * mus_data_format_to_bytes_per_sample(dacp->out_format);
	    mus_audio_write(dev_fd[i],(char *)(audio_bytes[i]),bytes);
	  }
    }
#else
  if (write_ok == WRITE_TO_DAC) 
    {
      mus_file_write_buffer(dacp->out_format,0,frames-1,dacp->channels,dac_buffers,(char *)(audio_bytes[0]),data_clipped(ss));
      bytes = dacp->channels * frames * mus_data_format_to_bytes_per_sample(dacp->out_format);
      mus_audio_write(dev_fd[0],(char *)(audio_bytes[0]),bytes);
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

static void dac_error(dac_state *dacp, const char *file, int line, const char *function)
{
  snd_error("can't play %s\n  (%s)\n  [%s[%d] %s]",
	    describe_dac(dacp,0),
	    (last_print) ? last_print : "reason not known",
	    file,line,function);
}


/* -------------------------------- initialize DAC -------------------------------- */

static void make_dac_buffers(dac_state *dacp)
{
  /* make the per-channel buffers and audio output buffers */
  int bytes,i;
  if ((dac_buffers == NULL) || (dac_buffer_chans < dacp->channels) || (dac_buffer_size < dacp->frames))
    {
      if (dac_buffers)
	{
	  for (i=0;i<dac_buffer_chans;i++) FREE(dac_buffers[i]);
	  FREE(dac_buffers);
	}
      dac_buffers = (MUS_SAMPLE_TYPE **)CALLOC(dacp->channels,sizeof(MUS_SAMPLE_TYPE *));
      for (i=0;i<dacp->channels;i++) 
	dac_buffers[i] = (MUS_SAMPLE_TYPE *)CALLOC(dacp->frames,sizeof(MUS_SAMPLE_TYPE));
      dac_buffer_chans = dacp->channels;
      dac_buffer_size = dacp->frames;
    }
  bytes = dacp->channels * dac_buffer_size * mus_data_format_to_bytes_per_sample(dacp->out_format);
  if ((audio_bytes_size < bytes) || (audio_bytes_devices < dacp->devices))
    {
      if (audio_bytes)
	{
	  for (i=0;i<audio_bytes_devices;i++) FREE(audio_bytes[i]);
	  FREE(audio_bytes);
	}
      audio_bytes = (unsigned char **)CALLOC(dacp->devices,sizeof(unsigned char *));
      for (i=0;i<dacp->devices;i++) 
	audio_bytes[i] = (unsigned char *)CALLOC(bytes,sizeof(unsigned char));
      audio_bytes_size = bytes;
      audio_bytes_devices = dacp->devices;
    }
}

static void stop_audio_output (dac_state *dacp);

#if (HAVE_ALSA || HAVE_OSS)

int mus_audio_compatible_format(int dev) 
{
#ifndef PPC
  int err, i;
  float val[32];
  int ival[32];
  err = mus_audio_mixer_read(dev,MUS_AUDIO_FORMAT,32,val);
  if (err != MUS_ERROR)
    {
      for (i=0;i<=(int)(val[0]);i++) ival[i] = (int)(val[i]);
      /*          ^ this cast is vital!  Memory clobbered otherwise in LinuxPPC */
      for (i=1;i<=ival[0];i++)
	if (ival[i] == MUS_COMPATIBLE_FORMAT) 
	  return(MUS_COMPATIBLE_FORMAT);
      for (i=1;i<=ival[0];i++) 
	if ((ival[i] == MUS_BINT) || (ival[i] == MUS_LINT) ||
	    (ival[i] == MUS_BFLOAT) || (ival[i] == MUS_LFLOAT) ||
	    (ival[i] == MUS_BSHORT) || (ival[i] == MUS_LSHORT))
	  return(ival[i]);
      for (i=1;i<=ival[0];i++) 
	if ((ival[i] == MUS_MULAW) || (ival[i] == MUS_ALAW) ||
	    (ival[i] == MUS_UBYTE) || (ival[i] == MUS_BYTE))
	  return(ival[i]);
      return(ival[1]);
    }
#endif
  return(MUS_COMPATIBLE_FORMAT);
}

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
      for (card=0;card<cards;card++) 
	{
	  if ((err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(card), MUS_AUDIO_PORT, ALSA_MAX_DEVICES, val)) != MUS_NO_ERROR) 
	    {
	      snd_error("%s[%d] %s: mus_audio_mixer_read", __FILE__, __LINE__, __FUNCTION__);
	    }
	  devs = (int)(val[0]);
	  /* scan all devices in the card */
	  for (d=0;d<devs;d++) 
	    {
	      dev = (int)(val[d+1]);
	      if ((err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(card)|dev, MUS_AUDIO_DIRECTION, 0, &direction)) != MUS_NO_ERROR) 
		{
		  snd_error("%s: can't read direction, ignoring device %d", __FUNCTION__, dev);
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
      for (d=0;d<index;d++) 
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
  int i,d;
  int samples_per_channel = 256;
  float val[ALSA_MAX_DEVICES];
  static int out_dev[ALSA_MAX_DEVICES];
  int alloc_devs=0;
  int alloc_chans=0;
  int oss_available_chans = 2;

  ss = dacp->ss;
  if (mus_audio_api() == ALSA_API) 
    {
      scan_audio_devices();

      /* allocate devices for playback */
      alloc_chans = 0;
      alloc_devs = 0;
      for (d=0;d<ALSA_MAX_DEVICES;d++) out_dev[d] = -1; 
      for (d=0;d<MAX_DEVICES;d++) dev_fd[d] = -1;
      if (feed_first_device == 0) 
	{
	  /* see if widest device can accomodate all channels */
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
	      for (d=0;d<alsa_devices_available;d++) 
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
		  for (d=0;d<ALSA_MAX_DEVICES;d++) out_dev[d] = -1;
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
	  if (dac_folding(ss)) snd_warning("folding %d chans into %d ", dacp->channels, alloc_chans);
	  dacp->channels = alloc_chans;
	}
      /* read the number of samples per channel the device wants buffered */
      if ((err = mus_audio_mixer_read(alsa_devices[out_dev[0]], MUS_AUDIO_SAMPLES_PER_CHANNEL, 2, val)) != -1) 
	{
	  samples_per_channel = (int)(val[0]);
	}
      dacp->frames = samples_per_channel;
      set_dac_size(ss, dacp->frames*mus_data_format_to_bytes_per_sample(dacp->out_format));
      /* open all allocated devices */
      for (d=0;d<alloc_devs;d++) 
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
					    dacp->frames*channels*mus_data_format_to_bytes_per_sample(dacp->out_format));
	  unset_dac_print();
      
	  if (dev_fd[d] == -1) 
	    {
	      /* could not open a device, close all others and quit playback */
	      int i;
	      for (i=0;i<d;i++) 
		{
		  mus_audio_close(alsa_devices[out_dev[i]]);
		}
	      dac_error(dacp,__FILE__,__LINE__,__FUNCTION__);
	      dac_running = 0;
	      unlock_recording_audio();
	      if (global_reverb) {free_reverb(global_reverb); global_reverb = NULL;}
	      max_active_slot = -1;
	      free_dac_state(dacp); 
	      return(FALSE);
	    }
	}
      dacp->devices = alloc_devs;
      /* for now assume all are same number of chans */
      dacp->chans_per_device = (int *)CALLOC(dacp->devices,sizeof(int));
      for (i=0;i<dacp->devices;i++) dacp->chans_per_device[i] = dacp->channels / dacp->devices;
      make_dac_buffers(dacp);
    } 
  else 
    {
      /* api == OSS_API */
      if (dacp->channels > 2)
	{
	  err = mus_audio_mixer_read(audio_output_device(ss),MUS_AUDIO_CHANNEL,0,val);
	  if (err != -1) oss_available_chans = (int)(val[0]);
	}
      for (i=0;i<MAX_DEVICES;i++) dev_fd[i] = -1;
      /* see if we can play 16 bit output */
      dacp->out_format = mus_audio_compatible_format(audio_output_device(ss));
  #ifndef PPC
      /* check for chans>def chans, open 2 cards if available */
      if ((oss_available_chans < dacp->channels) && (dacp->channels == 4))
	{
	  if (mus_audio_systems() > 1)
	    {
	      set_dac_print();
	      dev_fd[0] = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss),dacp->srate,2,dacp->out_format,dac_size(ss));
	      unset_dac_print();
	      if (dev_fd[0] != MUS_ERROR) 
		dev_fd[1] = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(1) | audio_output_device(ss),dacp->srate,2,dacp->out_format,dac_size(ss));
	    }
	  else
	    {
	      /* there is one special case here: Ensoniq's allow you to play quad
	       * by sending two channels (non-clock-synchronous with the other two)
	       * out the line in port, but this possibility confuses LinuxPPC (OSS-Free)
	       */
	      set_dac_print();
	      dev_fd[0] = mus_audio_open_output(MUS_AUDIO_AUX_OUTPUT,dacp->srate,2,dacp->out_format,dac_size(ss));
	      unset_dac_print();
	      if (dev_fd[0] != MUS_ERROR) 
		dev_fd[1] = mus_audio_open_output(audio_output_device(ss),dacp->srate,2,dacp->out_format,dac_size(ss));
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
	  if (dac_folding(ss)) snd_warning("folding %d chans into %d ",dacp->channels,oss_available_chans);
	  dacp->channels = oss_available_chans;
	}
      set_dac_print();
      if (dev_fd[0] == MUS_ERROR)
	dev_fd[0] = mus_audio_open_output(audio_output_device(ss),dacp->srate,dacp->channels,dacp->out_format,dac_size(ss));
      unset_dac_print();
      if (dev_fd[0] == MUS_ERROR)
	{
	  dac_error(dacp,__FILE__,__LINE__,__FUNCTION__);
	  stop_audio_output(dacp);
	  return(FALSE);
	}
      dacp->devices = (dev_fd[1] != -1) ? 2 : 1;
      dacp->chans_per_device = (int *)CALLOC(dacp->devices,sizeof(int));
      for (i=0;i<dacp->devices;i++) dacp->chans_per_device[i] = dacp->channels / dacp->devices;
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
      err = mus_audio_mixer_read(audio_output_device(ss),MUS_AUDIO_CHANNEL,0,val);
      if (err != MUS_ERROR) 
	available_chans = (int)(val[0]);
      else 
	{
	  snd_error("can't get audio output chans? (%d) ",audio_output_device(ss));
	  return(FALSE);
	}
    }
  for (i=0;i<MAX_DEVICES;i++) dev_fd[i] = -1;
  dacp->out_format = MUS_COMPATIBLE_FORMAT;
  if (available_chans < dacp->channels) 
    {
      if (dac_folding(ss)) snd_warning("folding %d chans into %d ",dacp->channels,available_chans);
      dacp->channels = available_chans;
    }
  
  set_dac_print();
  if (dev_fd[0] == MUS_ERROR)
    dev_fd[0] = mus_audio_open_output(audio_output_device(ss),dacp->srate,dacp->channels,dacp->out_format,dac_size(ss));
  unset_dac_print();
  if (dev_fd[0] == MUS_ERROR)
    {
      dac_error(dacp,__FILE__,__LINE__,__FUNCTION__);
      stop_audio_output(dacp);
      return(FALSE);
    }
  dacp->devices = 1;
  dacp->chans_per_device = (int *)CALLOC(dacp->devices,sizeof(int));
  for (i=0;i<dacp->devices;i++) dacp->chans_per_device[i] = available_chans / dacp->devices;
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
      for (i=0;i<=max_active_slot;i++)
	{
	  dp = play_list[i];
	  if (dp)
	    {
	      if (dp->audio_chan >= dacp->channels)
		{
		  if (dac_folding(dacp->ss))
		    dp->audio_chan %= dacp->channels;
		  else stop_playing(dp);
		}
	    }
	}
      dac_running = 1;
      fill_dac_buffers(dacp,WRITE_TO_DAC);
      lock_apply(dacp->ss,NULL);
      return(TRUE);
    }
  return(FALSE);
}
 
static void stop_audio_output (dac_state *dacp)
{
   int i;
   for (i=0;i<MAX_DEVICES;i++)
     if (dev_fd[i] != -1) 
       {
	 mus_audio_close(dev_fd[i]);
	 dev_fd[i] = -1;
       }
   dac_running = 0;
   unlock_recording_audio();
   dac_pausing = 0;
   if (global_reverb) {free_reverb(global_reverb); global_reverb = NULL;}
   max_active_slot = -1;
   unlock_apply(dacp->ss,NULL);
   free_dac_state(dacp);
}

static BACKGROUND_TYPE dac_in_background(GUI_POINTER ptr)
{
  /* slice 0: try to open audio output devices and get ready to send samples
   *       1: loop sending data until the play_list is empty or some error (C-g) happens
   *       2: try to close all active outputs and remove background procedure
   */
  dac_state *dacp = (dac_state *)ptr;
  switch (dacp->slice)
    {
    case 0:
      if (start_audio_output(dacp))
	{
	  dacp->slice = 1;
	  return(BACKGROUND_CONTINUE);
	}
      else 
	{
	  free_dac_state(dacp);
	  return(BACKGROUND_QUIT);
	}
      break;
    case 1:
      fill_dac_buffers(dacp,WRITE_TO_DAC);
      if ((!global_reverbing) && (play_list_members == 0)) dacp->slice = 2;
      return(BACKGROUND_CONTINUE);
      break;
     case 2:
       stop_audio_output(dacp);
       return(BACKGROUND_QUIT);
       break;
     }
  return(BACKGROUND_QUIT);
}
 

/* ---------------- support for Apply button (snd-apply.c) ---------------- */

static dac_state *apply_dacp = NULL;

void initialize_apply(snd_info *sp, int chans, int dur)
{
  int curchan=0;
  snd_state *ss;
  ss = sp->state;
  stop_playing_all_sounds();
  max_active_slot = -1;
  play_list_members = 0;

  dac_running = 1; /* this keeps start_dac from actually starting the dac */
  apply_dacp = (dac_state *)CALLOC(1,sizeof(dac_state));
  apply_dacp->slice = 0;
  apply_dacp->ss = ss;
  apply_dacp->srate = SND_SRATE(sp);
  apply_dacp->out_format = MUS_COMPATIBLE_FORMAT;
  if (apply_dacp->srate <= 0) apply_dacp->srate = 44100;
  apply_dacp->channels = chans;
  apply_dacp->frames = 8192;
  apply_dacp->devices = 1;
  apply_dacp->chans_per_device = (int *)CALLOC(1,sizeof(int));
  apply_dacp->chans_per_device[0] = chans;
  apply_dacp->reverb_ring_frames = (int)(apply_dacp->srate * reverb_decay(ss));
  make_dac_buffers(apply_dacp);
  switch (ss->apply_choice)
    {
    case APPLY_TO_SOUND: 
      play_sound(sp,0,dur,IN_BACKGROUND);
      break;
    case APPLY_TO_SELECTION: 
      play_selection(IN_BACKGROUND);
      break;
    case APPLY_TO_CHANNEL: 
      if (sp->selected_channel != NO_SELECTION)
	curchan = sp->selected_channel;
      play_channel(sp->chans[curchan],0,dur,IN_BACKGROUND);
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
  free_dac_state(apply_dacp);
  apply_dacp = NULL;
  if (global_reverb) {free_reverb(global_reverb); global_reverb=NULL;}
}

int run_apply(int ofd)
{
  int len;
  len = fill_dac_buffers(apply_dacp,WRITE_TO_FILE);
   mus_file_write(ofd,0,len-1,apply_dacp->channels,dac_buffers);
  return(len);
}


/* -------------------------------- scheme connection -------------------------------- */

#if HAVE_GUILE

static SCM g_play_1(SCM samp_n, SCM snd_n, SCM chn_n, int background, int syncd, SCM end_n) 
{
  /* all chans if chn_n omitted, arbitrary file if snd_n is name */
  snd_info *sp;
  chan_info *cp;
  sync_info *si = NULL;
  char *name = NULL,*urn;
  int i,samp = 0;
  int end = NO_END_SPECIFIED;
  int *ends = NULL;
  if (SCM_INUMP(end_n)) end = SCM_INUM(end_n);

  /* if even samp_n is SCM_UNDEFINED, start_dac? */

  if (gh_string_p(samp_n))
    {
      /* filename beg end background syncd ignored */
      urn = gh_scm2newstr(samp_n,NULL);
      name = mus_file_full_name(urn);
      free(urn);
      sp = make_sound_readable(get_global_state(),name,FALSE);
      if (sp == NULL) 
	{
	  if (name) FREE(name); 
	  return(scm_throw(NO_SUCH_FILE,SCM_LIST2(gh_str02scm(S_play),samp_n)));
	}
      sp->shortname = filename_without_home_directory(name);
      sp->fullname = NULL;
      sp->delete_me = 1;
      samp = g_scm2intdef(snd_n,0);
      if (SCM_INUMP(chn_n)) end = SCM_INUM(chn_n);
      play_sound(sp,samp,end,background);
      if (name) FREE(name);
    }
  else
    {
      ERRB1(samp_n,S_play);
      ERRCP(S_play,snd_n,chn_n,2);
      sp = get_sp(snd_n);
      if (sp == NULL) return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_play),snd_n)));
      samp = g_scm2intdef(samp_n,0);
      if ((syncd) && (sp->syncing != 0))
	{
	  si = snd_sync(sp->state,sp->syncing);
	  if (end != NO_END_SPECIFIED)
	    {
	      ends = (int *)CALLOC(si->chans,sizeof(int));
	      for (i=0;i<si->chans;i++) ends[i] = end;
	    }
	  play_channels(si->cps,si->chans,si->begs,ends,background);
	  si = free_sync_info(si);
	  FREE(ends);
	}
      else
	{
	  if (!(gh_number_p(chn_n)))
	    play_sound(sp,samp,end,background);
	  else 
	    {
	      cp = get_cp(snd_n,chn_n,S_play);
	      if (cp) 
		play_channel(cp,samp,end,background);
	      else return(scm_throw(NO_SUCH_CHANNEL,SCM_LIST3(gh_str02scm(S_play),snd_n,chn_n)));
	    }
	}
    }
  return(SCM_BOOL_T);
}

static int bool_int_or_zero(SCM n) {if (SCM_TRUE_P(n)) return(1); else return(g_scm2intdef(n,0));}

static SCM g_play(SCM samp_n, SCM snd_n, SCM chn_n, SCM syncd, SCM end_n) 
{
  #define H_play "(" S_play " &optional (start 0) snd chn sync end) plays snd or snd's channel chn starting at start.\n\
   'start' can also be a filename: (" S_play " \"oboe.snd\").  If 'sync' is true, all sounds syncd to snd are played.\n\
   if 'end' is not given, it plays to the end of the sound."

  return(g_play_1(samp_n,snd_n,chn_n,TRUE,bool_int_or_zero(syncd),end_n));
}

static SCM g_play_selection(SCM wait) 
{
  #define H_play_selection "(" S_play_selection " &optional (wait #f)) plays the current selection"
  if (selection_is_active())
    {
      play_selection(!(bool_int_or_zero(wait)));
      return(SCM_BOOL_T);
    }
  return(scm_throw(NO_ACTIVE_SELECTION,SCM_LIST1(gh_str02scm(S_play_selection))));
}

static SCM g_play_and_wait(SCM samp_n, SCM snd_n, SCM chn_n, SCM syncd, SCM end_n) 
{
  #define H_play_and_wait "(" S_play_and_wait " &optional (start 0) snd chn end) plays snd or snd's channel chn starting at start\n\
   and waiting for the play to complete before returning.  'start' can also be a filename: (" S_play_and_wait " \"oboe.snd\")"

  return(g_play_1(samp_n,snd_n,chn_n,FALSE,bool_int_or_zero(syncd),end_n));
}

static SCM g_stop_playing(SCM snd_n)
{
  #define H_stop_playing "(" S_stop_playing " &optional snd) stops play in progress"
  snd_info *sp = NULL;
  if (gh_number_p(snd_n)) sp = get_sp(snd_n);
  if (sp) stop_playing_sound(sp); else stop_playing_all_sounds();
  return(SCM_BOOL_F);
}


/* -------- players -------- */

static snd_info **players = NULL;
static int *player_chans = NULL;
static int players_size = 0;

static int new_player_index(void)
{
  int i,old_size;
  if (players_size == 0)
    {
      players_size = 8;
      players = (snd_info **)CALLOC(players_size,sizeof(snd_info *));
      player_chans = (int *)CALLOC(players_size,sizeof(int));
      return(-1);
    }
  for (i=1;i<players_size;i++)
    if (players[i] == NULL)
      return(-i);
  old_size = players_size;
  players_size += 8;
  players = (snd_info **)REALLOC(players,players_size * sizeof(snd_info *));
  player_chans = (int *)REALLOC(player_chans,players_size * sizeof(int));
  for (i=old_size;i<players_size;i++)
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
  if ((-index) < players_size) 
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
  FREE(sp->fullname);
  FREE(sp->chans);
  FREE(sp);
}


/* add-player make-player stop-player start-playing */

static SCM g_make_player(SCM snd, SCM chn)
{
  #define H_make_player "(" S_make_player " &optional snd chn) prepares snd's channel chn for " S_add_player
  snd_info *true_sp,*new_sp;
  chan_info *cp;
  ERRCP(S_make_player,snd,chn,1);
  true_sp = get_sp(snd);
  if (true_sp == NULL) return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_make_player),snd)));
  cp = get_cp(snd,chn,S_make_player);
  if (cp)
    {
      new_sp = make_snd_info(NULL,get_global_state(),"wrapper",true_sp->hdr,new_player_index());
      FREE(new_sp->sgx); /* no built-in GUI */
      new_sp->sgx = NULL;
      new_sp->chans[cp->chan] = cp;
      return(gh_int2scm(make_player(new_sp,cp)));
    }
  else return(scm_throw(NO_SUCH_CHANNEL,SCM_LIST3(gh_str02scm(S_make_player),snd,chn)));
}

static SCM g_add_player(SCM snd_chn, SCM start, SCM end)
{
  #define H_add_player "(" S_add_player " &optional player start end) starts playing snd's channel chn"
  snd_info *sp;
  chan_info *cp;
  int index;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(snd_chn)),snd_chn,SCM_ARG1,S_add_player);
  index = -gh_scm2int(snd_chn);
  sp = players[index];
  if (sp)
    {
      cp = sp->chans[player_chans[index]];
      add_channel_to_play_list(cp,sp,g_scm2intdef(start,0),g_scm2intdef(end,NO_END_SPECIFIED));
    }
  /* else no such player? */
  else return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_add_player),snd_chn)));
  return(snd_chn);
}

static SCM g_start_playing(SCM chans, SCM srate, SCM in_background)
{
  #define H_start_playing "(" S_start_playing " &optional chans srate in-background)"
  start_dac(get_global_state(),g_scm2intdef(srate,44100),g_scm2intdef(chans,1),bool_int_or_one(in_background));
  return(SCM_BOOL_F);
}

static SCM g_stop_player(SCM snd_chn)
{
  #define H_stop_player "(" S_stop_player " player) stops player"
  int index;
  snd_info *sp;
  index = -gh_scm2int(snd_chn);
  sp = players[index];
  if (sp) 
    stop_playing_sound(sp);
  else return(scm_throw(NO_SUCH_SOUND,SCM_LIST2(gh_str02scm(S_stop_player),snd_chn)));
  return(snd_chn);
}

/* also the dac filler needs to run on empty buffers in this case? */



static SCM start_playing_hook,stop_playing_hook,stop_playing_region_hook,stop_playing_channel_hook;

#if HAVE_HOOKS
static void call_stop_playing_hook(snd_info *sp)
{
  if (HOOKED(stop_playing_hook))
    g_c_run_or_hook(stop_playing_hook,SCM_LIST1(gh_int2scm(sp->index)));
}

static void call_stop_playing_channel_hook(snd_info *sp, chan_info *cp)
{
  if (HOOKED(stop_playing_channel_hook))
    g_c_run_or_hook(stop_playing_channel_hook,SCM_LIST2(gh_int2scm(sp->index),gh_int2scm(cp->chan)));
}

static void call_stop_playing_region_hook(int n)
{
  if (HOOKED(stop_playing_region_hook))
    g_c_run_or_hook(stop_playing_region_hook,SCM_LIST1(gh_int2scm(n)));
}

static int call_start_playing_hook(snd_info *sp)
{
  SCM stop = SCM_BOOL_F;
  if (HOOKED(start_playing_hook))
    stop = g_c_run_or_hook(start_playing_hook,SCM_LIST1(gh_int2scm(sp->index)));
  return(SCM_TRUE_P(stop));
}
#endif

void g_init_dac(SCM local_doc)
{
  DEFINE_PROC(gh_new_procedure("set-" S_reverb_funcs,SCM_FNC g_set_reverb_funcs,3,0,0),H_set_reverb_funcs);
  DEFINE_PROC(gh_new_procedure("set-" S_expand_funcs,SCM_FNC g_set_expand_funcs,3,0,0),H_set_expand_funcs);
  DEFINE_PROC(gh_new_procedure("set-" S_contrast_func,SCM_FNC g_set_contrast_func,1,0,0),H_set_contrast_func);
  DEFINE_PROC(gh_new_procedure(S_reverb_funcs,SCM_FNC g_reverb_funcs,0,0,0),H_reverb_funcs);
  DEFINE_PROC(gh_new_procedure(S_expand_funcs,SCM_FNC g_expand_funcs,0,0,0),H_expand_funcs);
  DEFINE_PROC(gh_new_procedure(S_contrast_func,SCM_FNC g_contrast_func,0,0,0),H_contrast_func);

  DEFINE_PROC(gh_new_procedure(S_play,SCM_FNC g_play,0,5,0),H_play);
  DEFINE_PROC(gh_new_procedure(S_play_selection,SCM_FNC g_play_selection,0,1,0),H_play_selection);
  DEFINE_PROC(gh_new_procedure(S_play_and_wait,SCM_FNC g_play_and_wait,0,5,0),H_play_and_wait);
  DEFINE_PROC(gh_new_procedure(S_stop_playing,SCM_FNC g_stop_playing,0,1,0),H_stop_playing);

  DEFINE_PROC(gh_new_procedure(S_make_player,SCM_FNC g_make_player,0,2,0),H_make_player);
  DEFINE_PROC(gh_new_procedure(S_add_player,SCM_FNC g_add_player,1,2,0),H_add_player);
  DEFINE_PROC(gh_new_procedure(S_start_playing,SCM_FNC g_start_playing,0,3,0),H_start_playing);
  DEFINE_PROC(gh_new_procedure(S_stop_player,SCM_FNC g_stop_player,1,0,0),H_stop_player);

#if HAVE_HOOKS
  stop_playing_hook = scm_create_hook(S_stop_playing_hook,1);                     /* arg = sound */
  stop_playing_channel_hook = scm_create_hook(S_stop_playing_channel_hook,2);     /* args = sound channel */
  stop_playing_region_hook = scm_create_hook(S_stop_playing_region_hook,1);       /* arg = region number */
  start_playing_hook = scm_create_hook(S_start_playing_hook,1);                   /* arg = sound */
#else
  stop_playing_hook = gh_define(S_stop_playing_hook,SCM_BOOL_F);
  stop_playing_channel_hook = gh_define(S_stop_playing_channel_hook,SCM_BOOL_F);
  stop_playing_region_hook = gh_define(S_stop_playing_region_hook,SCM_BOOL_F);
  start_playing_hook = gh_define(S_start_playing_hook,SCM_BOOL_F);
#endif
}

#endif
