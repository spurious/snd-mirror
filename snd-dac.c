/* TODO  make revlen follow slider in "real-time":
 *         set up line_size in mus_make_comb to 5.0*srate/25641, then
 *         then as running, at each block reset to initial - new scaled
 *         (negative pm = longer delay)
 */

/* this was sound-oriented; changed to be channel-oriented 31-Aug-00 */
/*
 * each channel currently being played has an associated dac_info struct
 *   all active dac_info structs are held in a play_list
 *   channels can come and go as a play is in progress
 */

#include "snd.h"

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
  int region;          /* to reset region-browser play button upon completion */
  src_state *src;
  snd_info *sp;        /* needed to see button callback changes etc */
  chan_info *cp;
  snd_state *ss;
  int end, no_scalers, never_sped, expand_ring_frames;
} dac_info;


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

static void *make_expand(snd_info *sp, Float sampling_rate, Float initial_ex, dac_info *dp)
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


static int prime (int num)
{
  int lim, i;
  if (num == 2) return(1);
  if ((num%2) == 1)
    {
      lim = (int)(sqrt(num));
      for (i = 3; i < lim; i += 2)
	if ((num%i) == 0) 
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


/* -------------------------------- user-defined control-panel functions -------------------------------- */

enum {NREVERB, FREEVERB, USERVERB};
static int which_reverb = NREVERB;

char *reverb_name(void)
{
  switch (which_reverb)
    {
    case USERVERB: return("yervrb:"); break;
    case FREEVERB: return("frevrb:"); break;
    default: return(STR_reverb); break;
    }
}

/* user hooks into reverb */
static SCM g_make_reverb, g_reverb, g_free_reverb;

static SCM g_reverb_procedures(void) 
{
  return(SCM_LIST3(g_reverb, g_make_reverb, g_free_reverb));
}

static SCM g_set_reverb_procedures(SCM rev, SCM make_rev, SCM free_rev)
{
  #define H_reverb_control_procedures "(" S_reverb_control_procedures ") -> list of the 3 reverb procedures (reverb make-reverb free-reverb)"
  #define H_set_reverb_control_procedures "(" "set-" S_reverb_control_procedures " reverb make-reverb free-reverb) sets the current reverb procedures"

  char *errmsg;
  SCM errstr, bad_func = SCM_BOOL_F;

  errmsg = procedure_ok(rev, 3, "set-" S_reverb_control_procedures, "reverb", 1);
  if (errmsg == NULL) 
    {
      errmsg = procedure_ok(make_rev, 2, "set-" S_reverb_control_procedures, "make-reverb", 2); 
      if (errmsg == NULL) 
	{
	  errmsg = procedure_ok(free_rev, 1, "set-" S_reverb_control_procedures, "free-reverb", 3); 
	  if (errmsg) bad_func = free_rev;
	}
      else bad_func = make_rev;
    }
  else bad_func = rev;
  if (errmsg)
    {
      errstr = TO_SCM_STRING(errmsg);
      FREE(errmsg);
      return(snd_bad_arity_error(S_reverb_control_procedures, errstr, bad_func));
    }
  if (PROCEDURE_P(g_reverb)) snd_unprotect(g_reverb);
  if (PROCEDURE_P(g_make_reverb)) snd_unprotect(g_make_reverb);
  if (PROCEDURE_P(g_free_reverb)) snd_unprotect(g_free_reverb);

  if (NOT_FALSE_P(rev))
    {
      g_reverb = rev;
      g_make_reverb = make_rev;
      g_free_reverb = free_rev;
      if (SCM_EQ_P(g_reverb, SND_LOOKUP("snd-nrev")))
	which_reverb = NREVERB;
      else
	{
	  if (SCM_EQ_P(g_reverb, SND_LOOKUP("snd-freeverb")))
	    which_reverb = FREEVERB;
	  else which_reverb = USERVERB;
	}
    }
  else 
    {
      g_make_reverb = SND_LOOKUP("make-snd-nrev");
      g_reverb = SND_LOOKUP("snd-nrev");
      g_free_reverb = SND_LOOKUP("free-snd-nrev");
      which_reverb = NREVERB;
    }
  set_reverb_labels(reverb_name());
  snd_protect(g_reverb);
  snd_protect(g_make_reverb);
  snd_protect(g_free_reverb);
  return(rev);
}

/* user hook into contrast */
static SCM g_contrast;
static int use_g_contrast = 0;

static SCM g_contrast_procedure(void) 
{
  return(g_contrast);
}

static SCM g_set_contrast_procedure(SCM func)
{
  #define H_contrast_control_procedure "(" S_contrast_control_procedure ") -> current contrast procedure"
  #define H_set_contrast_control_procedure "(" "set-" S_contrast_control_procedure " proc) sets the current contrast procedure"

  char *errmsg;
  SCM errstr;

  errmsg = procedure_ok(func, 2, "set-" S_contrast_control_procedure, "contrast", 1);
  if (errmsg)
    {
      errstr = TO_SCM_STRING(errmsg);
      FREE(errmsg);
      return(snd_bad_arity_error(S_contrast_control_procedure, errstr, func));
    }

  if (PROCEDURE_P(g_contrast)) snd_unprotect(g_contrast);
  if (NOT_FALSE_P(func))
    {
      g_contrast = func;
      use_g_contrast = 1;
    }
  else 
    {
      g_contrast = SND_LOOKUP("snd-contrast");
      use_g_contrast = 0;
    }
  snd_protect(g_contrast);
  return(func);
}

static SCM play_hook, start_playing_hook, stop_playing_hook, stop_playing_region_hook, stop_playing_channel_hook;



/* -------------------------------- fcomb gen ----------------------- */

static int MUS_FCOMB = 0;

typedef struct {
  mus_any_class *core;
  int loc, size;
  Float *line;
  Float xscl, a0, a1, x1;
} fcomb;

static int mus_fcomb_p(mus_any *ptr) {return((ptr) && ((ptr->core)->type == MUS_FCOMB));}

static char *inspect_fcomb(void *ptr) 
{
  fcomb *gen = (fcomb *)ptr;
  char *desc = NULL;
  desc = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  if (desc) 
    mus_snprintf(desc, PRINT_BUFFER_SIZE, "fcomb line[%d at %d], xscl: %f, a0: %f, a1: %f, x1: %f",
	    gen->size, gen->loc, gen->xscl, gen->a0, gen->a1, gen->x1);
  return(desc);
}

static char *describe_fcomb(void *ptr) 
{
  char *desc = NULL;
  fcomb *gen = (fcomb *)ptr;
  desc = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  if (desc)
    {
      if (mus_fcomb_p((mus_any *)ptr))
	mus_snprintf(desc, PRINT_BUFFER_SIZE, "fcomb: scaler: %.3f, a0: %.3f, a1: %.3f, line[%d]",
		gen->xscl, gen->a0, gen->a1, gen->size);
      else mus_snprintf(desc, PRINT_BUFFER_SIZE, "not an fcomb gen");
    }
  return(desc);
}

static int fcomb_equalp(void *p1, void *p2) {return(p1 == p2);}
static int fcomb_length(void *ptr) {return(((fcomb *)ptr)->size);}
static Float *fcomb_data(void *ptr) {return(((fcomb *)ptr)->line);}
static Float fcomb_scaler(void *ptr) {return(((fcomb *)ptr)->xscl);}
static Float set_fcomb_scaler(void *ptr, Float val) {((fcomb *)ptr)->xscl = val; return(val);}
static Float set_fcomb_coeff(void *ptr, Float val) {((fcomb *)ptr)->a1 = val; ((fcomb *)ptr)->a0 = 1.0 - val; return(val);}

static int free_fcomb(void *uptr) 
{
  fcomb *ptr = (fcomb *)uptr;
  if (ptr)
    {
      if (ptr->line) 
	{
	  FREE(ptr->line);
	  ptr->line = NULL;
	}
      FREE(ptr); 
    }
  return(0);
}

static Float mus_fcomb (mus_any *ptr, Float input, Float ignored) 
{
  fcomb *gen = (fcomb *)ptr;
  Float tap_result, filter_result;
  tap_result = gen->line[gen->loc];
  filter_result = (gen->a0 * tap_result) + (gen->a1 * gen->x1);
  gen->x1 = tap_result;
  gen->line[gen->loc] = input + filter_result * gen->xscl;
  gen->loc++;
  if (gen->loc >= gen->size) gen->loc = 0;
  return(tap_result);
}

static mus_any_class FCOMB_CLASS = {
  -1, /* MUS_FCOMB eventually */
  "fcomb",
  &free_fcomb,
  &describe_fcomb,
  &inspect_fcomb,
  &fcomb_equalp,
  &fcomb_data,
  0,
  &fcomb_length,
  0,
  0, 0, 0, 0, /* freq phase */
  &fcomb_scaler,
  &set_fcomb_scaler,
  &mus_fcomb,
  0
};

static mus_any *mus_make_fcomb (Float scaler, int size, Float a0, Float a1)
{
  fcomb *gen = NULL;
  gen = (fcomb *)CALLOC(1, sizeof(fcomb));
  if (gen == NULL) 
    mus_error(MUS_MEMORY_ALLOCATION_FAILED, "can't allocate struct for mus_make_fcomb!");
  else
    {
      gen->core = &FCOMB_CLASS;
      if (MUS_FCOMB == 0) MUS_FCOMB = mus_make_class_tag();
      gen->core->type = MUS_FCOMB;
      gen->loc = 0;
      gen->xscl = scaler;
      gen->x1 = 0.0;
      gen->a0 = a0;
      gen->a1 = a1;
      gen->size = size;
      gen->line = (Float *)CALLOC(size, sizeof(Float));
      if (gen->line == NULL) 
	mus_error(MUS_MEMORY_ALLOCATION_FAILED,
		  "can't allocate %d bytes for fcomb delay line in mus_make_fcomb!",
		  (int)(size * sizeof(Float)));
    }
  return((mus_any *)gen);
}

#if 0
        /* this code not needed */
        #define S_fcomb "fcomb"
        #define S_make_fcomb "make-fcomb"
        #define S_fcomb_p "fcomb?"
        
        static SCM g_fcomb(SCM obj, SCM input)
        {
          #define H_fcomb "(" S_fcomb " gen &optional (val 0.0)) comb filters val with low pass on feeback."
          Float in1 = 0.0;
          ASSERT_TYPE(((mus_scm_p(obj)) && (mus_fcomb_p(TO_CLM(obj)))), obj, SCM_ARG1, S_fcomb, "a comb filter");
          if (NUMBER_P(input)) in1 = TO_C_DOUBLE(input); else ASSERT_TYPE(NOT_BOUND_P(input), input, SCM_ARG2, S_fcomb, "a number");
          return(TO_SCM_DOUBLE(mus_fcomb(TO_CLM(obj), in1, 0.0)));
        }
        
        static SCM g_fcomb_p(SCM obj)
        {
          #define H_fcomb_p "(" S_fcomb_p " gen) -> #t if gen is an fcomb filter, else #f"
          return(TO_SCM_BOOLEAN((mus_scm_p(obj)) && (mus_fcomb_p(TO_CLM(obj)))));
        }
        
        static SCM g_make_fcomb(SCM scaler, SCM size, SCM a0, SCM a1)
        {
          #define H_make_fcomb "(" S_make_fcomb " scaler size a0 a1) -> a new " S_fcomb " (filtered comb) generator"
          mus_scm *gn;
          gn = (mus_scm *)CALLOC(1, sizeof(mus_scm));
          gn->gen = mus_make_fcomb(TO_C_DOUBLE(scaler), TO_SMALL_C_INT(size), TO_C_DOUBLE(a0), TO_C_DOUBLE(a1));
          gn->nvcts = 0;
          return(mus_scm_to_smob(gn));
        }
        
        static void init_fcomb(void)
        {
          SCM local_doc;
          local_doc = MAKE_PERMANENT(DOCUMENTATION);
          DEFINE_PROC(S_fcomb_p, g_fcomb_p, 1, 0, 0, H_fcomb_p);
          DEFINE_PROC(S_make_fcomb, g_make_fcomb, 4, 0, 0, H_make_fcomb);
          DEFINE_PROC(S_fcomb, g_fcomb, 2, 0, 0, H_fcomb);
        }
#endif


/* ---------------- reverbs ---------------- */
/*
 * call sequence is a bit convoluted because we want two internal reverb choices + user-defined SCM func choices,
 *   freeverb wants multi-channel inputs,
 *   g_calln is not fast enough to depend on in real time (where we could be running reverb+expand+src+filter)
 * so, wherever possible we call direct, but SCM backups exist as well (for possible explicit call)
 */

static void *global_rev = NULL;

typedef struct {
  int num_combs;
  mus_any **combs;
  int num_allpasses;
  mus_any **allpasses;
  mus_any *onep;
  int chan_combs, chan_allpasses;
  mus_any **predelays;
  int num_predelays;
} rev_info;

static void nrev(void *ur, Float *rins, Float *routs, int chans)
{
  rev_info *r = (rev_info *)ur;
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

static void freeverb(void *ur, Float *rins, Float *routs, int chans)
{
  rev_info *r = (rev_info *)ur;
  int i, j, k = 0, m = 0;
  for (i = 0; i < chans; i++)
    {
      rins[i] = mus_delay(r->predelays[i], rins[i], 0.0);
      routs[i] = 0.0;
      for (j = 0; j < r->chan_combs; j++)
	routs[i] += mus_fcomb(r->combs[k++], rins[i], 0.0);
      for (j = 0; j < r->chan_allpasses; j++)
	routs[i] = mus_all_pass(r->allpasses[m++], routs[i], 0.0);
    }
}

#include "vct.h"
#include "clm2scm.h"

static SCM g_nrev(SCM ptr, SCM invals, SCM outvals)
{
  vct *inp, *outp;
  inp = TO_VCT(invals);
  outp = TO_VCT(outvals);
  nrev((void *)SND_UNWRAP(ptr), inp->data, outp->data, outp->length);
  return(outvals);
}

static SCM g_freeverb(SCM ptr, SCM invals, SCM outvals)
{
  /* actually just a place-holder */
  vct *inp, *outp;
  inp = TO_VCT(invals);
  outp = TO_VCT(outvals);
  freeverb((void *)SND_UNWRAP(ptr), inp->data, outp->data, outp->length);
  return(outvals);
}

static SCM v_ins, v_outs;

static Float *r_ins, *r_outs;
static void free_reverb(void *ur);
static int reverb_chans = 0;

static void reverb(void *ur, Float **rins, MUS_SAMPLE_TYPE **outs, int ind)
{
  int i, chans;
  chans = reverb_chans;
  for (i = 0; i < chans; i++)
    {
      r_ins[i] = rins[i][ind];
      r_outs[i] = 0.0;
    }
  switch (which_reverb)
    {
    case NREVERB: 
      nrev(ur, r_ins, r_outs, chans); 
      break;
    case FREEVERB: 
      freeverb(ur, r_ins, r_outs, chans); 
      break;
    case USERVERB:
      {
	SCM res;
	if (!ur) return;
	res = CALL3(g_reverb, (SCM)ur, v_ins, v_outs, __FUNCTION__);
	if (!(VCT_P(res)))
	  {
	    stop_playing_all_sounds();
	    free_reverb(global_rev);
	    snd_warning("play stopped due to reverb error");
	  }
      }
      break;
    }
  for (i = 0; i < chans; i++)
    outs[i][ind] += MUS_FLOAT_TO_SAMPLE(r_outs[i]);
}


static void free_rev(void) 
{
  int i;
  rev_info *r;
  r = (rev_info *)global_rev;
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
      if (r->predelays)
	{
	  for (i = 0; i < r->num_predelays; i++)
	    if (r->predelays[i])
	      mus_free(r->predelays[i]);
	  FREE(r->predelays);
	}
      FREE(r);
    }
}

static SCM g_free_rev(SCM ptr) 
{
  free_rev();
  return(SCM_BOOL_F);
}

static void free_reverb(void *ur)
{
  switch (which_reverb)
    {
    case NREVERB: 
    case FREEVERB: 
      free_rev(); 
      break;
    case USERVERB:
      CALL1(g_free_reverb, (SCM)ur, "free-reverb");
      break;
    }
  global_rev = NULL;
}

static Float *comb_factors = NULL;

static void *make_nrev(snd_info *sp, int chans) 
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
  r->predelays = NULL;
  r->num_predelays = 0;
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
  return((void *)r);
}


static void *make_freeverb(snd_info *sp, int chans) 
{
  int freeverb_stereo_spread = 23;
  #define FREEVERB_COMBS 8
  static int freeverb_comb_tuning[FREEVERB_COMBS] = {1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617};
  #define FREEVERB_ALLPASSES 4
  static int freeverb_allpass_tuning[FREEVERB_ALLPASSES] = {556, 441, 341, 225};
  Float freeverb_room_decay = 0.5;
  Float freeverb_damping = 0.5;
  Float freeverb_predelay = 0.03;
  Float freeverb_scale_damping = 0.4;
  Float freeverb_scale_room_decay = 0.28;
  Float freeverb_offset_room_decay = 0.7;
  int i, j, k, delay_len;
  rev_info *r;
  Float srscale, fcmb;
  if (sp == NULL) return(NULL);
  srscale = sp->reverb_control_length * SND_SRATE(sp) / 44100.0;
  r = (rev_info *)CALLOC(1, sizeof(rev_info));
  r->num_predelays = chans;
  r->predelays = (mus_any **)CALLOC(chans, sizeof(mus_any *));
  delay_len = (int)(SND_SRATE(sp) * freeverb_predelay);
  for (i = 0; i < chans; i++)
    r->predelays[i] = mus_make_delay(delay_len, NULL, delay_len);
  r->chan_combs = FREEVERB_COMBS;
  r->chan_allpasses = FREEVERB_ALLPASSES;
  r->num_combs = r->chan_combs * chans;
  r->num_allpasses = r->chan_allpasses * chans;
  r->combs = (mus_any **)CALLOC(r->num_combs, sizeof(mus_any *));
  r->allpasses = (mus_any **)CALLOC(r->num_allpasses, sizeof(mus_any *));
  if (comb_factors) FREE(comb_factors);
  comb_factors = (Float *)CALLOC(r->num_combs, sizeof(Float));
  k = 0;
  for (i = 0; i < chans; i++)
    for (j = 0; j < r->chan_combs; j++)
      {
	delay_len = (int)(srscale * freeverb_comb_tuning[j]);
	if (i & 1) 
	  delay_len += (int)(srscale * freeverb_stereo_spread);
	fcmb = (freeverb_scale_damping * freeverb_damping) * sp->reverb_control_lowpass / DEFAULT_REVERB_CONTROL_LOWPASS;
	comb_factors[k] = (freeverb_room_decay * freeverb_scale_room_decay + freeverb_offset_room_decay) * 
	                  sp->reverb_control_feedback / DEFAULT_REVERB_CONTROL_FEEDBACK;
	r->combs[k] = mus_make_fcomb(comb_factors[k], delay_len, 1.0 - fcmb, fcmb);
	k++;
      }
  k = 0;
  for (i = 0; i < chans; i++)
    for (j = 0; j < r->chan_allpasses; j++)
      {
	delay_len = (int)(srscale * freeverb_allpass_tuning[j]);
	if (i&1) delay_len += (int)(srscale * freeverb_stereo_spread);
	r->allpasses[k] = mus_make_all_pass(0.5, -1.0, delay_len, NULL, delay_len);
	k++;
      }
  return((void *)r);
}

static snd_info *ind2sp(int i)
{
  snd_state *ss;
  ss = get_global_state();
  return(ss->sounds[i]);
}

static SCM g_make_nrev(SCM ind, SCM chns) 
{
  return(SND_WRAP(make_nrev(ind2sp(TO_SMALL_C_INT(ind)),
			    TO_SMALL_C_INT(chns))));
}

static SCM g_make_freeverb(SCM ind, SCM chns) 
{
  return(SND_WRAP(make_freeverb(ind2sp(TO_SMALL_C_INT(ind)),
				TO_SMALL_C_INT(chns))));
}

static void *make_reverb(snd_info *sp, int chans)
{ 
  reverb_chans = chans;
  switch (which_reverb)
    {
    case NREVERB: 
      global_rev = make_nrev(sp, chans);
      break;
    case FREEVERB:
      global_rev = make_freeverb(sp, chans);
      break;
    case USERVERB:
      global_rev = (void *)CALL2(g_make_reverb,
				 TO_SMALL_SCM_INT(sp->index),
				 TO_SMALL_SCM_INT(chans),
				 __FUNCTION__);
      if (SYMBOL_P((SCM)global_rev))
	{
	  report_in_minibuffer(sp, "make-reverb unhappy?");
	  global_rev = NULL;
	}
      break;
    }
  return(global_rev);
}


/* -------- contrast-enhancement -------- */

static Float contrast (dac_info *dp, Float amp, Float index, Float inval)
{
  if (use_g_contrast)
    return(amp * TO_C_DOUBLE(CALL2(g_contrast,
				   TO_SCM_DOUBLE(dp->contrast_amp * inval),
				   TO_SCM_DOUBLE(index),
				   __FUNCTION__)));
  return(amp * mus_contrast_enhancement(dp->contrast_amp * inval, index));
}

static SCM g_mus_contrast(SCM inval, SCM index)
{
#ifdef SCM_REAL_VALUE
  return(TO_SCM_DOUBLE(mus_contrast_enhancement(SCM_REAL_VALUE(inval), SCM_REAL_VALUE(index))));
#else
  return(TO_SCM_DOUBLE(mus_contrast_enhancement(TO_C_DOUBLE(inval), TO_C_DOUBLE(index))));
#endif
}

static void init_rev_funcs(SCM local_doc)
{
  g_make_reverb = SCM_BOOL_F;
  g_reverb = SCM_BOOL_F;
  g_free_reverb = SCM_BOOL_F;
  g_contrast = SCM_BOOL_F;
  v_ins = SCM_BOOL_F;
  v_outs = SCM_BOOL_F;
  DEFINE_PROC("make-snd-nrev",     g_make_nrev, 2, 0, 0,     "make-snd-nrev is the default reverb make function");
  DEFINE_PROC("snd-nrev",          g_nrev, 3, 0, 0,          "snd-nrev is the default reverb");
  DEFINE_PROC("free-snd-nrev",     g_free_rev, 1, 0, 0,      "free-snd-nrev is the default reverb free function");
  DEFINE_PROC("snd-contrast",      g_mus_contrast, 2, 0, 0,  "snd-contrast is the default contrast function");
  DEFINE_PROC("make-snd-freeverb", g_make_freeverb, 2, 0, 0, "make-snd-freeverb is the freeverb reverb make function");
  DEFINE_PROC("snd-freeverb",      g_freeverb, 3, 0, 0,      "snd-freeverb is the freeverb reverb");
  DEFINE_PROC("free-snd-freeverb", g_free_rev, 1, 0, 0,      "free-snd-freeverb is the freeverb reverb free function");
}


static void set_nrev_filter_coeff(Float newval)
{
  rev_info *r;
  r = (rev_info *)global_rev;
  mus_set_a0(r->onep, newval);
  mus_set_b1(r->onep, 1.0 - newval);
}

static void set_freeverb_filter_coeff(Float newval)
{
  int j;
  rev_info *r;
  r = (rev_info *)global_rev;
  if (r)
    {
      for (j = 0; j < r->num_combs; j++)
	set_fcomb_coeff(r->combs[j], newval / DEFAULT_REVERB_CONTROL_LOWPASS);
    }
}

static void set_reverb_filter_coeff(Float newval)
{
  switch (which_reverb)
    {
    case NREVERB: set_nrev_filter_coeff(newval); break;
    case FREEVERB: set_freeverb_filter_coeff(newval); break;
    default: break;
    }
}

static void set_reverb_comb_factors(Float newval)
{
  rev_info *r;
  int j;
  Float val;
  if (which_reverb != USERVERB)
    {
      r = (rev_info *)global_rev;
      if (which_reverb == FREEVERB)
	val = newval / DEFAULT_REVERB_CONTROL_FEEDBACK;
      else val = newval;
      if (r)
	{
	  for (j = 0; j < r->num_combs; j++)
	    mus_set_scaler(r->combs[j], comb_factors[j] * val);
	}
    }
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
	dp->src = make_src(sp->state, 0.0, fd);
      /* that is, if user wants fancy src, he needs to say so before we start */
      if (dp->expanding) 
	{
	  dp->spd = (spd_info *)make_expand(sp, (Float)SND_SRATE(sp), sp->expand_control, dp);
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
#define INITIAL_MAX_SOUNDS 32
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

#define MAX_DEVICES 8
static int dev_fd[MAX_DEVICES];

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
  dac_running = 0;
}

static void reflect_play_stop (snd_info *sp) 
{
  if (w_snd_play(sp)) set_toggle_button(w_snd_play(sp), FALSE, FALSE, sp);
  set_file_browser_play_button(sp->shortname, 0);
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
      if (sp->cursor_follows_play != DONT_FOLLOW)
	handle_cursor(cp, cursor_moveto(cp, cp->original_cursor));
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
      if (HOOKED(stop_playing_region_hook))
	g_c_run_or_hook(stop_playing_region_hook,
			SCM_LIST1(TO_SMALL_SCM_INT(dp->region)),
			S_stop_playing_region_hook);
    }
  else
    {
      if (sp_stopping)
	{
	  if (HOOKED(stop_playing_hook))
	    g_c_run_or_hook(stop_playing_hook,
			    SCM_LIST1(TO_SMALL_SCM_INT(sp->index)),
			    S_stop_playing_hook);
	}
      if (HOOKED(stop_playing_channel_hook))
	g_c_run_or_hook(stop_playing_channel_hook,
			SCM_LIST2(TO_SMALL_SCM_INT(sp->index),
				  TO_SMALL_SCM_INT(cp->chan)),
			S_stop_playing_channel_hook);
      if (sp->index < 0) {free_player(sp); sp = NULL;}
    }
  free_dac_info(dp);
  if ((sp) && (sp_stopping) && (sp->delete_me)) 
    completely_free_snd_info(sp); /* dummy snd_info struct for (play "filename") in snd-scm.c */
}

static void stop_playing(dac_info *dp) {stop_playing_with_toggle(dp, TRUE);}

static void stop_playing_sound_with_toggle(snd_info *sp, int toggle)
{
  /* this needs to scan all current play_list members and remove any that are referring
   * to sp, even indirectly (as through the current selection)
   */
  int i;
  if ((sp) && (play_list))
    for (i = 0; i <= max_active_slot; i++)
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
    for (i = 0; i <= max_active_slot; i++)
      {
	stop_playing(play_list[i]);
	play_list[i] = NULL;
      }
}

void stop_playing_region(int n)
{
  int i;
  if (play_list)
    for (i = 0; i <= max_active_slot; i++)
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
      dp->cur_amp = sp->amp_control;
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

static char *describe_dac(int error_type)
{
  /* TODO: pass back the relevant dac state given the error indication */
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
    return(ptr->sp->shortname);
  return("");
}


static BACKGROUND_TYPE dac_in_background(GUI_POINTER ptr);

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
	    global_rev = (void *)make_reverb(dp->sp, channels);
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
      snd_dacp->channels = channels;
      snd_dacp->frames = 256; /* just a first guess */
      snd_dacp->devices = 1;  /* just a first guess */
      snd_dacp->reverb_ring_frames = (int)(srate * reverb_control_decay(ss));
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
    }
}


static dac_info *add_channel_to_play_list(chan_info *cp, snd_info *sp, int start, int end, SCM edpos, const char *caller, int arg_pos)
{
  /* if not sp, control panel is ignored */
  int slot, beg = 0, direction = READ_FORWARD;
  slot = find_slot_to_play();
  if (slot == -1) return(NULL);
  play_list_members++;
  if (sp)
    {
      sp->playing++;
      if (sp->cursor_follows_play != DONT_FOLLOW)
	{
	  cp->original_cursor = cp->cursor;
	  handle_cursor(cp, cursor_moveto(cp, start));
	}
      if (sp->speed_control_direction == 1) 
	{
	  direction = READ_FORWARD; 
	  beg = start;
	}
      else 
	{
	  direction = READ_BACKWARD;
	  if (start == 0) 
	    beg = to_c_edit_samples(cp, edpos, caller, arg_pos) - 1;
	  else beg = start;
	}
    }
  return(init_dp(slot, cp, sp, 
		 init_sample_read_any(beg, cp, direction, 
				      to_c_edit_position(cp, edpos, caller, 4)),
		 start, end));
}

static dac_info *add_region_channel_to_play_list(int region, int chan, int beg, int end)
{
  int slot;
  snd_fd *fd;
  slot = find_slot_to_play();
  if (slot == -1) return(NULL);
  play_list_members++;
  fd = init_region_read(get_global_state(), beg, region, chan, READ_FORWARD);
  return(init_dp(slot, fd->cp, NULL, fd, beg, end));
}

void play_region(snd_state *ss, int region, int background)
{
  /* just plays region (not current selection) -- no control panel etc */
  int chans, i;
  dac_info *dp;
  if ((background == NOT_IN_BACKGROUND) && (play_list_members > 0)) return;
  if (!(region_ok(region))) return;
  chans = region_chans(region);
  if (chans == 0) return;
  for (i = 0; i < chans; i++) 
    {
      dp = add_region_channel_to_play_list(region, i, 0, NO_END_SPECIFIED);
      if (dp) dp->region = region;
    }
  start_dac(ss, region_srate(region), chans, background);
}

void play_channel(chan_info *cp, int start, int end, int background, SCM edpos, const char *caller, int arg_pos)
{
  /* just plays one channel (ignores possible sync) */
  snd_info *sp = NULL;
  dac_info *dp;
  if ((background == NOT_IN_BACKGROUND) && (play_list_members > 0)) return;
  sp = cp->sound;
  if (!(sp->inuse)) return;
  dp = add_channel_to_play_list(cp, sp, start, end, edpos, caller, arg_pos);
  start_dac(dp->ss, SND_SRATE(sp), 1, background);
}

void play_sound(snd_info *sp, int start, int end, int background, SCM edpos, const char *caller, int arg_pos)
{
  /* just plays one sound (ignores possible sync) */
  int i;
  if ((background == NOT_IN_BACKGROUND) && 
      (play_list_members > 0)) 
    return;
  if (!(sp->inuse)) return;
  if ((HOOKED(start_playing_hook)) &&
      (TRUE_P(g_c_run_or_hook(start_playing_hook,
				  SCM_LIST1(TO_SMALL_SCM_INT(sp->index)),
				  S_start_playing_hook))))
    {
      reflect_play_stop(sp);           /* turns off buttons */
      if (sp->delete_me) 
	completely_free_snd_info(sp);  /* dummy snd_info struct for (play "filename") in snd-scm.c */
      return;
    }
  for (i = 0; i < sp->nchans; i++) 
    add_channel_to_play_list(sp->chans[i], sp, start, end, edpos, caller, arg_pos);
  start_dac(sp->state, SND_SRATE(sp), sp->nchans, background);
}

void play_channels(chan_info **cps, int chans, int *starts, int *ur_ends, int background, SCM edpos, const char *caller, int arg_pos)
{
  /* ends can be NULL */
  int i;
  snd_info *sp = NULL;
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
    add_channel_to_play_list(cps[i], 
			     sp = (cps[i]->sound), 
			     starts[i], ends[i],
			     edpos, caller, arg_pos);
  if (ur_ends == NULL) FREE(ends);
  if (sp) start_dac(sp->state, SND_SRATE(sp), chans, background);
}

void play_selection(int background, SCM edpos, const char *caller, int arg_pos)
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
		ends[i] = si->begs[i] + (int)(((Float)selection_len() / (Float)(sp->speed_control))); /* TODO this should use the src->sample counter instead */
	      else ends[i] = si->begs[i] + selection_len();
	    }
	  play_channels(si->cps, si->chans, si->begs, ends, background, edpos, caller, arg_pos);
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
	  if ((sp->amp_control == dp->cur_amp) && (sp->amp_control == 1.0))
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

static int fill_dac_buffers(dac_state *dacp, int write_ok)
{
  int i, j, cursor_change;
  int bytes, frames;
  Float *revin;
  Float amp, incr, sr, sincr, ind, indincr, ex, exincr, rev, revincr, fval;
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
  clear_dac_buffers(dacp);

  if (dac_pausing) 
    cursor_change = 0;
  else
    {
      if (HOOKED(play_hook))
	g_c_run_progn_hook(play_hook, 
			   SCM_LIST1(TO_SCM_INT(frames)),
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
		  (dp->chn_fd->eof == 0) &&
		  (dp->chn_fd->cb))
		handle_cursor(dp->cp, cursor_moveto(dp->cp, current_location(dp->chn_fd)));

	      /* add a buffer's worth from the current source into dp->audio_chan */
	      buf = dac_buffers[dp->audio_chan];
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
		  /* sp->amp_control is current UI value, dp->cur_amp is current local value */
		  amp = dp->cur_amp;
		  incr = (sp->amp_control - amp) / (Float)(frames);
		  for (j = 0; j < frames; j++, amp += incr) 
		    buf[j] += MUS_FLOAT_TO_SAMPLE(next_sample_to_float(dp->chn_fd) * amp);
		  dp->cur_amp = amp;
		  break;

		case JUST_SPEED:
		  /* includes amp changes */
		  /* sp->speed_control is current UI value, dp->cur_srate is current local value */
		  dp->never_sped = 0;
		  amp = dp->cur_amp;
		  incr = (sp->amp_control - amp) / (Float)(frames);
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
		  incr = (sp->amp_control - amp) / (Float)(frames);
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
		  free_reverb(global_rev);
		}
	    }
	}
    }

  /* now parcel these buffers out to the available devices */
#if (HAVE_OSS || HAVE_ALSA)
  if (write_ok == WRITE_TO_DAC) 
    {
      dev_bufs = dac_buffers;
      for (i = 0; i < dacp->devices; i++)
	if (dev_fd[i] != -1)
	  {
	    mus_file_write_buffer(dacp->out_format,
				  0, frames-1,
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

static void dac_error(const char *file, int line, const char *function)
{
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
      if (!(VCT_P(v_ins)))
	{
	  v_ins = make_vct_wrapper(dacp->channels, r_ins);
	  v_outs = make_vct_wrapper(dacp->channels, r_outs);
	  snd_protect(v_ins);
	  snd_protect(v_outs);
	}
      else
	{
	  vct *v;
	  v = TO_VCT(v_ins);
	  v->data = r_ins;
	  v = TO_VCT(v_outs);
	  v->data = r_outs;
	}
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

int mus_audio_compatible_format(int dev) 
{
#ifndef PPC
  int err, i;
  float val[32];
  int ival[32];
  err = mus_audio_mixer_read(dev, MUS_AUDIO_FORMAT, 32, val);
  if (err != MUS_ERROR)
    {
      for (i = 0; i <=(int)(val[0]); i++) ival[i] = (int)(val[i]);
      /*          ^ this cast is vital!  Memory clobbered otherwise in LinuxPPC */
      for (i = 1; i <= ival[0]; i++)
	if (ival[i] == MUS_COMPATIBLE_FORMAT) 
	  return(MUS_COMPATIBLE_FORMAT);
      for (i = 1; i <= ival[0]; i++) 
	if ((ival[i] == MUS_BINT) || (ival[i] == MUS_LINT) ||
	    (ival[i] == MUS_BFLOAT) || (ival[i] == MUS_LFLOAT) ||
	    (ival[i] == MUS_BSHORT) || (ival[i] == MUS_LSHORT))
	  return(ival[i]);
      for (i = 1; i <= ival[0]; i++) 
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
      for (card = 0; card < cards; card++) 
	{
	  if ((err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(card), 
					  MUS_AUDIO_PORT, 
					  ALSA_MAX_DEVICES, val)) != MUS_NO_ERROR) 
	    {
	      snd_error("%s[%d] %s: mus_audio_mixer_read", 
			__FILE__, __LINE__, __FUNCTION__);
	    }
	  devs = (int)(val[0]);
	  /* scan all devices in the card */
	  for (d = 0; d < devs; d++) 
	    {
	      dev = (int)(val[d+1]);
	      if ((err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(card) | dev, 
					      MUS_AUDIO_DIRECTION, 
					      0, 
					      &direction)) != MUS_NO_ERROR) 
		{
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
	      unlock_recording_audio();
	      if (global_rev) free_reverb(global_rev);
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
		global_rev = (void *)make_reverb(dp->sp, dacp->channels);

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
   unlock_recording_audio();
   dac_pausing = 0;
   if (global_rev) free_reverb(global_rev);
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

void initialize_apply(snd_info *sp, int chans, int dur)
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
      play_sound(sp, 0, dur, IN_BACKGROUND, TO_SCM_INT(AT_CURRENT_EDIT_POSITION), S_apply_controls, 0); 
      break;
    case APPLY_TO_SELECTION: 
      play_selection(IN_BACKGROUND, TO_SCM_INT(AT_CURRENT_EDIT_POSITION), S_apply_controls, 0); 
      break;
    case APPLY_TO_CHANNEL: 
      if (sp->selected_channel != NO_SELECTION)
	curchan = sp->selected_channel;
      play_channel(sp->chans[curchan], 0, dur, IN_BACKGROUND, TO_SCM_INT(AT_CURRENT_EDIT_POSITION), S_apply_controls, 0); 
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
  if (snd_dacp) free_dac_state();
  if (global_rev) free_reverb(global_rev);
}

int run_apply(int ofd)
{
  int len;
  len = fill_dac_buffers(snd_dacp, WRITE_TO_FILE);
  mus_file_write(ofd, 0, len - 1, snd_dacp->channels, dac_buffers);
  return(len);
}


/* -------------------------------- scheme connection -------------------------------- */

static SCM g_play_1(SCM samp_n, SCM snd_n, SCM chn_n, int background, int syncd, SCM end_n, SCM edpos, const char *caller, int arg_pos) 
{
  /* all chans if chn_n omitted, arbitrary file if snd_n is name */
  snd_info *sp;
  chan_info *cp;
  sync_info *si = NULL;
  char *name = NULL;
  int i, samp = 0;
  int end = NO_END_SPECIFIED;
  int *ends = NULL;
  if (INTEGER_P(end_n)) end = TO_C_INT(end_n);
#if USE_NO_GUI
  background = 0;
#endif

  /* if even samp_n is SCM_UNDEFINED, start_dac? */

  if (STRING_P(samp_n))
    {
      /* filename beg end background syncd ignored */
      name = mus_expand_filename(TO_C_STRING(samp_n));
      if (!(mus_file_probe(name)))
	{
	  FREE(name);
	  return(snd_no_such_file_error(caller, samp_n));
	}
      if (!(MUS_HEADER_TYPE_OK(mus_sound_header_type(name))))
	{
	  FREE(name);
	  mus_misc_error(caller, "can't read header", 
			 SCM_LIST2(samp_n, 
				   TO_SCM_STRING(mus_header_type_name(mus_header_type()))));
	}
      if (!(MUS_DATA_FORMAT_OK(mus_sound_data_format(name))))
	{
	  FREE(name);
	  mus_misc_error(caller, "can't read data", 
			 SCM_LIST2(samp_n, 
				   TO_SCM_STRING(mus_header_original_format_name(mus_sound_original_format(name),
										 mus_sound_header_type(name)))));
	}
      sp = make_sound_readable(get_global_state(), name, FALSE);
      sp->shortname = filename_without_home_directory(name);
      sp->fullname = NULL;
      sp->delete_me = 1;
      samp = TO_C_INT_OR_ELSE(snd_n, 0);
      if (INTEGER_P(chn_n)) end = TO_C_INT(chn_n);
      play_sound(sp, samp, end, background, TO_SMALL_SCM_INT(0), caller, arg_pos);
      if (name) FREE(name);
    }
  else
    {
      ASSERT_TYPE(NUMBER_IF_BOUND_P(samp_n), samp_n, SCM_ARG1, caller, "a number");
      SND_ASSERT_CHAN(caller, snd_n, chn_n, 2);
      sp = get_sp(snd_n);
      if (sp == NULL) 
	return(snd_no_such_sound_error(caller, snd_n));
      samp = TO_C_INT_OR_ELSE(samp_n, 0);
      if ((syncd) && (sp->sync != 0))
	{
	  si = snd_sync(sp->state, sp->sync);
	  if (end != NO_END_SPECIFIED)
	    {
	      ends = (int *)CALLOC(si->chans, sizeof(int));
	      for (i = 0; i < si->chans; i++) ends[i] = end;
	    }
	  play_channels(si->cps, si->chans, si->begs, ends, background, edpos, caller, arg_pos);
	  si = free_sync_info(si);
	  FREE(ends);
	}
      else
	{
	  if (!(INTEGER_P(chn_n)))
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
  return(SCM_BOOL_T);
}

#define TO_C_BOOLEAN_OR_F(a) ((TRUE_P(a) || ((INTEGER_P(a)) && (TO_SMALL_C_INT(a) == 1))) ? 1 : 0)

static SCM g_play(SCM samp_n, SCM snd_n, SCM chn_n, SCM syncd, SCM end_n, SCM edpos) 
{
  #define H_play "(" S_play " &optional (start 0) snd chn sync end pos) plays snd or snd's channel chn starting at start. \
'start' can also be a filename: (" S_play " \"oboe.snd\").  If 'sync' is true, all sounds syncd to snd are played. \
if 'end' is not given, it plays to the end of the sound.  If 'pos' is -1 or not given, the current edit position is \
played."

  return(g_play_1(samp_n, snd_n, chn_n, TRUE, 
		  TO_C_BOOLEAN_OR_F(syncd), end_n,
		  edpos, S_play, 6));
}

static SCM g_play_selection(SCM wait, SCM edpos) 
{
  #define H_play_selection "(" S_play_selection " &optional (wait #f) pos) plays the current selection"
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(wait), wait, SCM_ARG1, S_play_selection, "a boolean");
  if (selection_is_active())
    {
      play_selection(!(TO_C_BOOLEAN_OR_F(wait)), edpos, S_play_selection, 2);
      return(SCM_BOOL_T);
    }
  snd_no_active_selection_error(S_play_selection);
  return(wait);
}

static SCM g_play_and_wait(SCM samp_n, SCM snd_n, SCM chn_n, SCM syncd, SCM end_n, SCM edpos) 
{
  #define H_play_and_wait "(" S_play_and_wait " &optional (start 0) snd chn end pos) plays snd or snd's channel chn starting at start \
and waiting for the play to complete before returning.  'start' can also be a filename: (" S_play_and_wait " \"oboe.snd\")"

  return(g_play_1(samp_n, snd_n, chn_n, FALSE, 
		  TO_C_BOOLEAN_OR_F(syncd), end_n, 
		  edpos, S_play_and_wait, 6));
}

static SCM g_stop_playing(SCM snd_n)
{
  #define H_stop_playing "(" S_stop_playing " &optional snd) stops play in progress"
  snd_info *sp = NULL;
  ASSERT_TYPE(INTEGER_IF_BOUND_P(snd_n), snd_n, SCM_ARG1, S_stop_playing, "an integer");
  if (INTEGER_P(snd_n)) sp = get_sp(snd_n);
  if (sp) 
    stop_playing_sound(sp); 
  else stop_playing_all_sounds();
  return(SCM_BOOL_F);
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
  FREE(sp->fullname);
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

static SCM snd_no_such_player_error(const char *caller, SCM index)
{
  ERROR(NO_SUCH_PLAYER,
	SCM_LIST2(TO_SCM_STRING(caller),
		  index));
  return(SCM_BOOL_F);
}

static SCM g_make_player(SCM snd, SCM chn)
{
  #define H_make_player "(" S_make_player " &optional snd chn) prepares snd's channel chn for " S_add_player
  snd_info *true_sp, *new_sp;
  chan_info *cp;
  SND_ASSERT_CHAN(S_make_player, snd, chn, 1);
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
      return(TO_SCM_INT(make_player(new_sp, cp)));
    }
  return(snd_no_such_channel_error(S_make_player, snd, chn));  
}

static SCM g_player_home(SCM snd_chn)
{
  #define H_player_home "(" S_player_home " player) returns a list of the sound index and channel number associated with player"
  int index;
  chan_info *cp;
  ASSERT_TYPE(INTEGER_P(snd_chn), snd_chn, SCM_ARG1, S_player_home, "an integer");
  index = -TO_SMALL_C_INT(snd_chn);
  if ((index > 0) && 
      (index < players_size) && 
      (players[index]) &&
      (players[index]->chans) &&
      (player_chans[index] < players[index]->nchans))
    {
      cp = players[index]->chans[player_chans[index]]; /* trying to get back to the original sound index (not the player index) */
      if ((cp->sound) && (cp->sound->active))
	return(SCM_LIST2(TO_SMALL_SCM_INT(cp->sound->index),
			 TO_SMALL_SCM_INT(cp->chan)));
      else return(SCM_LIST2(NO_SUCH_SOUND,
			    TO_SMALL_SCM_INT(cp->chan)));
    }
  return(snd_no_such_player_error(S_player_home, snd_chn));
}

static SCM g_add_player(SCM snd_chn, SCM start, SCM end, SCM edpos)
{
  #define H_add_player "(" S_add_player " &optional player start end pos) starts playing snd's channel chn"
  snd_info *sp = NULL;
  chan_info *cp;
  int index;
  ASSERT_TYPE(INTEGER_P(snd_chn), snd_chn, SCM_ARG1, S_add_player, "an integer");
  ASSERT_TYPE(NUMBER_IF_BOUND_P(start), start, SCM_ARG2, S_add_player, "a number");
  ASSERT_TYPE(NUMBER_IF_BOUND_P(end), end, SCM_ARG3, S_add_player, "a number");
  index = -TO_SMALL_C_INT(snd_chn);
  if ((index > 0) && (index < players_size)) sp = players[index];
  if (sp)
    {
      cp = sp->chans[player_chans[index]];
      add_channel_to_play_list(cp, sp,
			       TO_C_INT_OR_ELSE(start, 0),
			       TO_C_INT_OR_ELSE(end, NO_END_SPECIFIED),
			       edpos,
			       S_add_player,
			       4);
    }
  else snd_no_such_player_error(S_add_player, snd_chn);
  return(snd_chn);
}

static SCM g_start_playing(SCM Chans, SCM Srate, SCM In_Background)
{

  /* need some way to distinguish SCM from C vars that represent the same thing -- trying Caps here as an experiment */

  #define H_start_playing "(" S_start_playing " &optional chans srate in-background)"
  int chans, srate;
  ASSERT_TYPE(INTEGER_IF_BOUND_P(Chans), Chans, SCM_ARG1, S_start_playing, "an integer");
  ASSERT_TYPE(NUMBER_IF_BOUND_P(Srate), Srate, SCM_ARG2, S_start_playing, "a number");
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(In_Background), In_Background, SCM_ARG3, S_start_playing, "a boolean");
  chans = TO_C_INT_OR_ELSE(Chans, 1);
  if (chans <= 0)
    mus_misc_error(S_start_playing, "invalid chans arg", Chans);
  srate = TO_C_INT_OR_ELSE(Srate, 44100);
  if (srate <= 0)
    mus_misc_error(S_start_playing, "invalid srate arg", Srate);
  start_dac(get_global_state(), srate, chans, TO_C_BOOLEAN_OR_T(In_Background));
  return(SCM_BOOL_F);
}

static SCM g_stop_player(SCM snd_chn)
{
  #define H_stop_player "(" S_stop_player " player) stops player"
  int index;
  snd_info *sp = NULL;
  ASSERT_TYPE(INTEGER_P(snd_chn), snd_chn, SCM_ARG1, S_stop_player, "an integer");
  index = -TO_SMALL_C_INT(snd_chn);
  if ((index > 0) && (index < players_size)) sp = players[index];
  if (sp) 
    stop_playing_sound(sp);
  else snd_no_such_player_error(S_stop_player, snd_chn);
  return(snd_chn);
}

/* also the dac filler needs to run on empty buffers in this case? */

static SCM g_player_p(SCM snd_chn)
{
  #define H_player_p "(" S_player_p " obj) -> is obj an active player"
  int index;
  ASSERT_TYPE(INTEGER_P(snd_chn), snd_chn, SCM_ARG1, S_player_p, "an integer");
  index = -TO_SMALL_C_INT(snd_chn);
  return(TO_SCM_BOOLEAN((index > 0) && 
			(index < players_size) && 
			(players[index])));
}

void g_init_dac(SCM local_doc)
{
  DEFINE_PROC(S_reverb_control_procedures, g_reverb_procedures, 0, 0, 0, H_reverb_control_procedures);
  DEFINE_PROC("set-" S_reverb_control_procedures, g_set_reverb_procedures, 3, 0, 0, H_set_reverb_control_procedures);
  /* can't use generalized set here because it's confused by the 3 args -- perhaps a list would be ok */

  define_procedure_with_setter(S_contrast_control_procedure, SCM_FNC g_contrast_procedure, H_contrast_control_procedure,
			       "set-" S_contrast_control_procedure, SCM_FNC g_set_contrast_procedure, local_doc, 0, 0, 1, 0);

  DEFINE_PROC(S_play,           g_play, 0, 6, 0,           H_play);
  DEFINE_PROC(S_play_selection, g_play_selection, 0, 2, 0, H_play_selection);
  DEFINE_PROC(S_play_and_wait,  g_play_and_wait, 0, 6, 0,  H_play_and_wait);
  DEFINE_PROC(S_stop_playing,   g_stop_playing, 0, 1, 0,   H_stop_playing);

  DEFINE_PROC(S_make_player,    g_make_player, 0, 2, 0,    H_make_player);
  DEFINE_PROC(S_add_player,     g_add_player, 1, 3, 0,     H_add_player);
  DEFINE_PROC(S_player_home,    g_player_home, 1, 0, 0,    H_player_home);
  DEFINE_PROC(S_start_playing,  g_start_playing, 0, 3, 0,  H_start_playing);
  DEFINE_PROC(S_stop_player,    g_stop_player, 1, 0, 0,    H_stop_player);
  DEFINE_PROC(S_player_p,       g_player_p, 1, 0, 0,       H_player_p);

  #define H_stop_playing_hook S_stop_playing_hook " (snd) is called when a sound finishes playing."
  #define H_stop_playing_channel_hook S_stop_playing_channel_hook " (snd chn) is called when a channel finishes playing."
  #define H_stop_playing_region_hook S_stop_playing_region_hook " (reg) is called when a region finishes playing."
  #define H_play_hook S_play_hook " (samps) is called each time a buffer is sent to the DAC."
  #define H_start_playing_hook S_start_playing_hook " (snd) is called when a play request is triggered. \
If it returns #t, the sound is not played."

  stop_playing_hook =         MAKE_HOOK(S_stop_playing_hook, 1, H_stop_playing_hook);                 /* arg = sound */
  stop_playing_channel_hook = MAKE_HOOK(S_stop_playing_channel_hook, 2, H_stop_playing_channel_hook); /* args = sound channel */
  stop_playing_region_hook =  MAKE_HOOK(S_stop_playing_region_hook, 1, H_stop_playing_region_hook);   /* arg = region number */
  start_playing_hook =        MAKE_HOOK(S_start_playing_hook, 1, H_start_playing_hook);               /* arg = sound */
  play_hook =                 MAKE_HOOK(S_play_hook, 1, H_play_hook);                                 /* args = size */

  init_rev_funcs(local_doc);
#if HAVE_GUILE
  EVAL_STRING("(set-reverb-control-procedures snd-nrev make-snd-nrev free-snd-nrev)");
  EVAL_STRING("(set-contrast-control-procedure snd-contrast)");
#endif
}
