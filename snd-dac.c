/* TODO: use clm for expsrc
 *       make revlen follow slider in "real-time":
 *         set up line_size in mus_make_comb to 5.0*srate/25641, then
 *         then as running, at each block reset to initial - new scaled
 *         (negative pm = longer delay)
 */

#include "snd.h"

#if (HAVE_ALSA || HAVE_OSS)
  #define MAX_DAC_BUFFER_SIZE 65536
#else
  #define MAX_DAC_BUFFER_SIZE 256
#endif

#define DEFAULT_DAC_BUFFER_SIZE 256

static int dac_buffer_size = 256;
static int dac_chans = 0;
static int dac_running = 0;
static int dac_decay = 0;

#if (HAVE_ALSA || HAVE_OSS)
/* static int dac_min_chans = 0; */ /* currently unused? */
#endif

static MUS_SAMPLE_TYPE dac_buffer[MAX_DAC_BUFFER_SIZE];
static Float *revin = NULL;
#if (HAVE_OSS || HAVE_ALSA)
  static MUS_SAMPLE_TYPE *dac_buffer0 = NULL;
  static MUS_SAMPLE_TYPE *dac_buffer1 = NULL;
#endif

static Float reverb_factor = 1.09;
static Float reverb_length = 1.0;
static Float lp_coeff = 0.7;
static Float volume = 1.0;
static int revchans = 0;
static int revdecay = 0;

#define MAX_DEV_FD 8
static int dev_fd[MAX_DEV_FD];

#if HAVE_GUILE

#include "sg.h"

/* user hooks into reverb */
static SCM g_make_reverb = SCM_UNDEFINED,g_reverb = SCM_UNDEFINED, g_free_reverb = SCM_UNDEFINED;
static int use_g_reverb = 0;
static SCM g_reverb_funcs(void) {return(SCM_LIST3(g_reverb,g_make_reverb,g_free_reverb));}
static SCM g_set_reverb_funcs(SCM rev, SCM make_rev, SCM free_rev)
{
  #define H_reverb_funcs "(" S_reverb_funcs ") -> list of the 3 reverb funcs (reverb make-reverb free-reverb)"
  #define H_set_reverb_funcs "(" S_set_reverb_funcs " reverb make-reverb free-reverb) sets the current reverb functions"
  if (gh_procedure_p(g_reverb)) snd_unprotect(g_reverb);
  if (gh_procedure_p(g_make_reverb)) snd_unprotect(g_make_reverb);
  if (gh_procedure_p(g_free_reverb)) snd_unprotect(g_free_reverb);
  if ((procedure_ok(rev,3,0,S_set_reverb_funcs,"reverb",1)) &&
      (procedure_ok(make_rev,3,0,S_set_reverb_funcs,"make-reverb",2)) &&
      (procedure_ok(free_rev,1,0,S_set_reverb_funcs,"free-reverb",3)))
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
  #define H_set_contrast_func "(" S_set_contrast_func " func) sets the current contrast function"
  if (gh_procedure_p(g_contrast)) snd_unprotect(g_contrast);
  if (procedure_ok(func,2,0,S_set_contrast_func,"contrast",1))
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
  #define H_set_expand_funcs "(" S_set_expand_funcs " expand make-expand free-expand) sets the current expand functions"
  if (gh_procedure_p(g_expand)) snd_unprotect(g_expand);
  if (gh_procedure_p(g_make_expand)) snd_unprotect(g_make_expand);
  if (gh_procedure_p(g_free_expand)) snd_unprotect(g_free_expand);
  if ((procedure_ok(expnd,3,0,S_set_expand_funcs,"expand",1)) &&
      (procedure_ok(make_expnd,3,0,S_set_expand_funcs,"make-expand",2)) &&
      (procedure_ok(free_expnd,1,0,S_set_expand_funcs,"free-expand",3)))
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

void g_init_dac(SCM local_doc)
{
  DEFINE_PROC(gh_new_procedure3_0(S_set_reverb_funcs,g_set_reverb_funcs),H_set_reverb_funcs);
  DEFINE_PROC(gh_new_procedure3_0(S_set_expand_funcs,g_set_expand_funcs),H_set_expand_funcs);
  DEFINE_PROC(gh_new_procedure1_0(S_set_contrast_func,g_set_contrast_func),H_set_contrast_func);
  DEFINE_PROC(gh_new_procedure0_0(S_reverb_funcs,g_reverb_funcs),H_reverb_funcs);
  DEFINE_PROC(gh_new_procedure0_0(S_expand_funcs,g_expand_funcs),H_expand_funcs);
  DEFINE_PROC(gh_new_procedure0_0(S_contrast_func,g_contrast_func),H_contrast_func);
}
#endif


void cleanup_dac(void)
{
  int i;
  if (dac_running) 
    {
      for (i=0;i<MAX_DEV_FD;i++) 
	{
	  if (dev_fd[i] != -1) 
	    {
	      mus_audio_close(dev_fd[i]);
	      dev_fd[i] = -1;
	    }
	}
    }
  for (i=0;i<MAX_DEV_FD;i++) dev_fd[i] = -1;
}


/* these (reverb expand etc) were written long before the CLM module -- someday I'll rewrite them to use CLM */
/* replaced reverb, contrast, and filter so far */

typedef struct {
  int s20;
  int s50;
  int rmp;
  Float amp;
  int len;
  int cur_out;
  int cur_in;
  int input_hop;
  int ctr;
  int output_hop;
  Float *b;
  Float *buf;
  int bufptr;
  int buflen;
  int bufbeg;
} spd_info;

static void *make_expand(snd_info *sp,Float sampling_rate,Float initial_ex)
{
  spd_info *spd;
  int outlen;
  Float segment_length,ramp_time,segment_scaler,output_hop,accuracy;
#if HAVE_GUILE
  if (use_g_expand)
    return((void *)g_call3(g_make_expand,gh_int2scm(sp->index),gh_double2scm(sampling_rate),gh_double2scm(initial_ex)));
#endif
  segment_length = sp->local_explen;
  ramp_time = sp->local_exprmp;
  segment_scaler = .6;
  output_hop = sp->local_exphop;
  accuracy = 10.0;
  spd = (spd_info *)CALLOC(1,sizeof(spd_info));
  spd->cur_out = 0;
  spd->len = (int)(ceil(segment_length * sampling_rate));
  spd->rmp = (int)(ramp_time * spd->len);
  spd->amp = segment_scaler;
  spd->output_hop = (int)(output_hop * sampling_rate);
  spd->input_hop = (int)((Float)spd->output_hop / initial_ex);
  spd->s20 = (int)(sampling_rate/(accuracy*20));
  spd->s50 = (int)(sampling_rate/(accuracy*50));
  spd->ctr = 0;
  if (segment_length > (output_hop + .02)) outlen = spd->len; else outlen = (int)(((output_hop + .1)*sampling_rate));
  spd->b = (Float *)CALLOC(outlen,sizeof(Float));
  spd->buflen = outlen;
  spd->bufptr = 0;
  spd->buf = (Float *)CALLOC(spd->buflen,sizeof(Float));
  spd->bufbeg = 0;
  spd->cur_in = 0;
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
	{
#endif
	  if (spd->b) FREE(spd->b);
	  if (spd->buf) FREE(spd->buf);
	  FREE(spd);
#if HAVE_GUILE
	}
#endif
    }
}

typedef struct {
  mus_any *comb1,*comb2,*comb3,*comb4,*comb5,*comb6;
  mus_any *allpass1,*allpass2,*allpass3,*allpass4,*allpass5,*allpass6,*allpass7,*allpass8;
  mus_any *onep;
} rev_info;

static void *global_reverb = NULL;
static int global_reverbing = 0;

typedef struct {
  Float cur_index;
  Float cur_amp;
  Float cur_srate;
  Float cur_exp;
  Float cur_rev; /* rev scaler -- len is set at initialization */
  Float contrast_amp;
  int expanding,reverbing,filtering; /* these need lots of preparation, so they're noticed only at the start */
  int chans;
  int slot;
  Float *lst; 
  Float *nxt;
  Float *x;
  Float *fvals;
  Float *a;
  Float rev_in;
  snd_fd **chn_fds;
  void **spds;
  mus_any **flt;
  region_info *ri;
  src_state **srcs;
  snd_info *sp; /* needed to see button callback changes etc */
  snd_state *state;
} dac_info;

void snd_make_filter(int order, Float *env, Float *a)
{ /* env = evenly sampled freq response, has order samples */
  int n,m,i,j,jj;
  Float am,q,xt = 0.0;
  n=order;
  if (n<=0) return;
  m=(n+1)/2;
  am=0.5*(n+1);
  q=TWO_PI/(Float)n;
  for (j=0,jj=n-1;j<m;j++,jj--)
    {
      xt=env[0]*0.5;
      for (i=1;i<m;i++)	xt += (env[i]*cos(q*(am-j-1)*i));
      a[j]=2.0*xt/(Float)n;
      a[jj]=a[j];
    }
}

static mus_any **make_flt(dac_info *dp, int order, int chans, Float *env)
{
  mus_any **flt;
  int i;
  if (order<=0) return(NULL);
  dp->a = (Float *)CALLOC(order,sizeof(Float));
  if (env) snd_make_filter(order,env,dp->a);
  flt = (mus_any **)CALLOC(chans,sizeof(mus_any *));
  for (i=0;i<chans;i++) flt[i] = mus_make_fir_filter(order,dp->a,NULL);
  return(flt);
}

#define ADD_NEXT_SAMPLE(val,sf)  do {if (sf->data > sf->last) val += next_sound(sf); else val += (*sf->data++);} while(0)
#define SCALE_NEXT_SAMPLE(val,sf,amp)  do {if (sf->data > sf->last) val += (MUS_SAMPLE_TYPE)(amp*next_sound(sf)); else val += (MUS_SAMPLE_TYPE)(amp*(*sf->data++));} while(0)

static void speed_1(dac_info *dp, Float sr, int chan)
{
  int move,i;
  MUS_SAMPLE_TYPE tmp;
  if ((use_sinc_interp((dp->state))) && (dp->srcs))
    {
      (dp->srcs[chan])->incr = sr;
      dp->fvals[chan] = MUS_SAMPLE_TO_FLOAT(run_src(dp->srcs[chan],0.0));
    }
  else
    {
      if (sr > 0.0) 
	{
	  dp->fvals[chan] = dp->lst[chan] + dp->x[chan] * (dp->nxt[chan] - dp->lst[chan]);
	  dp->x[chan] += sr;
	  move = (int)(dp->x[chan]);
	  if (move != 0)
	    {
	      dp->x[chan] -= move;
	      for (i=0;i<move;i++)
		{
		  dp->lst[chan] = dp->nxt[chan];
		  NEXT_SAMPLE(tmp,dp->chn_fds[chan]);
		  dp->nxt[chan] = MUS_SAMPLE_TO_FLOAT(tmp);
		}
	    }
	}
      else
	{
	  dp->fvals[chan] = dp->lst[chan] + dp->x[chan] * (dp->nxt[chan] - dp->lst[chan]);
	  dp->x[chan] -= sr;
	  move = (int)(dp->x[chan]);
	  if (move != 0)
	    {
	      dp->x[chan] -= move;
	      for (i=0;i<move;i++)
		{
		  dp->lst[chan] = dp->nxt[chan];
		  PREVIOUS_SAMPLE(tmp,dp->chn_fds[chan]);
		  dp->nxt[chan] = MUS_SAMPLE_TO_FLOAT(tmp);
		}
	    }
	}
    }
}

static void speed(dac_info *dp, Float sr)
{
  int chan;
  for (chan=0;chan<dp->chans;chan++) speed_1(dp,sr,chan);
}

static void load_speed_buf(dac_info *dp, Float sr, int beg, int end, spd_info *spd, int chan)
{
  int i;
  for (i=beg;i<end;i++)
    {
      speed_1(dp,sr,chan);
      spd->buf[spd->bufptr] = dp->fvals[chan];
      spd->bufptr++;
      if (spd->bufptr >= spd->buflen) {spd->bufbeg += spd->buflen; spd->bufptr = 0;}
    }
}

static unsigned long randx = 1;
#define INVERSE_MAX_RAND 0.0000305185

#if 0
static void c_srand(int val) {randx = val;} 
#endif

static int ci_frandom(int amp)
{
  int val;
  randx=randx*1103515245 + 12345;
  val=(unsigned int)(randx >> 16) & 32767;
  return((int)(amp * (((Float)val)*INVERSE_MAX_RAND)));
}

static void expand(dac_info *dp, Float sr, Float ex)
{ 
  /* from Mixer, used in "Leviathan", 1986 */
  spd_info *spd;
  snd_fd *sf;
  int end,i,j,jump,chan,speeding,loc,trig_time,iloc,k,ramp_time;
  Float curamp,incr;
  Float saved_val;
  snd_info *sp;
  MUS_SAMPLE_TYPE tmp;
  sp=dp->sp;
  speeding = ((sp->play_direction != 1) || (sp->srate != 1.0) || (dp->cur_srate != 1.0));
#if HAVE_GUILE
  if (use_g_expand)
    {
      /* if speeding, pick up speed vals first, else read direct (working at vals level here) */
      if (speeding) 
	speed(dp,sr);
      else
	{
	  for (chan=0;chan<dp->chans;chan++) 
	    {
	      NEXT_SAMPLE(tmp,dp->chn_fds[chan]);
	      dp->fvals[chan] = MUS_SAMPLE_TO_FLOAT(tmp);
	    }
	}
      for (chan=0;chan<dp->chans;chan++) 
	dp->fvals[chan] = gh_scm2double(g_call3(g_expand,(SCM)(dp->spds[chan]),gh_double2scm(dp->fvals[chan]),gh_double2scm(ex)));
    }
  else
    {
#endif
  for (chan=0;chan<dp->chans;chan++)
    {
      spd = (spd_info *)(dp->spds[chan]);
      dp->fvals[chan] = spd->b[spd->ctr];
      spd->ctr++;
      if (spd->ctr >= spd->cur_out)
	{
	  saved_val = dp->fvals[chan];
	  end = spd->len - spd->cur_out;
	  if (end>0) 
	    {
	      for (i=0,j=spd->cur_out;i<end;i++,j++) spd->b[i] = spd->b[j];
	      for (i=end;i<spd->len;i++) spd->b[i] = 0.0;
	    }
	  else
	    for (i=0;i<spd->cur_out;i++) spd->b[i] = 0.0;
	  ramp_time = spd->rmp;
	  incr = spd->amp / (Float)ramp_time;  /* precomputable */
	  trig_time = spd->len - ramp_time;
	  if (spd->cur_out > 0)
	    {
	      jump = spd->input_hop + ci_frandom(spd->s20);
	      spd->cur_in += jump;
	    }
	  else jump = spd->buflen;
	  sf = dp->chn_fds[chan];
	  if (speeding) 
	    load_speed_buf(dp,sr,0,jump,spd,chan);
	  else 
	    {
	      for (i=0;i<jump;i++) 
		{
		  NEXT_SAMPLE(tmp,sf); 
		  spd->buf[spd->bufptr] = MUS_SAMPLE_TO_FLOAT(tmp);
		  spd->bufptr++;
		  if (spd->bufptr >= spd->buflen) {spd->bufbeg += spd->buflen; spd->bufptr = 0;}
		}
	    }
	  for (i=0,k=0,curamp=0.0,loc=spd->cur_in;i<spd->len;i++,k++,loc++)
	    {
	      if (i<ramp_time) curamp+=incr; else if (i>trig_time) curamp-=incr;
	      iloc = loc - spd->bufbeg;
	      if (iloc >= 0) spd->b[k] += curamp * (Float)(spd->buf[iloc]);
	      else spd->b[k] += curamp * (Float)(spd->buf[iloc+spd->buflen]);
	    }
	  spd->ctr -= spd->cur_out;
	  spd->cur_out = spd->output_hop + ci_frandom(spd->s50);
	  spd->input_hop = (int)((Float)spd->output_hop / ex);
	  dp->fvals[chan] = saved_val;
	}
    }
#if HAVE_GUILE
    }
#endif
}


static void filter(dac_info *dp)
{
  int chan;
  for (chan=0;chan<dp->chans;chan++)
    dp->fvals[chan] = mus_fir_filter(dp->flt[chan],dp->fvals[chan]);
}

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

static int base_dly_len[15] = {1433, 1601, 1867, 2053, 2251, 2399, 347, 113, 37, 59, 53, 43, 37, 29, 19};
static int dly_len[15];

static void *make_reverb(snd_info *sp, Float sampling_rate, int chans)
{ 
  /* Mike McNabb's nrev from Mus10 days (ca. 1978) */
  Float srscale;
  int i;
  rev_info *r;

  revchans = chans;
  revdecay = 0;
  global_reverbing = 1;

#if HAVE_GUILE
  if (use_g_reverb)
    return((void *)g_call3(g_make_reverb,gh_double2scm(reverb_length),gh_double2scm(sampling_rate),gh_int2scm(chans)));
#endif

  reverb_factor = sp->local_revfb;
  lp_coeff = sp->local_revlp;
  srscale = reverb_length*sampling_rate/25641.0;
  for (i=0;i<15;i++)
    {
      dly_len[i] = get_prime((int)(srscale*base_dly_len[i]));
    }
  r=(rev_info *)CALLOC(1,sizeof(rev_info));
  r->comb1 = mus_make_comb(.822*reverb_factor,dly_len[0],NULL,dly_len[0]);
  r->comb2 = mus_make_comb(.802*reverb_factor,dly_len[1],NULL,dly_len[1]);
  r->comb3 = mus_make_comb(.773*reverb_factor,dly_len[2],NULL,dly_len[2]);
  r->comb4 = mus_make_comb(.753*reverb_factor,dly_len[3],NULL,dly_len[3]);
  r->comb5 = mus_make_comb(.753*reverb_factor,dly_len[4],NULL,dly_len[4]);
  r->comb6 = mus_make_comb(.733*reverb_factor,dly_len[5],NULL,dly_len[5]);
  r->onep = mus_make_one_pole(lp_coeff,lp_coeff-1.0);
  r->allpass1 = mus_make_all_pass(-0.700,0.700,dly_len[6],NULL,dly_len[6]);
  r->allpass2 = mus_make_all_pass(-0.700,0.700,dly_len[7],NULL,dly_len[7]);
  r->allpass3 = mus_make_all_pass(-0.700,0.700,dly_len[8],NULL,dly_len[8]);
  r->allpass4 = mus_make_all_pass(-0.700,0.700,dly_len[9],NULL,dly_len[9]);
  r->allpass5 = mus_make_all_pass(-0.700,0.700,dly_len[11],NULL,dly_len[10]);
  r->allpass6 = NULL;
  r->allpass7 = NULL;
  r->allpass8 = NULL;
  if (chans > 1)
    { 
      r->allpass6 = mus_make_all_pass(-0.700,0.700,dly_len[12],NULL,dly_len[12]);
      if (chans > 2)
	{
	  r->allpass7 = mus_make_all_pass(-0.700,0.700,dly_len[13],NULL,dly_len[13]);
	  r->allpass8 = mus_make_all_pass(-0.700,0.700,dly_len[14],NULL,dly_len[14]);
	}
    }
  return((void *)r);
}

static void free_reverb(void *ur)
{
  rev_info *r = (rev_info *)ur;

  global_reverbing = 0;

#if HAVE_GUILE
  if (use_g_reverb)
    g_call1(g_free_reverb,(SCM)ur);
  else
#endif

  if (r)
    {
      mus_free(r->comb1);
      mus_free(r->comb2);
      mus_free(r->comb3);
      mus_free(r->comb4);
      mus_free(r->comb5);
      mus_free(r->comb6);
      mus_free(r->onep);
      mus_free(r->allpass1);
      mus_free(r->allpass2);
      mus_free(r->allpass3);
      mus_free(r->allpass4);
      mus_free(r->allpass5);
      if (r->allpass6) mus_free(r->allpass6);
      if (r->allpass7) mus_free(r->allpass7);
      if (r->allpass8) mus_free(r->allpass8);
      FREE(r);
    }
}

static void reverb(void *ur, Float inp, MUS_SAMPLE_TYPE *outs, int chans)
{
  rev_info *r = (rev_info *)ur;
  Float rin,rout;
#if HAVE_GUILE
  SCM outputs;
  int i;
  if (use_g_reverb)
    {
      outputs = g_call3(g_reverb,(SCM)ur,gh_double2scm(volume*inp),gh_int2scm(chans));
      for (i=0;i<chans;i++) outs[i] += MUS_FLOAT_TO_SAMPLE(((Float)(gh_scm2double(gh_vector_ref(outputs,gh_int2scm(i))))));
    }
  else
    {
#endif
      rin = volume * inp;
      rout = mus_all_pass(r->allpass4,
			  mus_one_pole(r->onep,
				       mus_all_pass(r->allpass3,
						    mus_all_pass(r->allpass2,
								 mus_all_pass(r->allpass1,
									      mus_comb(r->comb1,rin,0.0) + 
									      mus_comb(r->comb2,rin,0.0) + 
									      mus_comb(r->comb3,rin,0.0) + 
									      mus_comb(r->comb4,rin,0.0) + 
									      mus_comb(r->comb5,rin,0.0) + 
									      mus_comb(r->comb6,rin,0.0),
									      0.0),
								 0.0),
						    0.0)),
			  0.0);
      outs[0] += MUS_FLOAT_TO_SAMPLE(mus_all_pass(r->allpass5,rout,0.0));
      if (chans > 1)
	{
	  outs[1] += MUS_FLOAT_TO_SAMPLE(mus_all_pass(r->allpass6,rout,0.0));
	  if (chans > 2)
	    {
	      outs[2] += MUS_FLOAT_TO_SAMPLE(mus_all_pass(r->allpass7,rout,0.0));
	      outs[3] += MUS_FLOAT_TO_SAMPLE(mus_all_pass(r->allpass8,rout,0.0));
	    }
	}
#if HAVE_GUILE
    }
#endif
}

static void contrast (dac_info *dp, Float amp, Float index)
{
  int i;
#if HAVE_GUILE
  if (use_g_contrast)
    {
      for (i=0;i<dp->chans;i++)
	dp->fvals[i] = amp * gh_scm2double(g_call2(g_contrast,
						   gh_double2scm(dp->contrast_amp * dp->fvals[i]),
						   gh_double2scm(index)));
    }
  else
    {
#endif
  for (i=0;i<dp->chans;i++)
    dp->fvals[i] = amp * mus_contrast_enhancement(dp->contrast_amp * dp->fvals[i],index);
#if HAVE_GUILE
    }
#endif
}

static void apply_changes(dac_info *dp, snd_info *sp, Float amp, Float sr, Float index, Float ex, Float revscl)
{
  int i;
  if (dp->expanding) 
    expand(dp,sr,ex);
  else speed(dp,sr);
  if (sp->contrasting) 
    contrast(dp,amp,index);
  else 
    {
      for (i=0;i<dp->chans;i++) dp->fvals[i] *= amp;
    }
  if (dp->filtering) filter(dp);
  if (dp->reverbing) 
    dp->rev_in = dp->fvals[0]*revscl;
}

static void free_dac_info (dac_info *dp)
{
  int i;
  FREE(dp->lst);
  FREE(dp->nxt);
  FREE(dp->x);
  FREE(dp->fvals);
  if (dp->a) {FREE(dp->a); dp->a = NULL;}
  for (i=0;i<dp->chans;i++)
    {
      free_snd_fd(dp->chn_fds[i]);
    }
  FREE(dp->chn_fds);
  dp->chn_fds = NULL;
  if (dp->spds) 
    {
      for (i=0;i<dp->chans;i++)
	{
	  free_expand(dp->spds[i]);
	}
      FREE(dp->spds);
      dp->spds = NULL;
    }
  if (dp->srcs) 
    {
      for (i=0;i<dp->chans;i++)
	{
	  free_src(dp->srcs[i]);
	}
      FREE(dp->srcs);
      dp->srcs = NULL;
    }
  if (dp->flt) 
    {
      for (i=0;i<dp->chans;i++)
	mus_free(dp->flt[i]);
      FREE(dp->flt);
    }
  if (dp->ri) FREE(dp->ri);
  FREE(dp);
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

static dac_info *make_dac_info(snd_info *sp, int chans, snd_fd **fds)
{
  dac_info *dp;
  int i;
  Float *data = NULL;
  dp = (dac_info *)CALLOC(1,sizeof(dac_info));
  dp->lst = (Float *)CALLOC(chans,sizeof(Float));
  dp->nxt = (Float *)CALLOC(chans,sizeof(Float));
  dp->fvals = (Float *)CALLOC(chans,sizeof(Float));
  dp->x = (Float *)CALLOC(chans,sizeof(Float));
  dp->ri = NULL;
  dp->a = NULL;
  if (sp)
    {
      dp->expanding = sp->expanding;
      dp->filtering = ((sp->filtering) && (sp->filter_order > 0));
      dp->reverbing = sp->reverbing;
      dp->contrast_amp = sp->contrast_amp;
      if (use_sinc_interp(sp->state))
	{
	  dp->srcs = (src_state **)CALLOC(chans,sizeof(src_state *));
	  for (i=0;i<chans;i++)
	    {
	      dp->srcs[i] = make_src(sp->state,1.0,fds[i]);
	    }
	}
      else dp->srcs = NULL;
      if (dp->expanding) 
	{
	  dp->spds = (void **)CALLOC(chans,sizeof(void *));
	  for (i=0;i<chans;i++)
	    {
	      dp->spds[i] = make_expand(sp,(Float)SND_SRATE(sp),sp->expand);
	    }
	}
      if (dp->reverbing)
	{
	  reverb_length = sp->revlen;
	  if (!global_reverb) global_reverb = (void *)make_reverb(sp,(Float)SND_SRATE(sp),chans);
	}
      if (dp->filtering)
	{
	  sp->filter_changed = 0;
	  if (!(sp->filter_env)) dp->filtering = 0;
	  else
	    {
	      data = sample_linear_env(sp->filter_env,sp->filter_order);
	      dp->flt = make_flt(dp,sp->filter_order,chans,data);
	      FREE(data);
	    }
	}
    }
  dp->state = get_global_state();
  dp->chn_fds = fds;
  dp->sp = sp;
  dp->chans = chans;
  return(dp);
}

static int dac_max_sounds = 0;
static dac_info **play_list = NULL;
#define INITIAL_MAX_SOUNDS 16
static int play_list_members = 0;
static int max_active_slot = -1;

static void reflect_play_stop (snd_info *sp) 
{
  if (w_snd_play(sp)) set_toggle_button(w_snd_play(sp),FALSE,FALSE,sp);
  set_file_browser_play_button(sp->shortname,0);
}

static void stop_playing_1(void *dp1, int toggle)
{
  snd_info *sp = NULL;
  int i,index = -1;
  chan_info *ncp;
  region_info *ri = NULL;
  dac_info *dp = (dac_info *)dp1;
  if ((dp1 == NULL) || (play_list == NULL)) return;
  sp = dp->sp;
  ri = dp->ri;
  if (sp) 
    {
      sp->playing_mark = NULL;
      if (sp->playing > 0) sp->playing--;
      if (sp->cursor_follows_play != DONT_FOLLOW)
	{
	  for (i=0;i<sp->nchans;i++) 
	    {
	      ncp = sp->chans[i];
	      handle_cursor(ncp,cursor_moveto(ncp,ncp->original_cursor));
	    }
	  if (sp->cursor_follows_play == FOLLOW_ONCE) sp->cursor_follows_play = DONT_FOLLOW;
	  /* if ctrl-click play, c-t, c-q -> this flag is still set from aborted previous play, so clear at c-t (or c-g) */
	}
    }
  if (ri) index = ri->n;
  play_list[dp->slot] = NULL;
  play_list_members--;
  if (toggle) {if ((sp) && (sp->playing <= 0)) reflect_play_stop(sp); else if (ri) reflect_play_region_stop(ri);}
  if (dp->slot == max_active_slot) max_active_slot--;
  free_dac_info(dp);
  if ((ri) && (index >= 0))
    call_stop_playing_region_hook(index); 
  else call_stop_playing_hook(sp);
  /* chan case?? */
  if ((sp) && (sp->delete_me)) completely_free_snd_info(sp); /* dummy snd_info struct for (play "filename") in snd-scm.c */
}

static void stop_playing(void *dp1) {stop_playing_1(dp1,TRUE);}

static void stop_playing_sound_1(snd_info *sp, int toggle)
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
	      stop_playing_1(play_list[i],toggle);
	      play_list[i] = NULL;
	    }
	}
    }
}

void stop_playing_sound(snd_info *sp) {stop_playing_sound_1(sp,TRUE);}
void stop_playing_sound_no_toggle(snd_info *sp) {stop_playing_sound_1(sp,FALSE);}

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
  region_info *ri;
  if (play_list)
    {
      for (i=0;i<=max_active_slot;i++)
	{
	  if (play_list[i])
	    {
	      ri = play_list[i]->ri;
	      if ((ri) && (ri->n == n)) 
		{
		  stop_playing(play_list[i]);
		  play_list[i] = NULL;
		}
	    }
	}
    }
}

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

static void start_playing_1(void *ptr, int start, int background, int paused)
{
  int slot,chans = 0,i,direction,beg = 0,channels = 1;
  dac_info *dp;
  snd_info *sp = NULL;
  snd_state *ss = NULL;
  dac_manager *dac_m;
  chan_info *cp = NULL,*ncp = NULL;
  region_info *ri = NULL;
  snd_fd **fds;
  if ((!(background)) && (play_list_members > 0)) return;
  switch (((snd_any *)ptr)->s_type)
    {
    case SND_INFO:
      sp = (snd_info *)ptr;
      if (!(sp->inuse)) return;
      if (sp) 
	{
	  if (call_start_playing_hook(sp))
	    {
	      reflect_play_stop(sp); /* turns off buttons (snd-xdac.c) */
	      if (sp->delete_me) completely_free_snd_info(sp); /* dummy snd_info struct for (play "filename") in snd-scm.c */
	      return;
	    }
	}
      ss = sp->state;
      ss->apply_choice = APPLY_TO_SOUND;
      chans = sp->nchans;
      break;
    case CHAN_INFO: 
      cp = (chan_info *)ptr; 
      sp = cp->sound;
      if (!(sp->inuse)) return;
      ss = sp->state;
      ss->apply_choice = APPLY_TO_SOUND;
      if (sp->syncing != 0)
	chans = sp->nchans;
      else 
	{
	  chans = 1; 
	  if (sp->nchans > 1) ss->apply_choice = APPLY_TO_CHANNEL;
	}
      /* start playing hook?? */
      break;
    case REGION_INFO:
      ri = (region_info *) ptr;
      if (!(ri->rg)) sp = region_sound(ri->n); else sp=NULL;
      /* to be completely consistent with the region browser, this should be just sp=NULL;
       * but that disables a nice feature that we can goof with the controls while listening
       * to just the current active selection (sp is not null only in this case).
       * But, that means a subsequent 'Apply' will not do 'the right thing' -- it will try to
       * treat the whole file, rather than deleting the current selection, running apply over
       * that data, and splicing in the result (perhaps longer etc).  To get the latter to work will require a
       * much smarter selection editing mechanism (since file length can change) -- and what
       * is the selection afterwards?  Does it follow the redo/undo state?
       */
      ss = (snd_state *)(ri->ss);
      if ((sp) && (ri->n == 0)) ss->apply_choice = APPLY_TO_SELECTION; else ss->apply_choice = APPLY_TO_SOUND;
      chans = region_chans(ri->n);
      /* start playing hook?? */
      break;
    }
  if (sp) sp->playing++;
  if ((sp) && (sp->cursor_follows_play != DONT_FOLLOW)) /* sp can be nil if ptr is region */
    {
      for (i=0;i<sp->nchans;i++) 
	{
	  ncp = sp->chans[i];
	  ncp->original_cursor = ncp->cursor;
	  handle_cursor(ncp,cursor_moveto(ncp,start));
	}
    }
  slot = find_slot_to_play();
  if (slot == -1) return;
  play_list_members++;
  if (sp)
    {
      if (sp->play_direction == 1) 
	{
	  direction = READ_FORWARD; 
	  beg = start;
	}
      else 
	{
	  direction = READ_BACKWARD;
	  if (start == 0) beg = current_ed_samples(sp->chans[0])-1; else beg = start;
	}
    }
  else direction = READ_FORWARD;
  fds = (snd_fd **)CALLOC(chans,sizeof(snd_fd *));
  switch (((snd_any *)ptr)->s_type)
    {
    case SND_INFO:  
      for (i=0;i<chans;i++)
	{
	  cp = sp->chans[i];
	  fds[i] = init_sample_read(beg,cp,direction);
	}
      break;

    case CHAN_INFO: 
      if (chans > 1)
	{
	  for (i=0;i<chans;i++)
	    {
	      cp = sp->chans[i];
	      fds[i] = init_sample_read(beg,cp,direction);
	    }
	}
      else fds[0] = init_sample_read(beg,cp,direction);
      break;

    case REGION_INFO:
      for (i=0;i<chans;i++)
	{
	  fds[i] = init_region_read(ss,0,ri->n,i,direction);
	}
      break;
    }
  dp = make_dac_info(sp,chans,fds);
  dp->ri = ri;
  if (chans > channels) channels = chans;
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
  if (!dac_running)
    {
      ss->play_start_time = 0; /* not redundant */
      ss->play_start_time = main_time(ss);
      dac_m = (dac_manager *)CALLOC(1,sizeof(dac_manager));
      dac_m->slice = 0;
      dac_m->ss = ss;
      dac_m->srate = 0;
      if (sp) 
	dac_m->srate = SND_SRATE(sp); 
      else 
	if (((snd_any *)ptr)->s_type == REGION_INFO)
	  dac_m->srate = region_srate(ri->n);
        else 
	  {
	    sp = any_selected_sound(ss);
	    if (sp) dac_m->srate = SND_SRATE(sp);
	  }
      if (dac_m->srate <= 0) dac_m->srate = 44100;
      dac_decay = (int)(reverb_decay(ss) * dac_m->srate);
      dac_m->channels = channels;
      if (!paused)
	{
	  if (background) 
	    set_play_in_progress(ss,dac_m); /* -> feed_dac */
	  else
	    {
	      /* here we want to play as an atomic (not background) action */
	      while (feed_dac(dac_m) == BACKGROUND_CONTINUE)
		{
		  check_for_event(ss); /* need to be able to C-g out of this */
		  if ((sp) && (!(sp->inuse))) break;
		}
	    }
	}
    }
}

void start_playing(void *ptr, int start) {start_playing_1(ptr,start,TRUE,FALSE);}
void play_to_end(void *ptr, int start) {start_playing_1(ptr,start,FALSE,FALSE);}

/* TODO: check stop_playing -- perhaps need a sync indication for it? */
void start_playing_syncd(snd_info *sp, int start, int background)
{
  int i;
  snd_state *ss;
  snd_info *nsp = NULL,*lsp = NULL;
  ss = sp->state;
  if (sp->syncing != 0)
    {
      for (i=0;i<ss->max_sounds;i++)
	{
	  nsp = ss->sounds[i];
	  if ((nsp) && (nsp->inuse) && (nsp->syncing == sp->syncing))
	    {
	      if (lsp) start_playing_1((void *)lsp,start,background,TRUE); /* TRUE=dac waits for the rest of the sounds to be queued up */
	      lsp = nsp;
	    }
	}
      if (lsp) start_playing_1((void *)lsp,start,background,FALSE); /* this triggers the dac */
    }
  else start_playing_1((void *)sp,start,background,FALSE);
}

void start_playing_chan_syncd(chan_info *cp, int start, int background, int pause)
{
  snd_info *sp;
  int old_syncing;
  sp = cp->sound;
  old_syncing = sp->syncing;
  sp->syncing = 0;
  start_playing_1((void *)cp,start,background,pause);
  sp->syncing = old_syncing;
}

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
	    return(NO_CHANGE);
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

static unsigned char *dacbuf = NULL,*dacbuf1 = NULL;
static int compatible_format = MUS_COMPATIBLE_FORMAT;

static int fill_dac(snd_state *ss, int write_ok)
{
  int i,j,k,m,dac_increments,len,cursor_change,chn;
  MUS_SAMPLE_TYPE *bufs[1];
  int bytes,dp_chans;
  Float amp,incr,sr,sincr,ind,indincr,ex,exincr,rev,revincr;
  dac_info *dp;
  snd_info *sp;
  Float *data;
  bufs[0] = dac_buffer;
  bytes = dac_buffer_size * mus_data_format_to_bytes_per_sample(compatible_format);
  memset(dac_buffer,0,dac_buffer_size * sizeof(MUS_SAMPLE_TYPE));
  if (global_reverb) 
    {
      if (revin == NULL)
	revin = (Float *)CALLOC(MAX_DAC_BUFFER_SIZE,sizeof(Float));
      else memset((char *)revin,0,dac_buffer_size * sizeof(Float));
    }
  dac_increments = dac_buffer_size / dac_chans;
  len = dac_increments;
  if (dac_pausing) 
    cursor_change = 0;
  else
    {
      cursor_time += len;
      cursor_change = (cursor_time >= CURSOR_UPDATE_INTERVAL);
      for (i=0;i<=max_active_slot;i++)
	{
	  if ((dp = (play_list[i])))
	    {
	      if (dac_folding(ss)) dp_chans = dp->chans; else dp_chans = dac_chans;
	      sp = dp->sp; /* can be nil if region playing */
	      if ((sp) && ((!(sp->inuse)) || (sp->playing == 0))) return(-len);
	      if ((sp) && (cursor_change) && (sp->cursor_follows_play != DONT_FOLLOW)) 
		{
		  if (sp->syncing != 0) 
		    handle_cursor(sp->chans[0],cursor_moveto(sp->chans[0],current_location(dp->chn_fds[0])));
		  else
		    {
		      for (m=0;m<dp->chans;m++) 
			handle_cursor(sp->chans[m],cursor_moveto(sp->chans[m],current_location(dp->chn_fds[m])));
		    }
		}
	      switch (choose_dac_op(dp,sp))
		{
		  
		case NO_CHANGE:
		  /* simplest case -- no changes at all */
		  if (dp_chans == 1) 
		    {
		      for (j=0;j<dac_buffer_size;j+=dac_chans) 
			ADD_NEXT_SAMPLE(dac_buffer[j],dp->chn_fds[0]);
		    }
		  else 
		    {
		      if (dp_chans <= dac_chans)
			{
			  for (j=0;j<dac_buffer_size;j+=dac_chans) 
			    {
			      for (k=0;k<dp_chans;k++) 
				ADD_NEXT_SAMPLE(dac_buffer[j+k],dp->chn_fds[k]);
			    }
			}
		      else
			{
			  for (j=0;j<dac_buffer_size;j+=dac_chans) 
			    {
			      for (chn=0,k=0;k<dp_chans;k++) 
				{
				  ADD_NEXT_SAMPLE(dac_buffer[j+chn],dp->chn_fds[k]); 
				  chn++; 
				  if (chn == dac_chans) chn = 0;
				}
			    }
			}
		    }
		  break;

		case JUST_AMP:
		  /* sp->amp is current UI value, dp->cur_amp is current local value */
		  amp = dp->cur_amp;
		  incr = (sp->amp - amp) / (Float)dac_increments;
		  if (dp_chans == 1) 
		    {
		      for (j=0;j<dac_buffer_size;j+=dac_chans,amp+=incr) 
			SCALE_NEXT_SAMPLE(dac_buffer[j],dp->chn_fds[0],amp);
		    }
		  else 
		    {
		      if (dp_chans <= dac_chans)
			{
			  for (j=0;j<dac_buffer_size;j+=dac_chans,amp+=incr) 
			    {
			      for (k=0;k<dp_chans;k++) 
				SCALE_NEXT_SAMPLE(dac_buffer[j+k],dp->chn_fds[k],amp);
			    }
			}
		      else
			{
			  for (j=0;j<dac_buffer_size;j+=dac_chans,amp+=incr) 
			    {
			      for (chn=0,k=0;k<dp_chans;k++) 
				{
				  if (chn >= dac_chans) chn = 0;
				  SCALE_NEXT_SAMPLE(dac_buffer[j+chn],dp->chn_fds[k],amp); 
				  chn++; 
				}
			    }
			}
		    }
		  dp->cur_amp = amp;
		  break;

		case JUST_SPEED:
		  /* includes amp changes */
		  /* sp->srate is current UI value, dp->cur_srate is current local value */
		  amp = dp->cur_amp;
		  incr = (sp->amp - amp) / (Float)dac_increments;
		  sr = dp->cur_srate;
		  sincr = (sp->srate*sp->play_direction - sr) / (Float)dac_increments;
		  if ((sr != 0.0) || (sincr != 0.0))
		    {
		      for (j=0;j<dac_buffer_size;j+=dac_chans,amp+=incr,sr+=sincr) 
			{
			  speed(dp,sr);
			  for (chn=0,k=0;k<dp_chans;k++) 
			    {
			      if (chn >= dac_chans) chn = 0;
			      dac_buffer[j+chn] += MUS_FLOAT_TO_SAMPLE(amp*dp->fvals[k]);
			      chn++; 
			    }
			}
		    }
		  dp->cur_srate = sr;
		  dp->cur_amp = amp;
		  break;

		case ALL_CHANGES:
		  amp = dp->cur_amp;
		  incr = (sp->amp - amp) / (Float)dac_increments;
		  sr = dp->cur_srate;
		  sincr = (sp->srate*sp->play_direction - sr) / (Float)dac_increments;
		  ind = dp->cur_index;
		  indincr = (sp->contrast - ind) / (Float)dac_increments;
		  ex = dp->cur_exp;
		  exincr = (sp->expand - ex) / (Float)dac_increments;
		  rev = dp->cur_rev;
		  revincr = (sp->revscl - rev) / (Float)dac_increments;
		  if ((dp->filtering) && (sp->filter_changed))
		    {
		      data = sample_linear_env(sp->filter_env,sp->filter_order);
		      snd_make_filter(sp->filter_order,data,dp->a); /* since dp->a is used directly, this might work */
		      FREE(data);
		      sp->filter_changed = 0;
		    }
		  for (j=0;j<dac_buffer_size;j+=dac_chans,amp+=incr,sr+=sincr,ind+=indincr,ex+=exincr,rev+=revincr) 
		    {
		      apply_changes(dp,sp,amp,sr,ind,ex,rev);
		      for (chn=0,k=0;k<dp_chans;k++)
			{
			  if (chn >= dac_chans) chn = 0;
			  dac_buffer[j+chn] += MUS_FLOAT_TO_SAMPLE(dp->fvals[k]);
			  chn++; 
			}
		      if (dp->reverbing) revin[j] += dp->rev_in;
		    }
		  dp->cur_srate = sr;
		  dp->cur_amp = amp;
		  dp->cur_rev = rev;
		  dp->cur_exp = ex;
		  dp->cur_index = ind;
		  break;
		}

	      /* check for EOF */
	      if (read_sample_eof(dp->chn_fds[0]))
		{
		  if (write_ok)  /* i.e. not apply */
		    stop_playing(dp);
		  else 
		    {
		      if (!global_reverb) 
			return(-len); /* stop apply without any reverb ringing */
		      else 
			{ /* let reverb ring through revdecay */
			  /* what's this about??? */
			  stop_playing_all_sounds();
			  play_list_members = 0; 
			  max_active_slot = -1;
			}
		    }
		}
	    }
	}
      if (global_reverb) 
	{
	  for (i=0;i<dac_buffer_size;i+=dac_chans)
	    {
	      reverb(global_reverb,revin[i],(MUS_SAMPLE_TYPE *)(dac_buffer+i),revchans);
	    }
	  if (play_list_members == 0)
	    {
	      revdecay += dac_buffer_size;
	      if (revdecay > dac_decay) {global_reverbing=0; revdecay=0; if (!write_ok) return(-len);}
	    }
	}
    }
#if (HAVE_OSS || HAVE_ALSA)
  if (write_ok) 
    {
      if (dev_fd[1] != -1)
	{
	  if (dac_buffer0 == NULL)
	    {
	      dac_buffer0 = (MUS_SAMPLE_TYPE *)CALLOC(MAX_DAC_BUFFER_SIZE,sizeof(MUS_SAMPLE_TYPE));
	      dac_buffer1 = (MUS_SAMPLE_TYPE *)CALLOC(MAX_DAC_BUFFER_SIZE,sizeof(MUS_SAMPLE_TYPE));
	    }
	  /* split in 2, write to both */
	  for (j=0,k=0;j<dac_buffer_size;j+=4,k+=2)
	    {
	      dac_buffer0[k] = dac_buffer[j];
	      dac_buffer0[k+1] = dac_buffer[j+1];
	      dac_buffer1[k] = dac_buffer[j+2];
	      dac_buffer1[k+1] = dac_buffer[j+3];
	    }
	  bufs[0] = dac_buffer0;
	  mus_file_write_buffer(compatible_format,0,dac_buffer_size/2-1,1,bufs,(char *)dacbuf,data_clipped(ss));
	  bufs[0] = dac_buffer1;
	  mus_file_write_buffer(compatible_format,0,dac_buffer_size/2-1,1,bufs,(char *)dacbuf1,data_clipped(ss));
	  mus_audio_write(dev_fd[0],(char *)dacbuf,bytes/2);
	  mus_audio_write(dev_fd[1],(char *)dacbuf1,bytes/2);
	}
      else 
	{
	  mus_file_write_buffer(compatible_format,0,dac_buffer_size-1,1,bufs,(char *)dacbuf,data_clipped(ss));
	  mus_audio_write(dev_fd[0],(char *)dacbuf,bytes);
	}
    }
#else
  if (write_ok) 
    {
      mus_file_write_buffer(compatible_format,0,dac_buffer_size-1,1,bufs,(char *)dacbuf,data_clipped(ss));
      mus_audio_write(dev_fd[0],(char *)dacbuf,bytes);
    }
#endif
  if (cursor_change) cursor_time = 0;
  return(len);
}


#if (HAVE_ALSA || HAVE_OSS)

#define MAX_ALSA_DEVS (64)

int mus_audio_compatible_format(int dev) 
{
  int err, i;
  float val[32];
  int ival[32];
  err = mus_audio_mixer_read(dev,MUS_AUDIO_FORMAT,32,val);
  if (err != -1)
    {
      for (i=0;i<=val[0];i++) ival[i] = (int)(val[i]);
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
  return(MUS_COMPATIBLE_FORMAT);
}

/* Controls behavior of device selection logic below. No amount of logic
 * can make everybody happy all the time. The [i]logic below cannot always
 * produce the desired result but deleting it altogether will break the
 * systems that currently rely on it. Not wise without an external api
 * in place designed to select whatever the user _reeely_ wants. Till 
 * then set this to "1" to always send to the first device. */

int feed_first_device = 0;

static int really_start_audio_output (dac_manager *tm)
{
  int err;
  snd_state *ss;
  int i;
  float direction;
  int samples_per_channel = 256;
  int min_fragment_size = 0;
  int max_fragment_size = 65536;

  int index, cards, card, devs, dev, d;
  float val[MAX_ALSA_DEVS];
  int devices[MAX_ALSA_DEVS];
  int available_chans[MAX_ALSA_DEVS];
  int max_chans[MAX_ALSA_DEVS];
  int min_chans[MAX_ALSA_DEVS];
  int out_dev[MAX_DEV_FD];

  int max_chans_value=0;
  int max_chans_dev=0;
  int alloc_devs=0;
  int alloc_chans=0;

  ss = tm->ss;
  if (mus_audio_api() == ALSA_API) {
    /* FIXME: all this initialization block should be moved somewhere else,
     * where it gets executed just once at snd startup time. A user preference
     * for output device should have priority over this logic. At this time
     * we always select the widest device if the requested channels fit into it. 
     * Otherwise we try to combine devices, if all fails we modify snd settings
     * so that channel folding takes place. This is inefficient but works for now. 
     */
    cards=mus_audio_systems();
    index=0;
    /* scan all cards and build a list of available output devices */
    for (card=0; card<cards; card++) {
      if ((err=mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(card), MUS_AUDIO_PORT, MAX_ALSA_DEVS, val))!=0) {
	snd_error("%s[%d] %s: mus_audio_mixer_read: %s ", __FILE__, __LINE__, __FUNCTION__,
		  mus_audio_error_name(mus_audio_error()));
      }
      devs=(int)(val[0]);
      /* scan all devices in the card */
      for (d=0; d<devs; d++) {
	dev=(int)(val[d+1]);
	if ((err=mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(card)|dev, MUS_AUDIO_DIRECTION, 0, &direction))!=0) {
	  snd_error("%s: can't read direction, ignoring device %d", __FUNCTION__, dev);
	  direction=0;
	} else {
	  if((int)direction==0) {
	    /* remember output device */
	    devices[index++]=MUS_AUDIO_PACK_SYSTEM(card)|dev;
	    if (index>=MAX_ALSA_DEVS) goto NO_MORE_DEVICES;
	  }
	}
      }
    }
  NO_MORE_DEVICES:
    /* get channel availability for all devices */
    for (d=0; d<index; d++) {
      available_chans[d]=min_chans[d]=max_chans[d]=0;
      if ((err=mus_audio_mixer_read(devices[d], MUS_AUDIO_CHANNEL, 2, val))==0) {
	available_chans[d]=(int)(val[0]);
	min_chans[d]=(int)(val[1]);
	max_chans[d]=(int)(val[2]);
	if (max_chans[d]>max_chans_value) {
	  /* remember widest device */
	  max_chans_value=max_chans[d];
	  max_chans_dev=d;
	}
      }
    }
    /* allocate devices for playback */
    alloc_chans=0;
    alloc_devs=0;
    for (d=0; d<MAX_DEV_FD; d++) out_dev[d]=dev_fd[d]=-1;
    if (feed_first_device == 0) {
      /* see if widest device can accomodate all channels */
      if (max_chans_value>=tm->channels) {
	out_dev[alloc_devs++]=max_chans_dev;
	alloc_chans+=max_chans_value;
      }
      if (alloc_devs==0) {
	/* try to use several devices */
	int this_format=-1;
	int prev_format=-1;
	for (d=0; d<index; d++) {
	  this_format=mus_audio_compatible_format(devices[d]);
	  if (prev_format==-1) {
	    prev_format=this_format;
	  }
	  /* format for all selected devices should match */
	  if (this_format==prev_format) {
	    out_dev[alloc_devs++]=d;
	    alloc_chans+=available_chans[d];
	    if (alloc_devs>=MAX_DEV_FD ||
		/* FIXME: limit number of devices to two for now, 
		 * we have to reimplement fill_dac for more */
		alloc_devs>1) 
	      break;
	  }
	}
	if (alloc_devs!=0 && alloc_chans<tm->channels) {
	  /* not enough available channels, give up */
	  for (d=0; d<MAX_DEV_FD; d++) out_dev[d]=-1;
	  alloc_devs=0;
	  alloc_chans=0;
	}
	if (alloc_devs==0) {
	  /* fold all channels into the first device */
	  out_dev[alloc_devs++]=0;
	  alloc_chans+=available_chans[0];
	}
      }
    } else {
      /* first device on first card is the one */
      out_dev[alloc_devs++]=0;
      alloc_chans+=available_chans[0];
    }
    compatible_format=mus_audio_compatible_format(devices[out_dev[0]]);
    if (alloc_devs<2) {
      /* see if we have a minimum sized frame to fill 
       * FIXME: could this happen in more than one device? */
      int c=min_chans[out_dev[0]];
      if (c>tm->channels) {
	tm->channels=c;
      }
    }
    /* see if we have to fold channels */
    if (alloc_chans<tm->channels) {
      if (dac_folding(ss)) snd_warning("folding %d chans into %d ", tm->channels, alloc_chans);
      tm->channels=alloc_chans;
    }
    /* read the number of samples per channel the device wants buffered */
    if ((err=mus_audio_mixer_read(devices[out_dev[0]], MUS_AUDIO_SAMPLES_PER_CHANNEL, 2, val))!=-1) {
      samples_per_channel = (int)(val[0]);
      min_fragment_size = (int)(val[1]);
      max_fragment_size = (int)(val[2]);
    }
    dac_buffer_size=samples_per_channel*tm->channels;
    while (dac_buffer_size*sizeof(MUS_SAMPLE_TYPE)>MAX_DAC_BUFFER_SIZE) {
      samples_per_channel/=2;
      dac_buffer_size=samples_per_channel*tm->channels;
    }
    set_dac_size(ss, dac_buffer_size*mus_data_format_to_bytes_per_sample(compatible_format));
    /* open all allocated devices */
    for (d=0; d<alloc_devs; d++) {
      int channels=available_chans[out_dev[d]];
      if (alloc_chans<=available_chans[out_dev[d]]) {
	if (tm->channels<min_chans[out_dev[d]]) {
	  channels=min_chans[out_dev[d]];
	} else {
	  channels=tm->channels;
	}
      }
      /* FIXME: assumes devices are same size... */
      dev_fd[d]=mus_audio_open_output(devices[out_dev[d]], tm->srate, channels, 
				      compatible_format, (dac_size(ss))/alloc_devs);
      
      if (dev_fd[d]==-1) {
	/* could not open a device, close all others and quit playback */
	int i;
	for (i=0; i<d; i++) {
	  mus_audio_close(devices[out_dev[i]]);
	}
	snd_error("%s: %s", STR_cant_play, mus_audio_error_name(mus_audio_error()));
	dac_running=0;
	unlock_recording_audio();
	if (global_reverb) {free_reverb(global_reverb); global_reverb=NULL;}
	max_active_slot=-1;
	FREE(tm); 
	return(-1);
      }
    }
  #if (!NONINTERLEAVED_AUDIO)
    /* create buffers, FIXME: assumes two devices max */
    if (dacbuf) FREE(dacbuf);
    if (dacbuf1) FREE(dacbuf1);
    dacbuf=(unsigned char *)CALLOC(dac_buffer_size * mus_data_format_to_bytes_per_sample(compatible_format),sizeof(unsigned char));
    dacbuf1=(unsigned char *)CALLOC(dac_buffer_size * mus_data_format_to_bytes_per_sample(compatible_format),sizeof(unsigned char));
  #endif
  } else {
    /* api == OSS_API */
    for (i=0;i<MAX_ALSA_DEVS;i++) available_chans[i]=2;
    if (tm->channels > 2)
      {
	err = mus_audio_mixer_read(audio_output_device(ss),MUS_AUDIO_CHANNEL,0,val);
	if (err != -1) available_chans[0] = (int)(val[0]);
      }
    for (i=0;i<MAX_DEV_FD;i++) dev_fd[i] = -1;
    /* see if we can play 16 bit output */
    compatible_format = mus_audio_compatible_format(audio_output_device(ss));
  #ifndef PPC
    /* check for chans>def chans, open 2 cards if available */
    if ((available_chans[0] < tm->channels) && (tm->channels == 4))
      {
	if (mus_audio_systems() > 1)
	  {
	    dev_fd[0] = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | audio_output_device(ss),tm->srate,2,compatible_format,dac_size(ss));
	    if (dev_fd[0] != -1) 
	      dev_fd[1] = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(1) | audio_output_device(ss),tm->srate,2,compatible_format,dac_size(ss));
	  }
	else
	  {
	    /* there is one special case here: Ensoniq's allow you to play quad
	     * by sending two channels (non-clock-synchronous with the other two)
	     * out the line in port, but this possibility confuses LinuxPPC (OSS-Free)
	     */
	    dev_fd[0] = mus_audio_open_output(MUS_AUDIO_AUX_OUTPUT,tm->srate,2,compatible_format,dac_size(ss));
	    if (dev_fd[0] != -1) 
	      dev_fd[1] = mus_audio_open_output(audio_output_device(ss),tm->srate,2,compatible_format,dac_size(ss));
	  }
	if (dev_fd[1] == -1)
	  {
	    mus_audio_close(dev_fd[0]);
	    dev_fd[0] = -1;
	  }
	else available_chans[0] = 4;
      }
  #endif
    if (dacbuf) FREE(dacbuf);
    if (dacbuf1) FREE(dacbuf1);
    dacbuf = (unsigned char *)CALLOC(dac_buffer_size * mus_data_format_to_bytes_per_sample(compatible_format),sizeof(unsigned char));
    dacbuf1 = (unsigned char *)CALLOC(dac_buffer_size * mus_data_format_to_bytes_per_sample(compatible_format),sizeof(unsigned char));
    if (available_chans[0] < tm->channels) 
      {
	if (dac_folding(ss)) snd_warning("folding %d chans into %d ",tm->channels,available_chans[0]);
	tm->channels = available_chans[0];
      }
    if (dev_fd[0] == -1)
      dev_fd[0] = mus_audio_open_output(audio_output_device(ss),tm->srate,tm->channels,compatible_format,dac_size(ss));
    if (dev_fd[0] == -1)
      {
	snd_error("%s: %s",STR_cant_play,mus_audio_error_name(mus_audio_error()));
	dac_running = 0;
	unlock_recording_audio();
	if (global_reverb) {free_reverb(global_reverb); global_reverb=NULL;}
	max_active_slot = -1;
	FREE(tm); 
	return(-1);
      }
  }
  return(0);
}
#else /* not ALSA or OSS */

static int really_start_audio_output (dac_manager *tm)
{
  int err;
  snd_state *ss;
  int i;
  int available_chans = 2;
  float val[32];

  ss = tm->ss;
  if (tm->channels > 2)
    {
      err = mus_audio_mixer_read(audio_output_device(ss),MUS_AUDIO_CHANNEL,0,val);
      if (err != -1) available_chans = (int)(val[0]);
    }
  for (i=0;i<MAX_DEV_FD;i++) dev_fd[i] = -1;
  compatible_format = MUS_COMPATIBLE_FORMAT;
  if (dacbuf) FREE(dacbuf);
  if (dacbuf1) FREE(dacbuf1);
  dacbuf = (unsigned char *)CALLOC(dac_buffer_size * mus_data_format_to_bytes_per_sample(compatible_format),sizeof(unsigned char));
  dacbuf1 = (unsigned char *)CALLOC(dac_buffer_size * mus_data_format_to_bytes_per_sample(compatible_format),sizeof(unsigned char));
  if (available_chans < tm->channels) 
    {
      if (dac_folding(ss)) snd_warning("folding %d chans into %d ",tm->channels,available_chans);
      tm->channels = available_chans;
    }
  if (dev_fd[0] == -1)
    dev_fd[0] = mus_audio_open_output(audio_output_device(ss),tm->srate,tm->channels,compatible_format,dac_size(ss));
  if (dev_fd[0] == -1)
    {
      snd_error("%s: %s",STR_cant_play,mus_audio_error_name(mus_audio_error()));
      dac_running = 0;
      unlock_recording_audio();
      if (global_reverb) {free_reverb(global_reverb); global_reverb=NULL;}
      max_active_slot = -1;
      FREE(tm); 
      return(-1);
    }
  return(0);
}
#endif

static int start_audio_output (dac_manager *tm)
{
  snd_state *ss;
#if NONINTERLEAVED_AUDIO
  int i;
#endif
  ss = tm->ss;
  cursor_time = 0;
  lock_recording_audio();
#if NONINTERLEAVED_AUDIO
  /* initialize the per-channel buffers */
  if (dac_buffers == NULL) 
    {
      dac_buffers = (MUS_SAMPLE_TYPE **)CALLOC(ss->audio_hw_channels,sizeof(MUS_SAMPLE_TYPE *));
      for (i = 0; i < ss->audio_hw_channels; i++) 
	{
	  dac_buffers[i] = (MUS_SAMPLE_TYPE *)CALLOC(MAX_DAC_BUFFER_SIZE,sizeof(MUS_SAMPLE_TYPE));
	}
    }
#endif
  if (really_start_audio_output(tm)) return(-1);
  dac_chans = tm->channels;
  dac_running = 1;
  fill_dac(ss,1);
  lock_apply(tm->ss,NULL);
  return(0);
}
 
static void stop_audio_output (dac_manager *tm)
{
   int i;
   for (i=0;i<MAX_DEV_FD;i++)
     if (dev_fd[i] != -1) 
       {
	 mus_audio_close(dev_fd[i]);
	 dev_fd[i] = -1;
       }
   dac_running = 0;
   unlock_recording_audio();
   dac_pausing = 0;
   if (global_reverb) {free_reverb(global_reverb); global_reverb=NULL;}
   max_active_slot = -1;
   unlock_apply(tm->ss,NULL);
   FREE(tm);
}

BACKGROUND_TYPE feed_dac(dac_manager *tm)
{
  /* return BACKGROUND_QUIT when done */
  snd_state *ss;
  ss = tm->ss;
  switch (tm->slice)
    {
    case 0: /* start_dac, get first buffer, goto next step, return BACKGROUND_CONTINUE */
      if (start_audio_output(tm)) 
	return(BACKGROUND_QUIT);
      tm->slice++;
      return(BACKGROUND_CONTINUE);
      break;
    case 1: /* get next buffer, feed dac, when play_list_members == 0 goto next step, return BACKGROUND_CONTINUE */
      fill_dac(ss,1);
      if ((!global_reverbing) && (play_list_members == 0)) tm->slice++; 
      return(BACKGROUND_CONTINUE);
      break;
     case 2: /* close dac, clear play_in_progress, unset play buttons? return BACKGROUND_QUIT */
       stop_audio_output(tm);
       return(BACKGROUND_QUIT);
       break;
     }
  return(BACKGROUND_QUIT);
}
 

/* ---------------- support for Apply button (snd-apply.c) ---------------- */

static int apply_dur = 0;
static int apply_beg = 0;
static int apply_reporting = 0;
static snd_fd *apply_location_fd;
#define APPLY_TICKS 50
static int apply_tick = 0;
static int apply_dac_op = 0;

int apply_duration(void) {return(apply_dur);}

void initialize_apply(snd_info *sp)
{
  int i,direction,chans;
  dac_info *dp;
  snd_state *ss;
  snd_fd **fds;
  ss = sp->state;
  switch (ss->apply_choice)
    {
    case APPLY_TO_SOUND: chans = sp->nchans; break;
    case APPLY_TO_SELECTION: chans = region_chans(0); break;
    case APPLY_TO_CHANNEL: chans = 1; break;
    default: chans = sp->nchans; ss->apply_choice = APPLY_TO_SOUND; break;
    }
  if (sp->play_direction == 1) 
    {
      direction = READ_FORWARD; 
      apply_beg = 0;
    }
  else 
    {
      direction = READ_BACKWARD;
      apply_beg = current_ed_samples(sp->chans[0])-1;
    }
  fds = (snd_fd **)CALLOC(chans,sizeof(snd_fd *));
  switch (ss->apply_choice)
    {
    case APPLY_TO_SOUND:
      apply_dur = current_ed_samples(sp->chans[0]);
      for (i=0;i<chans;i++)
	fds[i] = init_sample_read(apply_beg,sp->chans[i],direction);
      break;
    case APPLY_TO_SELECTION:
      apply_dur = region_len(0); /* len = samps per chan */
      apply_beg = 0;
      for (i=0;i<chans;i++)
	fds[i] = init_region_read(ss,0,0,i,direction);
      break;
    case APPLY_TO_CHANNEL:
      apply_dur = current_ed_samples(sp->chans[0]);
      fds[0] = init_sample_read(apply_beg,selected_channel(ss),direction);
      break;
    }
  dp = make_dac_info(sp,chans,fds);
  max_active_slot = 0;
  if (play_list == NULL) 
    {
      dac_max_sounds = INITIAL_MAX_SOUNDS;
      play_list = (dac_info **)CALLOC(dac_max_sounds,sizeof(dac_info *));
    }
  play_list[0] = dp;
  play_list_members = 1;
  sp->playing = 1;
  dp->cur_srate = sp->srate*sp->play_direction;
  dp->cur_amp = sp->amp;
  dp->cur_index = sp->contrast;
  dp->cur_exp = sp->expand;
  dp->cur_rev = sp->revscl;
  apply_dac_op = choose_dac_op(dp,sp);
  dac_chans = chans;
  apply_reporting = (apply_dur > (MAX_BUFFER_SIZE * 4));
  if (apply_reporting) 
    {
      start_progress_report(ss,sp,NOT_FROM_ENVED);
      apply_location_fd = fds[0];
      apply_tick = 0;
    }
}

int finalize_apply(snd_info *sp)
{
  /* if no reverb, these need to be cleaned up */
  stop_playing_all_sounds();
  max_active_slot = -1;
  play_list_members = 0;
  sp->playing = 0;
  if (global_reverb) {free_reverb(global_reverb); global_reverb=NULL;}
  if (apply_reporting) finish_progress_report(sp->state,sp,NOT_FROM_ENVED);
  return(apply_dac_op);
}

int run_apply(snd_info *sp, int ofd)
{
  int len;
  snd_state *ss;
  MUS_SAMPLE_TYPE *bufs[1];
  ss = sp->state;
  len = fill_dac(ss,0); /* only place where fill_dac return value is used (negative => all done) */

  /* here if not NO_CHANGE or JUST_AMP, and cp->marks, we need to check
   *   for new mark locations, saving associated reset-marks list across
   *   fill-dac calls, then at end, reset marks. 
   *   for (i=0;i<=max_active_slot;i++) {if ((dp = (play_list[i]))) current_location(dp->chn_fds[0])}
   *   this set of pointers should be saved somewhere also
   * actual edit takes place in apply_controls_1 in snd-apply.c
   * (there's also ss->apply_choice complication)
   * and this has to be across all chans 
   */

  check_for_event(ss);
  /* if C-G, stop_applying called which cancels and backs out */
  if (apply_reporting) 
    {
      if (ss->stopped_explicitly)
	finish_progress_report(ss,sp,NOT_FROM_ENVED);
      else
	{
	  apply_tick++;
	  if (apply_tick > APPLY_TICKS)
	    {
	      apply_tick = 0;
	      progress_report(ss,sp,"apply",1,1,(Float)abs(current_location(apply_location_fd) - apply_beg) / (Float)apply_dur,NOT_FROM_ENVED);
	    }
	}
    }

  /* sr .07 -> infinite output? */

  bufs[0] = dac_buffer;
  mus_file_write(ofd,0,dac_buffer_size-1,1,bufs);
  return(len);
}


