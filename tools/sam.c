/* a samson box emulator */

/* I assume what people really want is a good rendition from their ancient SAM files,
 *   not an exact replica of the Samson box output.  The latter used 12, 14, 20, 24, 28, and 30-bit
 *   fractional and integer fields, which are a pain to deal with when we would rather use doubles.
 *   The 20-bitness matters in the noise calculation.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

#include <byteswap.h>
#define big_endian_int(n) bswap_32(n)

#define DESCRIBE_COMMANDS false
#define OUTPUT_FILENAME "test.wav"
#define TOTAL_SAMPLES -1


#define LDB(Cmd, Size, Position) ((Cmd >> Position) & ((1 << Size) - 1))
#define BIT(Cmd, Position) ((Cmd >> Position) & 1)

#define TWOS_20(N) ((N < (1 << 19)) ? N : ((N & 0x7ffff) - (1 << 19)))
#define TWOS_24(N) ((N < (1 << 23)) ? N : ((N & 0x7fffff) - (1 << 23)))
#define TWOS_28(N) ((N < (1 << 27)) ? N : ((N & 0x7ffffff) - (1 << 27)))
#define TWOS_30(N) ((N < (1 << 29)) ? N : ((N & 0x1fffffff) - (1 << 29)))

#define TWOS_20_TO_DOUBLE(N) ((double)TWOS_20(N) / (double)(1 << 19))
#define TWOS_24_TO_DOUBLE(N) ((double)TWOS_24(N) / (double)(1 << 23))
#define TWOS_28_TO_DOUBLE(N) ((double)TWOS_28(N) / (double)(1 << 27))
#define TWOS_30_TO_DOUBLE(N) ((double)TWOS_30(N) / (double)(1 << 29))

#define UNSIGNED_12_TO_DOUBLE(N) ((double)N / (double)(1 << 12))
#define DOUBLE_TO_TWOS_20(X) ((X >= 0.0) ? (int)(X * (1 << 19)) : (int)((X + 1.0) * (1 << 19)))


#if (!defined(M_PI))
  #define M_PI 3.14159265358979323846264338327
  #define M_PI_2 (M_PI / 2.0)
#endif


typedef struct {
  int GO, GJ, GK, GN, GM, GP, GQ, GL, GSUM, GFM, GS, GMODE;
  double f_GO, f_GJ, f_GK, f_GM, f_GP, f_GQ, f_GL;
} generator;

typedef struct {
  int M0, M1, L0, L1, MIN, MRM, MSUM, MMODE, MMMMM, T, mult_scl_1, mult_scl_0, o_M0, o_M1; 
  double f_M0, f_M1, f_L0, f_L1, o_f_M0, o_f_M1;
  /* by "2nd multiplication" I think Pete means M0 since it follows M1 so AA -> M0 and BB -> M1*/
} modifier;

typedef struct {
  int P, Z, Y, X;
} delay;

static double gen_outs[64], gen_ins[64], mod_outs[64], mod_ins[64]; /* "sum memory" */

static generator *gens[256];
static modifier *mods[128];
static delay *dlys[32];

static double delay_memory[65536];
static double f_delay_memory[65536];
static float dac_out[4];

static int tick, pass, DX, highest_tick_per_pass, samples = 0, srate = 1;

FILE *snd_file = NULL; /* for now just riff/wave quad, but srate depends on tick setting */

static void start_clean(void)
{
  int i;
  for (i = 0; i < 256; i++)
    {
      gen_outs[i] = 0.0; /* "outs" are this pass */
      gen_ins[i] = 0.0;  /* "ins" are last pass */
      mod_outs[i] = 0.0;
      mod_ins[i] = 0.0;
    }

  for (i = 0; i < 256; i++)
    gens[i] = (generator *)calloc(1, sizeof(generator));

  for (i = 0; i < 128; i++)
    {
      mods[i] = (modifier *)calloc(1, sizeof(modifier));
      mods[i]->mult_scl_1 = 1;
      mods[i]->mult_scl_0 = 1;
    }

  for (i = 0; i < 32; i++)
    dlys[i] = (delay *)calloc(1, sizeof(delay));

  for (i = 0; i < 65536; i++)
    delay_memory[i] = 0.0;

  for (i = 0; i < 4; i++)
    dac_out[i] = 0.0;

  tick = 0;
  pass = 0;
}

static void all_done(void)
{
  if (snd_file)
    {
      int header_info[1];
      fclose(snd_file);
      snd_file = fopen(OUTPUT_FILENAME, "r+");
      fseek(snd_file, 4L, SEEK_SET);
      header_info[0] = 88 + samples * 4 * 4;  /* total data bytes  4 chans, 4 bytes/float */
      fwrite((void *)header_info, 4, 1, snd_file);
      fseek(snd_file, 76L, SEEK_SET);
      header_info[0] = samples * 4 * 4;
      fwrite((void *)header_info, 4, 1, snd_file);
      fclose(snd_file);

      fprintf(stderr, "test.wav: %dHz, %.4f secs\n", srate, (double)samples / (double)srate);
    }
  exit(0);
}


static void dac_write(double data, int chan)
{
  /* during a given pass we accumulate output to the dac */

  /*
  fprintf(stderr, "dac_write(%.4f, %d)\n", data, chan);
  */
  dac_out[chan] += (float)data;
}


/* ---------------------------------------- generator processing ---------------------------------------- */

/*
 * DAJ - Here is JOS's translation into english of the generator processing.
 *
 * Associated with each generator are the following quantities:
 * FrqSwp20  (20 bits) alpha -- oscillator frequency sweep rate
 * OscFrq28  (28 bits) omega -- oscillator frequency
 * OscAng20  (20 bits) theta -- oscillator angle
 * NumCos11  (11 bits) number of cosines to be summed
 * CosScl4   (4 bits) binary scale of cosine or sum of cosines
 * AmpSwp20  (20 bits) delta -- decay rate
 * CurAmp24  (24 bits) phi -- decay exponent
 * AmpOff12  (12 bits) asymptote
 * OutSum6   (6 bits) sum memory address into which output is added
 * FmSum7    (7 bits) sum memory address from which frequency modulation data is taken
 *	    FmSum7 = QAAAAAA
 *	    Q: 0  generator-last-pass quadrant
 *	       1  modifier-last-pass quadrant
 *	     AAAAAA:  sum address within quadrant
 * Gmode10   (10 bits) generator mode
 *	     Gmode10 = RRRREESSSS
 *
 * Processing
 * ----------
 *
 *	Calculations performed for a generator, governed by its
 * mode, proceed as detailed below.
 *
 * 1)  The word in sum memory addressed by FmSum7 is read (20 bits);
 * 	the sum is formed of it and the high-order 20 bits of
 * 	OscFrq28 (call the result FmPhase20).
 * 
 * 2)  If the oscillator side is running, FrqSwp20, right-adjusted with
 * 	sign extended, is added into OscFrq28.
 * 
 * 3)  If the oscillator mode is SIN(J+Fm), FmPhase20 is taken; otherwise OscAng20.
 * 	Call the 20-bit result Phase20, and its high-order 13 bits
 * 	Phase13.
 * 
 * 4)  If the oscillator side is running, FmPhase20 is added into OscAng20.
 * 
 * 5)  If the run mode is WRITEDATA, the word in sum memory addressed by FmSum7
 * 	is sent to the CPU as the next write-data item; if the run
 * 	mode is DACOUT it is sent to the DAC addressed by the low-order
 * 	4 bits of FrqSwp20.
 * 
 * 6)  In oscillator modes other than SIN(K) and SIN(J+Fm), Phase13 is multiplied
 * 	by NumCos11.  Call the low-order 12 bits of the product, with two bits
 * 	equal to 01 appended to the right, the 14-bit result SinAdr.
 * 	In oscillator modes SIN(K) and SIN(J+Fm), SinAdr is the high-order 13
 * 	bits of Phase20, with a bit equal to 1 appended to the right.
 * 
 * 7)  If the oscillator mode is SIN(K) or SIN(J+Fm), pi/2 is taken (the binary
 * 	number 010...0); otherwise Phase13.  Call the result CscAdr.
 * 
 * 8)  In floating point, the product csc (CscAdr) * sin (SinAdr) is
 * 	formed; then converted to fixed point with a scale factor
 * 	of 2**(-CosScl4).  Call the result (13 bits) TblOut13.
 * 
 * 
 * 9)  The result of the oscillator side (13 bits, call it OscOut13) is
 * 	then determined according to the oscillator mode.
 * 	SSSS: SUMCOS 	TblOut13
 * 	      SAWTOOTH 	Phase13 (but 0 when Phase13 is 1000000000000)
 * 	      SQUARE 	-1/2 (on a scale from -1 to +1) if Phase13 is negative,
 * 		   	  else +1/2
 * 	      PULSE 	+1/2 if overflow occured in step 1) or 4) above;
 * 		     	  else 0.
 * 	      SIN(K) 	TblOut13
 * 	      SIN(J+Fm) TblOut13
 * 
 * 10)  The high-order 12 bits of CurAmp24 are taken (call the result CurAmp12).
 * 
 * 11)  If the envelope side is running, AmpSwp20 right-adjusted, sign
 * 	extended, is added into CurAmp24 (overflow dealt with according
 * 	to the run mode).  (The overflow condition is CurAmp24 changing
 * 	sign such that the high-order bit of the resultant CurAmp24 equals
 * 	the sign bit of AmpSwp20.)
 * 
 * 12)  If the envelope mode is 10 or 11, 2**(-CurAmp12) is looked up;
 * 	otherwise CurAmp12 is taken.  Call the resulting 12 bits NewAmp12.
 * 	Scaling is such that if CurAmp12 is 0 then 2**(-CurAmp12) is
 * 	111 111 111 101 binary; if CurAmp12 is 000 100 000 000 binary,
 * 	then 2**(-CurAmp12) is 011 111 111 110.
 * 
 * 13)  If the envelope mode is 01 or 11, NewAmp12 is added to AmpOff12; else
 * 	it is subtracted from AmpOff12.  This creates Env12, the result
 * 	of the envelope side.
 * 
 * 14)  OscOut13 is multiplied by Env12.  If the run mode specifies adding
 * 	into sum memory, the high-order 19 bits of the rounded product,
 * 	right-adjusted with sign extended, are added into the sum
 * 	memory location designated by OutSum6; except that in run mode
 * 	READDATA, the product is added to the next read-data item from the
 * 	CPU and the sum replaces the contents of the sum memory
 * 	location addressed.
 */

#define osc_mode(gmode) (gmode & 0xf)
/*
SSSS: 0100  sum of cosines
      0001  sawtooth
      0010  square
      0011  pulse train
      0000  sin (K)
      1000  sin (J + fm)
*/
#define SUMCOS 4
#define SAWTOOTH 1
#define SQUARE 2
#define PULSE 3
#define SIN_K 0
#define SIN_FM 8


#define osc_env(gmode) ((gmode >> 4) & 0x3)
/*
EE: 00  L - Q
    01  L + Q
    10  L - 2**(-Q)
    11  L + 2**(-Q)
*/
#define L_PLUS_Q 1
#define L_MINUS_Q 0
#define L_MINUS_2_TO_MINUS_Q 2
#define L_PLUS_2_TO_MINUS_Q 3


#define osc_run(gmode) ((gmode >> 6) & 0xf)

static void set_osc_run(int gen, int RRRR)
{
  generator *g;
  if (gen >= 256) {fprintf(stderr, "gen mode set overflow\n"); gen = 0;}
  g = gens[gen];
  /* RRRREESSSS */
  g->GMODE = (g->GMODE & 0x3f) | (RRRR << 6);
}

/*				 osc. run?  env. run?  add to sum?
  RRRR:0000 inactive		    no	       no	   no
     0001 pause			    no	       no	   no
     1111 running A		    yes	    yes, sticky	   yes
     1110 running B		    yes	    yes, free;	   yes
					  triggers subseq.
					    on overflow
     1001 wait			    yes	       no	   no
     1101 running C		    yes	    yes, free;	   yes
					    stops and
					  triggers subseq.
					    on overflow
     0111 read data from computer   no	       yes	   yes
     0011 write data to computer    no	       no	   no
     0010 write data to DAC	    no	       no	   no
	   (address in GO)
*/

static bool osc_is_running(int mode)
{
  int RRRR;
  RRRR = osc_run(mode);
  return((RRRR == 15) || (RRRR == 14) || (RRRR == 9) || (RRRR == 13));
}

static bool env_is_running(int mode)
{
  int RRRR;
  RRRR = osc_run(mode);
  return((RRRR == 15) || (RRRR == 14) || (RRRR == 7) || (RRRR == 13));
}

static bool adding_to_sum(int mode)
{
  int RRRR;
  RRRR = osc_run(mode);
  return((RRRR == 15) || (RRRR == 14) || (RRRR == 7) || (RRRR == 13));
}

static bool pulse_warned = false;

static void process_gen(int gen)
{
  #define FmSum7    g->GFM
  #define OutSum6   g->GSUM
  #define FrqSwp20  g->f_GO
  #define OscFreq28 g->f_GJ
  #define OscAng20  g->f_GK
  #define NumCos11  g->GN
  #define AmpSwp20  g->f_GP
  #define AmpOff12  g->f_GL
  #define Gmode10   g->GMODE
  #define CurAmp24  g->f_GQ
  #define CosScl4   g->GM
  #define ShiftOut  g->GS

  generator *g;
  double fm, FmPhase20, Phase20, Phase13, SinAdr, CscAdr, TblOut13, OscOut13, CurAmp12, NewAmp12, Env12;
  double temp;

  g = gens[gen];
  if (osc_run(g->GMODE) == 0) /* inactive */
    return;

  if (osc_run(Gmode10) == 3)
    {
      fprintf(stderr, "Can't writedata!\n");
      return;
    }

  if ((FmSum7 >> 6) == 0)
    fm = gen_ins[FmSum7 & 0x3f];
  else fm = mod_ins[FmSum7 & 0x3f];

  if (osc_run(Gmode10) == 2)
    {
      dac_write(fm, g->GO & 0xf); /* in this case, we need the integer value of GO */
      return;
    }

  FmPhase20 = fm + OscFreq28; 
  
  if (osc_is_running(Gmode10))
    OscFreq28 += (FrqSwp20 / 256.0);     /* right adjusted 20 bit */
  
  if (osc_mode(Gmode10) == 9) /* sin(J+fm) */
    Phase20 = FmPhase20;
  else Phase20 = OscAng20;
  Phase13 = Phase20;             /* >> 7 */

  if (osc_is_running(Gmode10))
    OscAng20 += FmPhase20;
  
  if ((osc_mode(Gmode10) != SIN_K) && 
      (osc_mode(Gmode10) != SIN_FM))
    {
      SinAdr = (Phase13 * NumCos11);           /* was & 0xfff) << 2) + 1 */
      CscAdr = Phase13;
      temp = sin(M_PI * SinAdr) / sin(M_PI * CscAdr);       /* was (1 << 13)) */
    }
  else 
    {
      SinAdr = Phase20;                        /* was >> 6) | 1 */
      temp = sin(M_PI * SinAdr);
    }

  TblOut13 = temp / (double)(1 << CosScl4);

  switch (osc_mode(Gmode10))
    {
    case SUMCOS: case SIN_K: case SIN_FM:
      OscOut13 = TblOut13;
      break;
      
    case SAWTOOTH:
      OscOut13 = Phase13;
      if (Phase13 == -1.0) OscOut13 = 0.0;
      break;

    case SQUARE:
      if (Phase13 < 0.0) 
	OscOut13 = -1.0;  /* ?? */
      else OscOut13 = 1.0;
      break;

    case PULSE:
      if (!pulse_warned)
	{
	  fprintf(stderr, "pulse not implemented yet\n");
	  pulse_warned = true;
	}
      break;
    }

  CurAmp12 = CurAmp24;
  
  if (env_is_running(Gmode10))
    {
      double old_amp;
      old_amp = CurAmp24;
      CurAmp24 += (AmpSwp20 / 16.0);
      /*
	The envelope side of the generator can be sticky, which means
	that rather than overflow it will stay at the last value it attained
	before it would have overflowed; or it can be free, in which case it
	wraps around.

	Transitions between run modes can be accomplished in various ways.
	1)  A command can output a new GMODE.
	2)  A MISC command can specify "clear all pause bits", which
		will cause any generator in run mode 0001 to change to
		mode 1111.
	3)  A MISC command can specify "clear all wait bits", which
		will cause any generator in run mode 1001 to change to
		mode 1111.
	4)  If the envelope side of a generator in run mode 1101
		overflows, that generator goes to run mode 1001.
	5)  A generator in run mode 1001 will go to run mode 1101 if
		on the same pass the preceding generator (the one
		whose generator number is one less) caused a
		trigger (was in run mode 1110 or 1101 and envelope
		overflowed).
      */
      if ((CurAmp24 > 1.0) || (CurAmp24 < -1.0))  /*  if ((BIT(CurAmp24, 23) != BIT(old_amp, 23)) && (BIT(CurAmp24, 22) == BIT(AmpSwp20, 19))) */
	{
	  /* overflow */
	  fprintf(stderr, "env overflow\n");

	  if (osc_run(Gmode10) == 15)              /* "running A" */
	    CurAmp24 = old_amp;
	  else
	    {
	      if (osc_run(Gmode10) == 13)          /* "running C" */
		{
		  set_osc_run(gen, 9);
		  if (osc_run(gens[gen + 1]->GMODE) == 9)
		    set_osc_run(gen + 1, 13);
		}
	      else
		{
		  if ((osc_run(Gmode10) == 14) &&  /* "running B" */
		      (osc_run(gens[gen + 1]->GMODE) == 9))
		    set_osc_run(gen + 1, 13);		      
		}
	    }
	}
    }

  if ((osc_env(Gmode10) == L_PLUS_2_TO_MINUS_Q) || 
      (osc_env(Gmode10) == L_MINUS_2_TO_MINUS_Q))
    NewAmp12 = pow(2.0, -CurAmp12);
  else NewAmp12 = CurAmp12;
  /* in the notes: The scaling involved is a left shift of temp6 by 4 bits.
   */

  if ((osc_env(Gmode10) == L_PLUS_Q) || 
      (osc_env(Gmode10) == L_PLUS_2_TO_MINUS_Q))  
    Env12 = AmpOff12 + NewAmp12;
  else Env12 = AmpOff12 - NewAmp12;

  OscOut13 *= Env12;
  if (adding_to_sum(Gmode10))
    {
      if (osc_run(Gmode10) != 7)
	gen_outs[OutSum6] += OscOut13; /* what is the scaling here? 25 -> 19 according to specs, I think */
      else fprintf(stderr, "read data?!?\n");
    }

  /*    TODO: shift
	If GS is 0, the high-order 19 bits
	of the rounded product are taken, right-adjusted with sign
	extended; if GS is 1, the high-order 20 bits of the rounded
	product are taken.  Call this Temp9.  If the run mode 
	specifies adding into sum memory, Temp9 is added into the sum
	memory location designated by GSUM; except that in run mode
	0111, the product is added to the next read-data item from the
	CPU and the sum replaces the contents of the sum memory
	location addressed.
  */

}


/* ---------------------------------------- modifier processing ---------------------------------------- */

/* 
 * 	Each modifier has the following numeric parameters.
 * M0  (30 bits) coefficient
 * M1  (30 bits) other coefficient
 * L0  (20 bits) running term
 * L1  (20 bits) other running term
 * MIN  (8 bits) address in sum memory where modifier reads "A" data
 * MRM  (8 bits) address in sum memory where modifier reads "B" data
 * 	MIN, MRM = QQAAAAAA
 *     QQ:	
 *      00  generator-last-pass quadrant
 * 	01  modifier-last-pass quadrant
 * 	10  modifier-this-pass quadrant
 * 	11  (reserved)
 *     AAAAAA: sum address within quadrant
 * MSUM  (7 bits) result address in sum memory
 * 	MSUM = RAAAAAA
 *     R: 0  add to sum
 *        1  replace sum
 *     AAAAAA: sum address in modifier-this-pass quadrant
 */

static void print_mod_read_name(int m)
{
  char *mem_names[4] = {"gen-ins", "mod-ins", "mod-outs", "oops"};
  fprintf(stderr, "%s[%d]", mem_names[(m >> 6) & 0x3], m & 0x3f);
}


static double mod_read(int addr)
{
  int QQ, AAAAA;
  AAAAA = addr & 0x1f;
  QQ = LDB(addr, 2, 6);
  switch (QQ)
    {
    case 0: return(gen_ins[AAAAA]);
    case 1: return(mod_ins[AAAAA]);
    case 2: return(mod_outs[AAAAA]);
    }
  fprintf(stderr, "bad MIN/MRM\n");
  abort();
  return(0);
}

static void mod_write(int addr, double val)
{
  int R, AAAAAA;
  AAAAAA = addr & 0x3f;
  R = BIT(addr, 6);
  if (R == 0)
    mod_outs[AAAAAA] += val;
  else mod_outs[AAAAAA] = val;
}

/*
 * MMODE  (9 bits) modifier mode
 * 	MMODE = MMMMMAABB
 * AA:  scale of second multiplication
 * BB:  scale of first multiplication
 * For fraction multiplications:
 *   00:  x 1
 *   01:  x 2
 *   10:  x 4
 *   11:  x 8
 * For integer multiplications:
 *   00:  x 1/4
 *   01:  x 1/2
 *   10:  x 1
 *   11:  x 2
 *  A multiplication involving parameter M1 will be the first
 *   multiplication; one involving M0 will be the second.
 * 
 * MMMMM: function
 *   00000:  inactive
 *   00010:  uniform noise
 *   00011:  triggered uniform noise
 *   00100:  latch
 *   00110:  threshold
 *   00111:  invoke delay unit
 * 
 *   01000:  two poles
 *   01001:  two poles, M0 variable
 *   01011:  two poles, M1 variable
 *   01100:  two zeros
 *   01101:  two zeros, M0 variable
 *   01111:  two zeros, M1 variable
 * 
 *   10000:  integer mixing
 *   10001:  one pole
 *   10100:  mixing
 *   10110:  one zero
 * 
 *   11000:  four-quadrant multiplication
 *   11001:  amplitude modulation
 *   11010:  maximum
 *   11011:  minimum
 *   11100:  signum
 *   11101:  zero-crossing pulser
 * 
 *   others:  (reserved)
 */

#define mod_mode(M) ((M >> 4) & 0x1f)
#define M_INACTIVE 0
#define M_NOISE 2
#define M_TRIGGERED_NOISE 3
#define M_LATCH 4
#define M_THRESHOLD 6
#define M_DELAY 7
#define M_TWO_POLE 8
#define M_TWO_POLE_M0 9
#define M_TWO_POLE_M1 11
#define M_TWO_ZERO 12
#define M_TWO_ZERO_M0 13
#define M_TWO_ZERO_M1 15
#define M_INTEGER_MIXING 16
#define M_ONE_POLE 17
#define M_MIXING 20
#define M_ONE_ZERO 22
#define M_MULTIPLY 24
#define M_AMP_MOD 25
#define M_MAX 26
#define M_MIN 27
#define M_SIGNUM 28
#define M_ZERO_CROSS 29

static double delay_read(int dly);
static void delay_write(int dly, double val);

static void process_mod(int mod)
{
  modifier *m;
  int mode, IS;
  double S, A, B, DM, tmp0, tmp1;

  m = mods[mod];
  mode = mod_mode(m->MMODE);
  if (mode == M_INACTIVE) 
    return;

  A = mod_read(m->MIN);
  B = mod_read(m->MRM);
  
  switch (mode)
    {
    case M_INACTIVE:
      /* 00000:	inactive.  S := 0 
       */
      break;

    case M_NOISE:
      /* 00010:	uniform noise.  S := L0 + L1*M0 (integer multiply, low-order
       *                        20 bits of product used; overflow ignored); L1 := S
       */
      IS = m->L0 + ((m->L1 * m->M0) & 0xfffff);
      mod_write(m->MSUM, TWOS_20_TO_DOUBLE(IS));
      m->L1 = IS;
      break;

    case M_TRIGGERED_NOISE:
      /* 00011:	triggered uniform noise.  S := L0 + L1*M0 (integer multiply,
       *          low-order 20 bits of product used; overflow ignored);
       *          if B*M1 (integer multiply, low-order 20 bits of product
       *          used; overflow ignored) is not 0, L1 := S
       */
      IS = m->L0 + ((m->L1 * m->M0) & 0xfffff);
      mod_write(m->MSUM, TWOS_20_TO_DOUBLE(IS));
      if (((DOUBLE_TO_TWOS_20(B) * m->M1) & 0xfffff) != 0)
	m->L1 = IS;
      break;

    case M_LATCH:
      /* 00100:	latch (sample and hold).  S := L1;  If B*M1 is not 0, L1 := A 
       *   but in the errata:
       *   BIL has discovered empirically that the modifier latch mode operation should actually read
       * 00100:	latch (sample and hold).  S := L1;  If B*M1 is not 0, L1 := A*M0
       */
      mod_write(m->MSUM, m->f_L1);
      if ((B * m->f_M1) != 0.0) m->f_L1 = A * m->f_M0;
      break;

    case M_THRESHOLD:
      /* 00110:	threshold.  If A*M0 + L0 is less than 0, then S := 0;
       *                    if A*M0 + L0 is equal to or greater than 0, then S := B*M1
       */
      tmp0 = A * m->f_M0 + m->f_L0;
      if (tmp0 < 0.0)
	mod_write(m->MSUM, 0.0);
      else mod_write(m->MSUM, B * m->f_M1);
      break;

    case M_DELAY:
      /* 00111:	invoke delay unit.
       *     Unit # := MRM (low-order 5 bits);
       *     S := L0 + L1*M0;  L0 := DM;  Temp0 := A + DM*M1;
       *     L1 := Temp0;  DM := Temp0
       */
      /* fprintf(stderr, "d%d, m%d: %.4f = %.4f + %.4f * %.4f\n", m->MRM & 0x1f, mod, m->f_L0 + m->f_L1 * m->f_M0, m->f_L0, m->f_L1, m->f_M0); */
      mod_write(m->MSUM, m->f_L0 + m->f_L1 * m->f_M0);
      m->f_L0 = delay_read(m->MRM & 0x1f);
      m->f_L1 = A + m->f_L0 * m->f_M1;
      delay_write(m->MRM & 0x1f, m->f_L1);
      break;
      
    case M_TWO_POLE:
    case M_TWO_POLE_M0:
    case M_TWO_POLE_M1:
      /* 01000:	two poles.               S := L1*M1 + L0*M0 + A; L0 := L1; L1 := S
       *
       * 01001:	two poles, M0 variable.  S := L1*M1 + L0*M0 + A; L0 := L1; L1 := S; M0 := M0 + B
       *
       * 01011:	two poles, M1 variable.  S := L1*M1 + L0*M0 + A; L0 := L1; L1 := S; M1 := M1 + B
       */
      tmp0 = m->f_L1 * m->f_M1;
      tmp1 = m->f_L0 * m->f_M0;
      S = tmp0 + tmp1 + A;
      mod_write(m->MSUM, S);
      m->f_L0 = m->f_L1;
      m->f_L1 = S;
      if (mode == M_TWO_POLE_M0)
	m->f_M0 += B;
      if (mode == M_TWO_POLE_M1)
	m->f_M1 += B;
      break;

    case M_TWO_ZERO:
    case M_TWO_ZERO_M0:
    case M_TWO_ZERO_M1:
      /* 01100:	two zeros.               S := L1*M1 + L0*M0 + A; L0 := L1; L1 := A
       *
       * 01101: two zeros, M0 variable.  S := L1*M1 + L0*M0 + A; L0 := L1; L1 := A; M0 := M0 + B
       *
       * 01101:	two zeros, M0 variable.  S := L1*M1 + L0*M0 + A; L0 := L1; L1 := A; M1 := M1 + B
       */
      tmp0 = m->f_L1 * m->f_M1;
      tmp1 = m->f_L0 * m->f_M0;
      mod_write(m->MSUM, tmp0 + tmp1 + A);
      m->f_L0 = m->f_L1;
      m->f_L1 = A;
      if (mode == M_TWO_ZERO_M0)
	m->f_M0 += B;
      if (mode == M_TWO_ZERO_M1)
	m->f_M1 += B;
      break;

    case M_INTEGER_MIXING:
      /* 10000:	integer mixing.  S := A*M0 + B*M1 (integer multiply, low-order
       * 20 bits of product used; overflow ignored)
       */
      /* I don't remember how we used this -- I'll assume the M's are the ints */
      mod_write(m->MSUM, A * m->M0 + B * m->M1);
      break;

    case M_MIXING:
      /* 10100:	mixing.  S := A*M0 + B*M1 
       */
      mod_write(m->MSUM, A * m->f_M0 + B * m->f_M1);
      break;

    case M_ONE_POLE:
      /* 10001:	one pole.  S := L1*M1 + B*M0; L1 := S
       *    but in the errata:
       *    DAJ - It seems that the modifier mode one pole is really
       * 10001:	one pole.  S := L1*M1 + B*L0; L1 := S
       */
      tmp0 = m->f_L1 * m->f_M1;
      tmp1 = B * m->f_L0;
      m->f_L1 = tmp0 + tmp1;
      mod_write(m->MSUM, m->f_L1);
      break;
      
    case M_ONE_ZERO:
      /* 10110:	one zero.  S := L1*M1 + L0*M0; L0 := L1; L1 := A
       */
      tmp0 = m->f_L1 * m->f_M1;
      tmp1 = m->f_L0 * m->f_M0;
      m->f_L0 = m->f_L1;
      m->f_L1 = A;
      mod_write(m->MSUM, tmp0 + tmp1);
      break;

    case M_MULTIPLY:
      /* 11000:	four-quadrant multiplication.  S := L1*M1; L1 := A*B
       */
      mod_write(m->MSUM, m->f_L1 * m->f_M1);
      m->f_L1 = A * B;
      break;

    case M_AMP_MOD:
      /* 11001:	amplitude modulation.  S := L1*M1;  L1 := A * ((B+1)/2)
       *        (The term ((B+1)/2) interprets B as a signed two's-complement
       *        fraction ranging in value from -1 to +1-epsilon.)
       */
      mod_write(m->MSUM, m->f_L1 * m->f_M1);
      m->f_L1 = A * (B + 1.0) * 0.5;
      break;

    case M_MAX:
      /* 11010:	maximum.  S := max (A*M0, B*M1) 
       */
      tmp0 = A * m->f_M0;
      tmp1 = B * m->f_M1;
      mod_write(m->MSUM, (tmp0 > tmp1) ? tmp0 : tmp1);
      break;

    case M_MIN:
      /* 11011:	minimum.  S := min (A*M0, B*M1) 
       */
      tmp0 = A * m->f_M0;
      tmp1 = B * m->f_M1;
      mod_write(m->MSUM, (tmp0 < tmp1) ? tmp0 : tmp1);
      break;

    case M_SIGNUM:
      /* 11100:	signum.  If A*M0 is less than B*M1, then S := -1 (integer)
       *                 if A*M0 equals B*M1, then S := 0;
       *                 if A*M0 is greater than B*M1, the S := 1 (integer)
       */
      tmp0 = A * m->f_M0;
      tmp1 = B * m->f_M1;
      if (tmp0 < tmp1) mod_write(m->MSUM, TWOS_20_TO_DOUBLE(-1));
      else if (tmp0 == tmp1) mod_write(m->MSUM, 0.0);
      else mod_write(m->MSUM, TWOS_20_TO_DOUBLE(1));
      break;
      
    case M_ZERO_CROSS:
      /* 11101:	zero-crossing pulser.  Temp0 := B*M0; Temp1 := L1*M1;
       *        if Temp1 is not 0 and either Temp0 is 0 or Temp0*Temp1 is
       *        negative then S := -epsilon, else S := 0; L1 := Temp0
       *        (The term -epsilon is a binary number with all bits set.)
       */
      tmp0 = B * m->f_M0;
      tmp1 = m->f_L1 * m->f_M0;
      if ((tmp1 != 0) &&
	  ((tmp0 == 0) || (tmp0 * tmp1 < 0)))
	mod_write(m->MSUM, TWOS_20_TO_DOUBLE(-1));
      m->f_L1 = tmp0;
      break;

    default:
      fprintf(stderr, "reserved modifier mode?\n");
      break;
    }
}



/* ---------------------------------------- delay processing ---------------------------------------- */

/*	Each delay unit has the following numeric parameters.
 * 
 * P  mode (4 bits).  The mode is interpreted as follows:
 * 		mode: 0000  inactive
 * 		      1000  delay line
 * 		      1010  table look-up
 * 		      1011  table look-up, argument rounded
 * 		      1100  delay tap
 * 		      others: (reserved)
 */

#define D_INACTIVE 0
#define D_LINE 8
#define D_TABLE_LOOKUP 10
#define D_TABLE_LOOKUP_ROUNDED 11
#define D_TAP 12

/*
 * Z  unit length (16 bits) or binary scale factor (4 bits).
 * 	In delay line and delay tap modes, Z gives 1 less than the 
 * 	total number of locations in delay memory used by the delay 
 * 	unit, i.e. the index of the last delay memory address for 
 * 	this unit.  In table look-up modes, the low-order four bits 
 * 	of Z specify the number of binary places that the argument 
 * 	is shifted to the right before it is used to address the 
 * 	memory; if rounding is specified, the address after shifting
 * 	is incremented by 1 if the most-significant bit shifted out
 * 	was a 1.
 * 
 * Y  index (16 bits).  In delay line and delay tap modes, this is the 
 * 	running index on the memory area for the unit.
 * 
 * X  base address (16 bits).  The base address is the lowest-numbered
 * 	delay memory location used by this unit.
 *
 * 	In inactive mode, delay memory is not modified and the unit
 * returns indeterminate results.  Delay units not accommodated due
 * to the number of ticks in a pass act as if in the inactive mode.
 * If the number of processing ticks is 4*n + m where m is 1, 2, or 3,
 * delay unit number n should be put in the inactive mode.
 * 
 * 	In delay line mode, a 20-bit data word is received from
 * the modifier that calls for the delay unit, and another 20-bit
 * word is sent to it.  The word received is put into the next slot
 * in the delay line.  It will be retrieved and sent back to the
 * modifier Z+3 passes later.  In delay tap mode, a word is sent to
 * the modifier but delay memory is not written into.
 * 
 * 	In table look-up mode, the 20-bit data word received
 * from the modifier is shifted to the right Z bits, bringing in zeros,
 * and the right 16 bits of the result are used to address the memory
 * area assigned to the unit.  The 20-bit word in the addressed memory
 * location is returned to the modifier three passes later.
 */

static double delay_read(int dly)
{
  delay *d;
  d = dlys[dly];
  switch (d->P)
    {
    case D_INACTIVE:
      return(0.0);
      
    case D_LINE:
    case D_TAP:
      /*
      fprintf(stderr, "d%d: read %d+%d -> %.4f\n", dly, d->X, d->Y, delay_memory[d->X + d->Y]);
      */
      return(delay_memory[d->X + d->Y]);
      
    case D_TABLE_LOOKUP:
    case D_TABLE_LOOKUP_ROUNDED:
      fprintf(stderr, "table lookup not implemented yet.\n");
    }
  return(0);
}

static void delay_write(int dly, double val)
{
  delay *d;
  d = dlys[dly];
  switch (d->P)
    {
    case D_INACTIVE:
      break;
      
    case D_LINE:
    case D_TAP:
      delay_memory[d->X + d->Y] = val;
      /*
      fprintf(stderr, "d%d: write %d+%d -> %.4f\n", dly, d->X, d->Y, val);
      */
      break;
      
    case D_TABLE_LOOKUP:
    case D_TABLE_LOOKUP_ROUNDED:
      fprintf(stderr, "table lookup not implemented yet.\n");
    }
}

static void process_dly(int dly)
{
  delay *d;
  d = dlys[dly];
  d->Y += 1;
  if (d->Y > d->Z) /* unit size - 1 so not >= ? */
    d->Y = 0;
}


/* ---------------------------------------- run! ---------------------------------------- */

static void linger(int time)
{
  /* process each sample ("pass") until pass == time */
  while (pass < time)
    {
      /* run through all available ticks, processing gen+mod+dly, 
       *   then write accumulated dac_outs, clear, update memories (this-pass -> last-pass),
       *   and increment pass 
       */
      int i, tick, gen = 0, mod = 0, dly = 0;
      for (tick = 0; tick < highest_tick_per_pass; tick++)
	{
	  /* given the timing info I'll simplify a bit and run 1 gen per tick, 1 mod every 2 ticks, and 1 delay every 4 ticks */
	  if (gen < 256)
	    process_gen(gen++);

	  if (((tick & 1) == 0) &&
	      (mod < 128))
	    process_mod(mod++);

	  if (((tick & 3) == 0) &&
	      (dly < 32))
	    process_dly(dly++);
	}

      /*
      fprintf(stderr, "--------------------------------------------------------------------------------\n");
      */
      for (i = 0; i < 64; i++)
	{
	  gen_ins[i] = gen_outs[i];
	  gen_outs[i] = 0.0;
	  mod_ins[i] = mod_outs[i];
	  mod_outs[i] = 0.0;
	}

      fwrite((void *)dac_out, 4, 4, snd_file);
      samples++;
      for (i = 0; i < 4; i++) dac_out[i] = 0.0;
      pass++;

      if (samples == TOTAL_SAMPLES) 
	all_done();

      /* we have a problem here.  In the good old days, we'd pad the end with a huge linger to hold the box
       *   until ESC (or some such command).  But in this system, that means we end up writing a lot of silence
       *   at the end of the file.
       */
    }
}


/* ---------------------------------------- commands ---------------------------------------- */

/* 
 *    -----------------------------------------------------------------
 *    :	      (20) data		: 0  0  0  0  0:  RR : x  x: W: P: S:
 *    -----------------------------------------------------------------
 * MISC
 *      RR: 00  no effect
 *          01  load DX from data
 *	    10  load TTL buffer A from left 16 bits of data
 *	    11  load TTL buffer B from left 16 bits of data
 *		  set analog output filters from right 4 bits of data:
 *		    01xx  Mode 0
 *		    00nn  Mode 1, frequency f0, f1, f2, or f3 according
 *			to nn
 *	W:  if 1, clear all wait bits
 *	P:  if 1, clear all pause bits
 *	S:  if 1, stop clock
 */

static void misc_command(int cmd)
{
  int data, RR, W, P, S;
  char *RR_name[4] = {"noop", "load DX", "TTL-A", "TTL-B"};

  data = LDB(cmd, 20, 12);
  RR = LDB(cmd, 2, 5);
  W = BIT(cmd, 2);
  P = BIT(cmd, 1);
  S = BIT(cmd, 0);

  if (DESCRIBE_COMMANDS)
    fprintf(stderr, "sam: %d, %s%s%s%s\n", 
	    data, 
	    RR_name[RR], 
	    (W == 1) ? "" : ", clear waits",
	    (P == 1) ? "" : ", clear pauses",
	    (S == 1) ? "" : ", stop clock");

  if (RR == 1) DX = data;

#if 0
  /* TODO: implement these bits */
  if (W == 1) fprintf(stderr, "clear all wait bits\n");
  if (P == 1) fprintf(stderr, "clear all pause bits\n");
  if (S == 1) fprintf(stderr, "stop clock!\n");
#endif
}


/*
 *    -----------------------------------------------------------------
 *    :	  (16) data	:(4)data: 0  0  0  0  1: U  U:  (5) unit #  :
 *    -----------------------------------------------------------------
 * DLY X, Y, Z
 *	UU:  00  X    16 bits base address; clear Y
 *	     01  Y    16 bits one's complement of index
 *	     10  Z,P  16 bits delay unit size minus 1, or scale (low
 *			  4 bits of 16); 4 bits mode
 *	     11  (unused)
 */

static void dly_command(int cmd)
{
  int unit, UU, data_4, data_16;
  delay *d;
  char *UU_name[4] = {"set base, clear index", "set index", "set size", "un-used!"};

  unit = (cmd & 0x1f);
  UU = LDB(cmd, 2, 5);
  data_4 = LDB(cmd, 4, 12);
  data_16 = LDB(cmd, 16, 16);
  
  d = dlys[unit];
  switch (UU)
    {
    case 0: 
      d->X = data_16;
      d->Y = 0;
      break;

    case 1:
      d->Y = data_16;
      break;

    case 2:
      d->Z = data_16;
      d->P = data_4;
      break;
    }

  if (DESCRIBE_COMMANDS)
    {
      fprintf(stderr, "d%d %s", unit, UU_name[UU]);
      if (UU == 0)
	fprintf(stderr, ": X: %d", d->X);
      else
	{
	  if (UU == 1)
	    fprintf(stderr, ": Y: %d", d->Y);
	  else fprintf(stderr, ": Z: %d, P: %d", d->Z, d->P);
	}
      fprintf(stderr, "\n");
    }
      

}


/*
 *    -----------------------------------------------------------------
 *    :	      (20) data		: 0  0  0  1  0: x  x: T  T: x  x  x:
 *    -----------------------------------------------------------------
 * TIMER
 *	TT: 00  no effect
 *	    10  Linger: process no further commands until pass counter
 *		    equals data
 *	    11  clear pass counter, then Linger as for 10
 *	    01  set pass counter from data
 */

static void timer_command(int cmd)
{
  int data, TT, bit_31;
  char *TT_name[4] = {"noop", "set pass", "linger", "clear pass and linger"};

  TT = LDB(cmd, 2, 3);
  data = LDB(cmd, 20, 12);

  if (DESCRIBE_COMMANDS)
    fprintf(stderr, "sam %s: %d\n", TT_name[TT], data);

  switch (TT)
    {
    case 0: 
      break;
    case 1: 
      pass = data; 
      break;
    case 2: 
      linger(data);
      break;
    case 3: 
      pass = 0; 
      linger(data);
      break;
    }
}


/*
 *   -----------------------------------------------------------------
 *   : xxx xxx xxx x : (10) data  : 0  0  0  1  1: x  x: 0: Q: x  x  x:
 *   -----------------------------------------------------------------
 * TICKS
 *	Q: 0  designate highest-numbered processing tick per pass
 *		    (should not exceed 255 [See appendix - DAJ])
 *	   1  designate next-to-highest-numbered tick (processing
 *	      plus overhead plus update) per pass
 */
static bool bit_31_warned = false;

static void ticks_command(int cmd)
{
  int Q, data, bit_31;
  char *Q_name[2] = {"set highest processing tick", "set highest tick"};

  bit_31 = BIT(cmd, 4);
  Q = BIT(cmd, 3);
  data = LDB(cmd, 10, 12);

  /* something is weird about some files -- Q==0, data=out chan number??, bit_31=1??
   *   I find this repeatedly -- was it a bug in the original assembler?  is the spec wrong?
   */
  if (bit_31 != 0) 
    {
      if (!bit_31_warned)
	{
	  fprintf(stderr, "ticks bit 31 is on?\n");
	  bit_31_warned = true;
	}
      return; /* what is going on here? */
    }
  if (data != 0) /* used at end of some box sequences, but that confuses srate */
    {
      if (Q == 0)
	highest_tick_per_pass = data;
      else highest_tick_per_pass = data + 2; 

      /* it's a 10 bit field, and higher bits are ignored, so the slowest we
       *   can run is 5010Hz or thereabouts
       */

      if (highest_tick_per_pass > 256)
	highest_tick_per_pass = 256;

      srate = (int)(1000000000.0 / (double)(highest_tick_per_pass * 195));
    }

  if (DESCRIBE_COMMANDS)
    fprintf(stderr, "sam %s: %d (%d Hz)\n", Q_name[Q], data, srate);

  if (data != 0)
    {
      if ((snd_file) && (samples == 0) && (Q == 1)) /* 2 tick commands at the start? */
	{
	  fclose(snd_file);                         /* start over... */
	  snd_file = NULL;
	}

      if (snd_file == NULL)
	{
	  /* now that we know the sampling rate, open the output file */
	  /* each tick takes 195 nsec, and there are 8 overhead ticks,
	   *   so our srate is 1000000000 / ((highest_tick + 8) * 195) 
	   */
	  int header_info[24] = {1179011410, 88, 1163280727, 1263424842,
				 28, 0, 0, 0,
				 0, 0, 0, 0,
				 544501094, 16, 262147, 44100,
				 705600, 2097168, 1635017060, 16,
				 0, 0, 0, 0};
	  header_info[15] = srate;
	  snd_file = fopen(OUTPUT_FILENAME, "w");
	  if (!snd_file)
	    {
	      fprintf(stderr, "can't open test.snd!\n");
	      exit(0);
	    }
	  fwrite((void *)header_info, 4, 24, snd_file);
	}
    }
}


/* GQ  (24 bits) phi -- decay exponent
 *    -----------------------------------------------------------------
 * GQ  :      (20) data         : 0  0  1: E:      (8)   gen #      :
 *    -----------------------------------------------------------------
 *
 *	E: 0  Q right-adjusted, sign extended
 *	   1  Q left-adjusted, low bits from left of DX; clear DX
 */

static void gq_command(int cmd)
{
  /* GQ is 24 bits */
  int data, E, gen;
  generator *g;
  char *E_name[2] = {"right adjusted", "left adjusted + DX"};

  gen = LDB(cmd, 8, 0);
  E = BIT(cmd, 8);
  data = LDB(cmd, 20, 12);

  g = gens[gen];
  if (E == 0)
    g->GQ = TWOS_20(data);
  else 
    {
      g->GQ = TWOS_24((data << 4) + (DX >> 16));
      DX = 0;
    }

  g->f_GQ = TWOS_24_TO_DOUBLE(g->GQ);

  if (DESCRIBE_COMMANDS)
    fprintf(stderr, "g%d amp: %s, %d = %d %.4f\n", gen, E_name[E], data, g->GQ, g->f_GQ);
}


/* GJ  (28 bits) omega -- oscillator frequency
 *    -----------------------------------------------------------------
 * GJ  :      (20) data         : 0  1  0: E:      (8)   gen #      :
 *    -----------------------------------------------------------------
 *
 *	E: 0  J right-adjusted, sign extended
 *	   1  J left-adjusted, low bits from left of DX; clear DX
 */

static void gj_command(int cmd)
{
  /* GJ is 28 bits */
  int data, E, gen;
  generator *g;
  char *E_name[2] = {"right adjusted", "left adjusted + DX"};

  gen = LDB(cmd, 8, 0);
  E = BIT(cmd, 8);
  data = LDB(cmd, 20, 12);

  g = gens[gen];
  if (E == 0)
    g->GJ = TWOS_20(data);
  else 
    {
      g->GJ = TWOS_28((data << 8) + (DX >> 12));
      DX = 0;
    }

  g->f_GJ = TWOS_28_TO_DOUBLE(g->GJ);

  if (DESCRIBE_COMMANDS)
    {
      if (E == 0)
	fprintf(stderr, "g%d freq: %s, %d = %d %.4f (%.4f Hz)\n", gen, E_name[E], data, g->GJ, g->f_GJ, g->f_GJ * 0.5 * srate);
      else fprintf(stderr, "g%d freq: %s (DX: %d), %d = %d %.4f (%.4f Hz)\n", gen, E_name[E], DX, data, g->GJ, g->f_GJ, g->f_GJ * 0.5 *srate);
    }
}


/* GP  (20 bits) delta -- decay rate
 *    -----------------------------------------------------------------
 * GP  :      (20) data         : 0  1  1  0:      (8)   gen #      :
 *    -----------------------------------------------------------------
 */

static gp_command(int cmd)
{
  /* GP is 20 bits */
  int data, gen;
  generator *g;

  gen = LDB(cmd, 8, 0);
  data = LDB(cmd, 20, 12);

  g = gens[gen];
  g->GP = TWOS_20(data);

  g->f_GP = TWOS_20_TO_DOUBLE(g->GP);

  if (DESCRIBE_COMMANDS)
    fprintf(stderr, "g%d amp change: %d = %d %.4f\n", gen, data, g->GP, g->f_GP);
}


/* GN  (11 bits) number of cosines to be summed
 * GM  (4 bits) binary scale of cosine or sum of cosines
 * GS  (1 bit) whether to shift output when adding to sum memory
 * GN, -----------------------------------------------------------------
 * GM, :N:M:S S:x: (11) GN :(4) GM : 0  1  1  1:      (8)   gen #      :
 * GS  -----------------------------------------------------------------
 *
 *	N:  if 1, disable loading GN
 *	M:  if 1, disable loading GM
 *	SS: 00  clear GS to 0
 *	    01  set GS to 1
 *	    10  no effect
 *	    11  (reserved)
 */

static void gn_command(int cmd)
{
  int N, M, SS, GN, GM, gen;
  generator *g;
  char *SS_name[4] = {", clear GS", ", set GS to 1", "", ", GS reserved?"};

  gen = LDB(cmd, 8, 0);
  GM = LDB(cmd, 4, 12);
  GN = LDB(cmd, 11, 16);
  SS = LDB(cmd, 2, 28);
  M = BIT(cmd, 30);
  N = BIT(cmd, 31);

  if (DESCRIBE_COMMANDS)
    {
      if (N == 1)
	{
	  if (M == 1)
	    fprintf(stderr, "g%d sum-memory shift:%s\n", gen, SS_name[SS]);
	  else fprintf(stderr, "g%d ncos scale: %d%s\n", gen, GM, SS_name[SS]);
	}
      else
	{
	  if (M == 1)
	    fprintf(stderr, "g%d ncos: %d%s\n", gen, GN, SS_name[SS]);
	  else fprintf(stderr, "g%d ncos+scale: %d, %d%s\n", gen, GN, GM, SS_name[SS]);
	}
    }

  g = gens[gen];
  if (N == 0)
    g->GN = GN;
  if (M == 0)
    g->GM = GM;

  switch(SS)
    {
    case 0:
      g->GS = 0;
      break;
    case 1:
      g->GS = 1;
      break;
    }
}


/* GL  (12 bits) asymptote
 * GSUM  (6 bits) sum memory address into which output is added
 *      -----------------------------------------------------------------
 * GL,  :L:S:  (12) GL   : (6) GSUM : 1  0  0  0:      (8)   gen #      :
 * GSUM -----------------------------------------------------------------
 *
 *	L:  if 1, disable loading GL
 *	S:  if 1, disable loading GSUM
 */

static void gl_command(int cmd)
{
  int GL, GSUM, L, S, gen;
  generator *g;

  gen = LDB(cmd, 8, 0);
  GSUM = LDB(cmd, 6, 12);
  GL = LDB(cmd, 12, 18);
  L = BIT(cmd, 31);
  S = BIT(cmd, 30);

  g = gens[gen];

  if (L == 0)
    {
      g->GL = GL; /* this is apparently a 12-bit unsigned fraction */
      g->f_GL = UNSIGNED_12_TO_DOUBLE(g->GL);
    }

  if (S == 0)
    g->GSUM = GSUM;

  if (DESCRIBE_COMMANDS)
    {
      if (L == 1)
	{
	  if (S == 1)
	    fprintf(stderr, "g%d: noop\n", gen);
	  else fprintf(stderr, "g%d outloc: %d\n", gen, g->GSUM);
	}
      else
	{
	  if (S == 0)
	    fprintf(stderr, "g%d amp offset: %d = %.4f\n", gen, g->GL, g->f_GL);
	  else fprintf(stderr, "g%d outloc + amp offset:  %d, %d = %.4f\n", gen, g->GSUM, g->GL, g->f_GL);
	}
    }
}


/* (20 bits) theta -- oscillator angle
 *    -----------------------------------------------------------------
 * GK  :      (20) data         : 1  0  0  1:      (8)   gen #      :
 *    -----------------------------------------------------------------
*/

static gk_command(int cmd)
{
  /* GK is 20 bits */
  int data, gen;
  generator *g;

  gen = LDB(cmd, 8, 0);
  data = LDB(cmd, 20, 12);

  g = gens[gen];
  g->GK = TWOS_20(data);

  g->f_GK = TWOS_20_TO_DOUBLE(g->GK);

  if (DESCRIBE_COMMANDS)
    fprintf(stderr, "g%d phase: %d = %d %.4f\n", gen, data, g->GK, g->f_GK);
}


/* GFM  (7 bits) sum memory address from which frequency modulation
 * GMODE  (10 bits) generator mode
 *    -----------------------------------------------------------------
 *    :M:F:C:  (10) GMODE :(7) GFM: 1  0  1  0:      (8)   gen #      :
 *    -----------------------------------------------------------------
 * GMODE,
 * GFM	M:  if 1, disable loading GMODE
 *	F:  if 1, disable loading GFM
 *	C:  if 1, clear GK
 */

static char *print_GMODE_name(mode)
{
  /* RRRREESSSS */
  int R, E, S;
  char *E_name[4] = {"L-Q", "L+Q", "L-2^Q", "L+2^Q"};

  R = osc_run(mode);
  E = osc_env(mode);
  S = osc_mode(mode);

  if (R == 0)
    {
      fprintf(stderr, "inactive");
      return;
    }

  if ((R != 2) && (R != 7) && (R != 3))
    {
      switch (S)
	{
	case SUMCOS:   fprintf(stderr, "sum of cosines");    break;
	case SAWTOOTH: fprintf(stderr, "sawtooth wave");     break;
	case SQUARE:   fprintf(stderr, "square wave");       break;
	case PULSE:    fprintf(stderr, "pulse train");       break;
	case SIN_K:    fprintf(stderr, "sin");               break;
	case SIN_FM:   fprintf(stderr, "sin+fm");            break;
	default:       fprintf(stderr, "unknown osc mode!"); break;
	}
      
      fprintf(stderr, ", %s", E_name[E]);
    }

  switch (R)
    {
    case 1:  fprintf(stderr, ", pause");             break;
    case 15: fprintf(stderr, ", run A (normal)");    break;
    case 14: fprintf(stderr, ", run B (trigger)");   break;
    case 9:  fprintf(stderr, ", wait");              break;
    case 13: fprintf(stderr, ", run C (trigger)");   break;
    case 7:  fprintf(stderr, ", read-data");         break;
    case 3:  fprintf(stderr, ", write-data");        break;
    case 2:  fprintf(stderr, ", write DAC");         break;
    default: fprintf(stderr, ", unknown run mode!"); break;
    }
}

static gmode_command(int cmd)
{
  int gen, M, F, C, GMODE, GFM;
  generator *g;

  gen = LDB(cmd, 8, 0);
  GFM = LDB(cmd, 7, 12);
  GMODE = LDB(cmd, 10, 19);
  M = BIT(cmd, 31);
  F = BIT(cmd, 30);
  C = BIT(cmd, 29);

  g = gens[gen];
  if (M == 0)
    {
      g->GMODE = GMODE;
      if (GMODE == 979) /* looks like a bug... */
	{
	  if (!pulse_warned)
	    {
	      fprintf(stderr, "GMODE is pulse!?!\n");
	      pulse_warned = true;
	    }
	  GMODE = 976;
	}
    }

  if (F == 0)
    g->GFM = GFM;
  if (C == 1)
    g->GK = 0;

  if (DESCRIBE_COMMANDS)
    {
      fprintf(stderr, "g%d %s%s%s%s%s:", 
	      gen,
	      (M == 0) ? "mode" : "",
	      ((M == 0) && ((F == 0) || (C == 1))) ? "/" : "",
	      (F == 0) ? "inloc" : "",
	      ((F == 0) && (C == 1)) ? "/" : "",
	      (C == 1) ? "phase" : "");
      if (M == 0)
	print_GMODE_name(g->GMODE);
      if (F == 0)
	fprintf(stderr, ", inloc: %s[%d]", ((g->GFM >> 6) == 0) ? "gen-ins" : "mod-ins", g->GFM & 0x3f);
      if (C == 1)
	fprintf(stderr, ", clear phase");
      fprintf(stderr, "\n");
    }
}


/* GO  (20 bits) alpha -- oscillator frequency sweep rate
 *     -----------------------------------------------------------------
 * GO  :      (20) data         : 1  0  1  1:      (8)   gen #      :
 *     -----------------------------------------------------------------
*/

static go_command(int cmd)
{
  /* GO is 20  bits */
  int data, gen;
  generator *g;

  gen = LDB(cmd, 8, 0);
  data = LDB(cmd, 20, 12);

  g = gens[gen];
  g->GO = TWOS_20(data);

  g->f_GO = TWOS_20_TO_DOUBLE(g->GO);

  if (DESCRIBE_COMMANDS)
    {
      if (osc_run(g->GMODE) == 2)
	fprintf(stderr, "g%d DAC out: %d\n", gen, data);
      else fprintf(stderr, "g%d freq change: %d = %d %.4f (%.4f Hz)\n", gen, data, g->GO, g->f_GO, g->f_GO * 0.5 * srate / 256.0);
    }
}


/* M0  (30 bits) coefficient
 * M1  (30 bits) other coefficient
 *    -----------------------------------------------------------------
 * MM  :      (20) data         : 1  1  0: V  V:     (7)   mod #    :
 *    -----------------------------------------------------------------
 *
 *	VV: 00  M0 right-adjusted, sign extended
 *	    01  M1 right-adjusted, sign extended
 *	    10  M0 left-adjusted, low bits from left of DX; clear DX
 *	    11  M1 left-adjusted, low bits from left of DX; clear DX
 */

/* To avoid endless repetition in the modifier processing, I'll incorporate the scalers
 *   into M0 and M1 when they are set, or when the scalers are changed, but this means
 *   (for simplicity) keeping track of the oiginal M0 and M1 values "o_M0" and friends)
 */

static void mm_command(int cmd)
{
  /* M0 and M1 are 30 bits */
  int mod, VV, data;
  modifier *m;

  mod = LDB(cmd, 7, 0);
  VV = LDB(cmd, 2, 7);
  data = LDB(cmd, 20, 12);

  m = mods[mod];

  switch (VV)
    {
    case 0:
      m->M0 = TWOS_20(data);
      m->f_M0 = TWOS_20_TO_DOUBLE(data);
      m->o_M0 = m->M0;
      m->o_f_M0 = m->f_M0;
      m->M0 *= m->mult_scl_0 / 4;
      m->f_M0 *= m->mult_scl_0;
      break;

    case 1:
      m->M1 = TWOS_20(data);
      m->f_M1 = TWOS_20_TO_DOUBLE(data);
      m->o_M1 = m->M1;
      m->o_f_M1 = m->f_M1;
      m->M1 *= m->mult_scl_1 / 4;
      m->f_M1 *= m->mult_scl_1;
      break;

    case 2:
      m->M0 = (data << 10) + (DX >> 10);
      m->f_M0 = TWOS_30_TO_DOUBLE((data << 10) + (DX >> 10));
      m->o_M0 = m->M0;
      m->o_f_M0 = m->f_M0;
      DX = 0;
      m->M0 *= m->mult_scl_0 / 4;
      m->f_M0 *= m->mult_scl_0;
      break;

    case 3:
      m->M1 = (data << 10) + (DX >> 10);
      m->f_M1 = TWOS_30_TO_DOUBLE((data << 10) + (DX >> 10));
      m->o_M1 = m->M1;
      m->o_f_M1 = m->f_M1;
      DX = 0;
      m->M1 *= m->mult_scl_1 / 4;
      m->f_M1 *= m->mult_scl_1;
      break;
    }

  if (DESCRIBE_COMMANDS)
    {
      switch (VV)
	{
	case 0: fprintf(stderr, "m%d M0: %d: %d %.6f\n", mod, data, m->M0, m->f_M0); break;
	case 1: fprintf(stderr, "m%d M1: %d: %d %.6f\n", mod, data, m->M1, m->f_M1); break;
	case 2: fprintf(stderr, "m%d M0+DX: %d, DX: %d: %d %.6f\n", mod, data, DX, m->M0, m->f_M0); break;
	case 3: fprintf(stderr, "m%d M1+DX: %d, DX: %d: %d %.6f\n", mod, data, DX, m->M1, m->f_M1); break;
	}
    }
}


/* L0  (20 bits) running term
 * L1  (20 bits) other running term
 *    -----------------------------------------------------------------
 * ML :      (20) data         : 1  1  1  0: N:     (7)   mod #    :
 *    -----------------------------------------------------------------
 *
 *	N: 0  L0
 *	   1  L1
*/

static void ml_command(int cmd)
{
  int mod, N, data;
  modifier *m;

  mod = LDB(cmd, 7, 0);
  data = LDB(cmd, 20, 12);
  N = BIT(cmd, 7);

  m = mods[mod];
  if (N == 0)
    {
      m->L0 = TWOS_20(data);
      m->f_L0 = TWOS_20_TO_DOUBLE(data);
    }
  else 
    {
      m->L1 = TWOS_20(data);
      m->f_L0 = TWOS_20_TO_DOUBLE(data);
    }

  if (DESCRIBE_COMMANDS)
    {
      if (N == 0)
	fprintf(stderr, "m%d L0: %d: %d %.6f\n", mod, data, m->L0, m->f_L0);
      else fprintf(stderr, "m%d L1: %d: %d %.6f\n", mod, data, m->L1, m->f_L1);
    }
}


/* MSUM  (7 bits) result address in sum memory
 * MMODE  (9 bits) modifier mode
 *    -----------------------------------------------------------------
 *    :M:S:C:H: (9) MMODE :(7)MSUM: 1  1  1  1  0:      (7)  mod #    :
 *    -----------------------------------------------------------------
 *
 * MMODE,
 * MSUM	M:  if 1, disable loading MMMMM bits of MMODE
 *	S:  if 1, disable loading MSUM
 *	C:  if 1, clear L0
 *	H:  if 1, disable loading AABB bits of MMODE
 */

static const char *mode_name(int m)
{
  switch (m)
    {
    case 0: return("inactive");
    case 2: return("uniform noise");
    case 3: return("triggered noise");
    case 4: return("latch");
    case 6: return("threshold");
    case 7: return("invoke delay");
    case 8: return("two pole");
    case 9: return("two pole, M0 variable");
    case 11: return("two pole, M1 variable");
    case 12: return("two zero");
    case 13: return("two zero, M0 variable");
    case 15: return("two zero, M1 variable");
    case 16: return("integer mix");
    case 17: return("one pole");
    case 20: return("mix");
    case 22: return("one zero");
    case 24: return("four-quadrant multiply");
    case 25: return("amplitude modulation");
    case 26: return("maximum");
    case 27: return("minimum");
    case 28: return("signum");
    case 29: return("zero-crossing pulser");
    }
  return("unknown");
}

static void mmode_command(int cmd)
{
  int mod, MSUM, MMODE, M, S, C, H;
  modifier *m;

  mod = LDB(cmd, 7, 0);
  MSUM = LDB(cmd, 7, 12);
  MMODE = LDB(cmd, 9, 19);
  M = BIT(cmd, 31);
  S = BIT(cmd, 30);
  C = BIT(cmd, 29);
  H = BIT(cmd, 28);

  m = mods[mod];
  if (S == 0)
    m->MSUM = MSUM;
  if (C == 1)
    {
      m->L0 = 0;
      m->f_L0 = 0.0;
    }

  /* MMODE is MMMMMAABB */
  if (H == 0)
    {
      /* set up the scale factors now, so we don't have to futz around later */
      /* BB = first (!) */
      m->mult_scl_1 = (1 << (MMODE & 0x3));
      m->mult_scl_0 = (1 << ((MMODE >> 2) & 0x3));
      /* whenever M0/M1 are set, we will include these factors */

      m->M0 = m->o_M0 * m->mult_scl_0 / 4;
      m->M1 = m->o_M1 * m->mult_scl_1 / 4;
      m->f_M0 = m->o_f_M0 * m->mult_scl_0;
      m->f_M1 = m->o_f_M1 * m->mult_scl_1;

      if (M == 0)
	m->MMODE = MMODE;                                  /* set both */
      else m->MMODE = (MMODE & 0xf) + (m->MMODE & 0x1f0); /* H is 0, so set AABB */
    }
  else
    {
      if (M == 0)
	m->MMODE = (MMODE & 0x1f0) + (m->MMODE & 0xf);    /* M is 0, so set MMMMM */
    }

  if (DESCRIBE_COMMANDS)
    {
      fprintf(stderr, "m%d mode:", mod);
      if (M == 0)
	fprintf(stderr, ", %s", mode_name(MMODE >> 4));
      if (H == 0)
	fprintf(stderr, ", AA: %d, BB: %d", (MMODE >> 2) & 0x3, MMODE & 0x3);
      if (S == 0)
	fprintf(stderr, ", outloc(%s): %d", ((MSUM >> 6) == 0) ? "+" : "=", MSUM & 0x3f);
      if (C == 1)
	fprintf(stderr, ", L0=0");
      fprintf(stderr, "\n");
    }
}


/* MIN  (8 bits) address in sum memory where modifier reads "A" data
 * MRM  (8 bits) address in sum memory where modifier reads "B" data
 *    -----------------------------------------------------------------
 *    :R:I:C C: (8) MRM : (8) MIN : 1  1  1  1  1:      (7)  mod #    :
 *    -----------------------------------------------------------------
 *
 * MRM,
 * MIN,	R:  if 1, disable loading MRM
 * MT	I:  if 1, disable loading MIN
 *	CC: 00  turn off truncation
 *	    01  turn on truncation
 *	    10  clear L1
 *	    11  no effect
 */

static void mrm_command(int cmd)
{
  int mod, MRM, MIN, R, I, CC;
  modifier *m;

  mod = LDB(cmd, 7, 0);
  MIN = LDB(cmd, 8, 12);
  MRM = LDB(cmd, 8, 20);
  R = BIT(cmd, 31);
  I = BIT(cmd, 30);
  CC = LDB(cmd, 2, 28);

  m = mods[mod];
  if (R == 0)
    m->MRM = MRM;
  if (I == 0)
    m->MIN = MIN;
  switch (CC)
    {
    case 0: 
      m->T = 0;
      break;
    case 1:
      m->T = 1;
      break;
    case 2:
      m->L1 = 0;
      m->f_L1 = 0.0;
      break;
    }

  if (DESCRIBE_COMMANDS)
    {
      fprintf(stderr, "m%d inlocs:", mod);
      if (R == 0)
	{
	  fprintf(stderr, ", MRM: ");
	  print_mod_read_name(MRM);
	}
      if (I == 0)
	{
	  fprintf(stderr, ", MIN: ");
	  print_mod_read_name(MIN);
	}
      if (CC == 0) fprintf(stderr, ", trunc off");
      if (CC == 1) fprintf(stderr, ", trunc on");
      if (CC == 2) fprintf(stderr, ", L1=0");
      fprintf(stderr, "\n");
    }
}


static void handle_command(int cmd)
{
  int op;
  op = LDB(cmd, 4, 8);
  
  switch (op)
    {
    case 0: 
      if (BIT(cmd, 7) == 1)
	dly_command(cmd); 
      else misc_command(cmd);
      break;

    case 1:
      if (BIT(cmd, 7) == 1)
	ticks_command(cmd);
      else timer_command(cmd);
      break;

    case 2: case 3: 
      gq_command(cmd); 
      break;

    case 4: case 5: 
      gj_command(cmd); 
      break;

    case 6: 
      gp_command(cmd); 
      break;

    case 7: 
      gn_command(cmd); 
      break;

    case 8:
      gl_command(cmd);
      break;

    case 9: 
      gk_command(cmd); 
      break;

    case 10: 
      gmode_command(cmd);
      break;

    case 11: 
      go_command(cmd);
      break;

    case 12: case 13: 
      mm_command(cmd);
      break;

    case 14: 
      ml_command(cmd);
      break;

    case 15: 
      if (BIT(cmd, 7) == 0)
	mmode_command(cmd);
      else mrm_command(cmd);
      break;

    default: 
      fprintf(stderr, "impossible command\n"); 
      break;
    }
}


/* ---------------------------------------- main program ---------------------------------------- */

int main(int argc, char **argv)
{
  if (argc < 2)
    fprintf(stderr, "sam filename\n");
  else
    {
      FILE *sam_file;
      char *filename;
      filename = argv[1];

      sam_file = fopen(filename, "r");
      if (!sam_file)
	fprintf(stderr, "can't find %s\n", filename);
      else
	{
	  long size;
	  fseek(sam_file, 0, SEEK_END);
	  size = ftell(sam_file);
	  rewind(sam_file);

	  if (size <= 0)
	    {
	      fprintf(stderr, "%s is empty\n");
	      fclose(sam_file);
	    }
	  else
	    {
	      size_t bytes;
	      unsigned char *command;
	      int i, j;

	      start_clean();

	      command = (unsigned char *)calloc(size + 1, sizeof(unsigned char));
	      bytes = fread(command, sizeof(unsigned char), size, sam_file);
	      fclose(sam_file);

	      /* these were stored in at least 2 different formats
	       *
	       * FASTF.SAM:  "Type: 32BITR BADSAM ;Looks like a SAM command file but has questionable data"
	       * MACDON.SAM: "Type: SAM SIMPLE    ;Simple SAM command file (corresponding sound file possible)"
	       *
	       * FASTF was written as 32 bits (using the 1st case below), and MACDON as 36 (using the 2nd case).
	       * it looks like someone got a flag backwards, and wrote the known-good 32-bit files as 36,
	       *    and the possibly not-32 bit files as 32. I can't find the corresponding code in the writers
	       *    that Nando found on the exabyte tapes.  (I think FASTF does indeed have a bad command).
	       */

	      if ((command[0] != 0) || /* just a first guess */
		  (command[1] != 0))
		{
		  fprintf(stderr, "32\n");
		  for (i = 0; i < bytes; i += 4)
		    {
		      int cmd;
		      int b1, b2, b3, b4;
		      b1 = command[i + 0];
		      b2 = command[i + 1];
		      b3 = command[i + 2];
		      b4 = command[i + 3];
		      cmd = b4 + (b3 << 8) + (b2 << 16) + (b1 << 24);
		      handle_command(cmd);
		    }
		}
	      else
		{
		  fprintf(stderr, "36\n");
		  for (i = 0; i < bytes; i += 5)
		    {
		      int cmd;
		      int b1, b2, b3, b4, b5;
		      b1 = command[i + 0];
		      b2 = command[i + 1];
		      b3 = command[i + 2];
		      b4 = command[i + 3];
		      b5 = command[i + 4];
		      cmd = ((b5 >> 4) & 0xff) + (b4 << 4) + (b3 << 12) + (b2 << 20) + ((b1 & 0xff) << 28);
		      handle_command(cmd);
		    }
		}
	    }
	  
	  all_done();
	}
    }
  return(0);
}

