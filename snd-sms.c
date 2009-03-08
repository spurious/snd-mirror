/* libSMS support for Snd */

#include "snd.h"

#if HAVE_LIBSMS && HAVE_EXTENSION_LANGUAGE

#include <sms.h>

/* libsms by Xavier Serra and R. Eakin */

/* in the 80's or was it the early 90's, I wrote a Common Lisp version of libsms -- seems like an old friend. */
/*   Michael Klingbeil has made a very nice GUI for this kind of work -- can't remember what he calls it ("spear"?). */
/*   There's almost no way I can match that, so I think I'll just provide the usual hooks in the library */


/* TODO: send reakin a note about:
   sms.h defines PI and friends, also MIN/MAX -- these should be in a header private to the sms library.
   sms.h should define a string, or possibly a group of ints for the version, not a float! (also the tarfile says 1.02, but version=1.0)
 */

const char *snd_sms_version(void)
{
  /* can't include sms.h in any other file, so pick this up here (for snd-help.c) */
  if (SMS_VERSION == 1.0)
    return("1.0");
  return("unknown version");
}


#define S_sms_init "sms_init"

static XEN g_sms_init(void)
{
  #define H_sms_init "(" S_sms_init ") initializes some SMS global data, returns either SMS_OK or SMS_MALLOC"
  return(C_TO_XEN_INT(sms_init()));
}


#define S_sms_free "sms_free"

static XEN g_sms_free(void)
{
  #define H_sms_free "(" S_sms_free ") deallocates SMS global data"
  sms_free();
  return(XEN_FALSE);
}


#define S_sms_clearSine "sms_clearSine"

static XEN g_sms_clearSine(void)
{
  #define H_sms_clearSine "(" S_sms_clearSine ") deallocates the SMS global sine table"
  sms_clearSine();
  return(XEN_FALSE);
}


#define S_sms_clearSinc "sms_clearSinc"

static XEN g_sms_clearSinc(void)
{
  #define H_sms_clearSinc "(" S_sms_clearSinc ") deallocates the SMS global sinc table"
  sms_clearSinc();
  return(XEN_FALSE);
}


#define S_sms_prepSine "sms_prepSine"

static XEN g_sms_prepSine(XEN p)
{
  #define H_sms_prepSine "(" S_sms_prepSine " size) prepares the SMS sine table of the given size"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(p), p, XEN_ONLY_ARG, S_sms_prepSine, "int");
  return(C_TO_XEN_INT(sms_prepSine(XEN_TO_C_INT(p))));
}


#define S_sms_prepSinc "sms_prepSinc"

static XEN g_sms_prepSinc(XEN p)
{
  #define H_sms_prepSinc "(" S_sms_prepSinc " size) prepares the SMS sinc table of the given size"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(p), p, XEN_ONLY_ARG, S_sms_prepSinc, "int");
  return(C_TO_XEN_INT(sms_prepSinc(XEN_TO_C_INT(p))));
}


#define S_sms_random "sms_random"

static XEN g_sms_random(void)
{
  #define H_sms_random "(" S_sms_random ") returns a random float between -1.0 and 1.0"
  return(C_TO_XEN_DOUBLE((double)sms_random()));
}


#define S_sms_power2 "sms_power2"

static XEN g_sms_power2(XEN p)
{
  #define H_sms_power2 "(" S_sms_power2 " p) returns a power of 2 greater than or equal to its argument"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(p), p, XEN_ONLY_ARG, S_sms_power2, "int");
  return(C_TO_XEN_INT(sms_power2(XEN_TO_C_INT(p))));
}


#define S_sms_sine "sms_sine"

static XEN g_sms_sine(XEN p)
{
  #define H_sms_sine "(" S_sms_sine " angle) returns sin(angle), angle in radians"
  XEN_ASSERT_TYPE(XEN_DOUBLE_P(p), p, XEN_ONLY_ARG, S_sms_sine, "float");
  return(C_TO_XEN_DOUBLE(sms_sine(XEN_TO_C_DOUBLE(p))));
}


#define S_sms_sinc "sms_sinc"

static XEN g_sms_sinc(XEN p)
{
  #define H_sms_sinc "(" S_sms_sinc " angle) returns sinc(angle), angle in radians, between 0 and 8"
  XEN_ASSERT_TYPE(XEN_DOUBLE_P(p), p, XEN_ONLY_ARG, S_sms_sinc, "float between 0.0 and 8.0");
  return(C_TO_XEN_DOUBLE(sms_sinc(XEN_TO_C_DOUBLE(p))));
}


#define S_sms_errorString "sms_errorString"

static XEN g_sms_errorString(XEN p)
{
  #define H_sms_errorString "(" S_sms_errorString " p) returns a description of an error code"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(p), p, XEN_ONLY_ARG, S_sms_errorString, "int");
  return(C_TO_XEN_STRING(sms_errorString(XEN_TO_C_INT(p))));
}


#define S_sms_writeSF "sms_writeSF"

static XEN g_sms_writeSF(void)
{
  #define H_sms_writeSF "(" S_sms_writeSF ") closes the current output sound file"
  sms_writeSF();
  return(XEN_FALSE);
}


#define S_sms_preEmphasis "sms_preEmphasis"

static XEN g_sms_preEmphasis(XEN p)
{
  #define H_sms_preEmphasis "(" S_sms_preEmphasis " x) returns x pre-emphasized (filtered)"
  XEN_ASSERT_TYPE(XEN_DOUBLE_P(p), p, XEN_ONLY_ARG, S_sms_preEmphasis, "float");
  return(C_TO_XEN_DOUBLE(sms_preEmphasis(XEN_TO_C_DOUBLE(p))));
}


#define S_sms_deEmphasis "sms_deEmphasis"

static XEN g_sms_deEmphasis(XEN p)
{
  #define H_sms_deEmphasis "(" S_sms_deEmphasis " x) returns x de-emphasized (filtered)"
  XEN_ASSERT_TYPE(XEN_DOUBLE_P(p), p, XEN_ONLY_ARG, S_sms_deEmphasis, "float");
  return(C_TO_XEN_DOUBLE(sms_deEmphasis(XEN_TO_C_DOUBLE(p))));
}


#define S_sms_createSF "sms_createSF"

static XEN g_sms_createSF(XEN filename, XEN srate, XEN htype)
{
  #define H_sms_createSF "(" S_sms_createSF " filename srate header-type) creates an output sound file. \
The 'header-type' is either 0 for wav, or 1 for aiff"

  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ARG_1, S_sms_createSF, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(srate), srate, XEN_ARG_2, S_sms_createSF, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(htype), htype, XEN_ARG_3, S_sms_createSF, "0 for wav, or 1 for aiff");
  return(C_TO_XEN_INT(sms_createSF((char *)XEN_TO_C_STRING(filename), XEN_TO_C_INT(srate), XEN_TO_C_INT(htype))));
}




/* -------------------------------- float arrays -------------------------------- */

/* vcts might be double* not float*, so I guess the only reasonable way to accommodate libSMS
 *  is to make/free opaque arrays of floats -- kinda ugly!
 */

#define C_TO_XEN_floats(Value) ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("floats"), XEN_WRAP_C_POINTER(Value)) : XEN_FALSE)
#define XEN_TO_C_floats(Value) ((float *)(XEN_UNWRAP_C_POINTER(XEN_CADR(Value))))
#define XEN_floats_P(Value)    (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                                  (strcmp("floats", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))

#define S_sms_make_floats "sms_make_floats"

static XEN g_sms_make_floats(XEN size)
{
  #define H_sms_make_floats "(" S_sms_make_floats " size) returns a new (calloc'd) array of 'size' C floats"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(size), size, XEN_ARG_1, S_sms_make_floats, "an integer");
  return(C_TO_XEN_floats((float *)calloc(XEN_TO_C_INT(size), sizeof(float *))));
}
  

#define S_sms_free_floats "sms_free_floats"

static XEN g_sms_free_floats(XEN ptr)
{
  #define H_sms_free_floats "(" S_sms_free_floats " ptr) frees the pointer"
  XEN_ASSERT_TYPE(XEN_floats_P(ptr), ptr, XEN_ONLY_ARG, S_sms_free_floats, "pointer to an array of C floats");
  free(XEN_TO_C_floats(ptr));
  return(XEN_FALSE);
}


#define S_sms_writeSound "sms_writeSound"

static XEN g_sms_writeSound(XEN buffer, XEN size)
{
  #define H_sms_writeSound "(" S_sms_writeSound " buf size) writes buf to the sound output file"
  XEN_ASSERT_TYPE(XEN_floats_P(buffer), buffer, XEN_ARG_1, S_sms_writeSound, "an opaque float* array (from " S_sms_make_floats ")");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(size), size, XEN_ARG_2, S_sms_writeSound, "an integer");
  sms_writeSound(XEN_TO_C_floats(buffer), XEN_TO_C_INT(size));
  return(XEN_FALSE);
}


#if 0
int sms_spectralApprox (float *pFSpec1, int sizeSpec1, int sizeSpec1Used, float *pFSpec2, int sizeSpec2, int nCoefficients);
int sms_quickSpectrum (float *pFWaveform, float *pFWindow, int sizeWindow, float *pFMagSpectrum, float *pFPhaseSpectrum, int sizeFft);
int sms_invQuickSpectrum (float *pFMagSpectrum, float *pFPhaseSpectrum, int sizeFft, float *pFWaveform, int sizeWave);
int sms_invQuickSpectrumW (float *pFMagSpectrum, float *pFPhaseSpectrum, int sizeFft, float *pFWaveform, int sizeWave,  float *pFWindow);
void sms_rdft(int sizeFft, float *pRealArray, int direction );
void sms_getWindow (int sizeWindow, float *pFWindow, int iWindowType);
#endif



/* -------------------------------- SMS_Data -------------------------------- */

#if HAVE_SCHEME || HAVE_CL
  #define FIELD_PREFIX "."
#endif
#if HAVE_RUBY
  #define FIELD_PREFIX "R"
#endif
#if HAVE_FORTH
  #define FIELD_PREFIX "F"
#endif


#define C_TO_XEN_SMS_Data(Value) ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("SMS_Data"), XEN_WRAP_C_POINTER(Value)) : XEN_FALSE)
#define XEN_TO_C_SMS_Data(Value) ((SMS_Data *)(XEN_UNWRAP_C_POINTER(XEN_CADR(Value))))
#define XEN_SMS_Data_P(Value)    (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                                  (strcmp("SMS_Data", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))

#define S_sms_make_SMS_Data "sms_make_SMS_Data"

static XEN g_sms_make_SMS_Data(void)
{
  #define H_sms_make_SMS_Data "(" S_sms_make_SMS_Data ") returns a new (malloc'd) SMS_Data pointer"
  return(C_TO_XEN_SMS_Data((SMS_Data *)malloc(sizeof(SMS_Data))));
}
  

#define S_sms_free_SMS_Data "sms_free_SMS_Data"

static XEN g_sms_free_SMS_Data(XEN ptr)
{
  #define H_sms_free_SMS_Data "(" S_sms_free_SMS_Data " ptr) frees the pointer"
  XEN_ASSERT_TYPE(XEN_SMS_Data_P(ptr), ptr, XEN_ONLY_ARG, S_sms_free_SMS_Data, "SMS_Data pointer");
  free(XEN_TO_C_SMS_Data(ptr));
  return(XEN_FALSE);
}


static XEN g_SMS_Data_nTracks(XEN ptr)
{
  #define H_SMS_Data_nTracks "(" FIELD_PREFIX "nTracks SMS_Data) returns the number of sinusoidal tracks in the frame"
  XEN_ASSERT_TYPE(XEN_SMS_Data_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "nTracks", "SMS_Data pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_Data(ptr))->nTracks));
}


static XEN g_SMS_Data_nCoeff(XEN ptr)
{
  #define H_SMS_Data_nCoeff "(" FIELD_PREFIX "nCoeff SMS_Data) returns the number of filter coefficients"
  XEN_ASSERT_TYPE(XEN_SMS_Data_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "nCoeff", "SMS_Data pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_Data(ptr))->nCoeff));
}


static XEN g_SMS_Data_sizeData(XEN ptr)
{
  #define H_SMS_Data_sizeData "(" FIELD_PREFIX "sizeData SMS_Data) returns the size of the data arrays"
  XEN_ASSERT_TYPE(XEN_SMS_Data_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "sizeData", "SMS_Data pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_Data(ptr))->sizeData));
}

#if 0
typedef struct 
{
	float *pSmsData;        /*!< pointer to all SMS data */
	float *pFSinFreq;       /*!< frequency of sinusoids */
	float *pFSinAmp;       /*!< magnitude of sinusoids */
	float *pFSinPha;        /*!< phase of sinusoids */
	float *pFStocGain;     /*!< gain of stochastic component */
	float *pFStocCoeff;    /*!< filter coefficients for stochastic component */
} SMS_Data;
#endif

#define S_sms_freeFrame "sms_freeFrame"

static XEN g_sms_freeFrame(XEN ptr)
{
  #define H_sms_freeFrame "(" S_sms_freeFrame " ptr) clears some of the SMS_Data stucture's fields and frees stuff"
  XEN_ASSERT_TYPE(XEN_SMS_Data_P(ptr), ptr, XEN_ONLY_ARG, S_sms_freeFrame, "SMS_Data pointer");
  sms_freeFrame(XEN_TO_C_SMS_Data(ptr));
  return(XEN_FALSE);
}


#define S_sms_clearFrame "sms_clearFrame"

static XEN g_sms_clearFrame(XEN ptr)
{
  #define H_sms_clearFrame "(" S_sms_clearFrame " ptr) clears the SMS_Data stucture"
  XEN_ASSERT_TYPE(XEN_SMS_Data_P(ptr), ptr, XEN_ONLY_ARG, S_sms_clearFrame, "SMS_Data pointer");
  sms_clearFrame(XEN_TO_C_SMS_Data(ptr));
  return(XEN_FALSE);
}


#define S_sms_copyFrame "sms_copyFrame"

static XEN g_sms_copyFrame(XEN copy, XEN orig)
{
  #define H_sms_copyFrame "(" S_sms_copyFrame " copy orig) copies a frame of SMS_Data from 'orig' into 'copy'"
  XEN_ASSERT_TYPE(XEN_SMS_Data_P(copy), copy, XEN_ARG_1, S_sms_copyFrame, "an SMS_Data pointer");
  XEN_ASSERT_TYPE(XEN_SMS_Data_P(orig), orig, XEN_ARG_2, S_sms_copyFrame, "an SMS_Data pointer");
  sms_copyFrame(XEN_TO_C_SMS_Data(copy), XEN_TO_C_SMS_Data(orig));
  return(copy);
}


#if 0
void sms_sineSynthFrame (SMS_Data *pSmsFrame, float *pFBuffer,  int sizeBuffer, SMS_Data *pLastFrame, int iSamplingRate);
int sms_allocFrame (SMS_Data *pSmsFrame, int nTracks, int nCoeff,   int iPhase, int stochType);
void sms_interpolateFrames (SMS_Data *pSmsFrame1, SMS_Data *pSmsFrame2, SMS_Data *pSmsFrameOut, float fInterpFactor);
#endif



  /* need types for SMS_AnalParams SMS_Header SMS_SynthParams SMS_AnalFrame SMS_Peak SMS_SndHeader
     also make/free, I think -- how are these created?
     need conversions for float* and int* [float/int also]
     need holder for FILE*
     need scheme versions of smsAnal smsPrint smsSynth smsResample smsClean
  */


#if 0
int sms_analyze (float *pWaveform, long sizeNewData, SMS_Data *pSmsFrame,  SMS_AnalParams *pAnalParams, int *pINextSizeRead);
int sms_initAnalysis (  SMS_AnalParams *pAnalParams);
void sms_initAnalParams (SMS_AnalParams *pAnalParams);
int sms_initSynth( SMS_Header *pSmsHeader, SMS_SynthParams *pSynthParams );
int sms_changeSynthHop( SMS_SynthParams *pSynthParams, int sizeHop);
void sms_freeAnalysis (SMS_AnalParams *pAnalParams);
void sms_freeSynth( SMS_SynthParams *pSynthParams );
void sms_fillSndBuffer (float *pWaveform, long sizeNewData, SMS_AnalParams *pAnalParams);
int sms_spectrum (float *pFWaveform, int sizeWindow, float *pFMagSpectrum, float *pFPhaseSpectrum, SMS_AnalParams *pAnalParams);
int sms_sizeNextWindow (int iCurrentFrame, SMS_AnalParams *pAnalParams);
float sms_fundDeviation (SMS_AnalParams *pAnalParams, int iCurrentFrame);
int sms_detectPeaks (float *pFMagSpectrum, float *pAPhaSpectrum, int sizeMag, SMS_Peak *pSpectralPeaks, SMS_AnalParams *pAnalParams);
void sms_harmDetection (SMS_AnalFrame *pFrame, float fRefFundamental, SMS_AnalParams *pAnalParams);
int sms_peakContinuation (int iFrame, SMS_AnalParams *pAnalParams);
void sms_cleanTracks (int iCurrentFrame, SMS_AnalParams *pAnalParams);
void sms_scaleDet (float *pFSynthBuffer, float *pFOriginalBuffer, float *pFSinAmp, SMS_AnalParams *pAnalParams, int nTracks);
int sms_synthesize (SMS_Data *pSmsFrame, float*pFSynthesis,  SMS_SynthParams *pSynthParams);
void sms_initHeader (SMS_Header *pSmsHeader);
int sms_getHeader (char *pChFileName, SMS_Header **ppSmsHeader,	FILE **ppInputFile);
void sms_fillHeader (SMS_Header *pSmsHeader,  int nFrames, SMS_AnalParams *pAnalParams, int nTracks, int iOriginalSRate);
int sms_writeHeader (char *pChFileName, SMS_Header *pSmsHeader,  FILE **ppOutSmsFile);
int sms_writeFile (FILE *pSmsFile, SMS_Header *pSmsHeader);
int sms_initFrame (int iCurrentFrame, SMS_AnalParams *pAnalParams,  int sizeWindow);
int sms_allocFrameH (SMS_Header *pSmsHeader, SMS_Data *pSmsFrame);
int sms_getFrame (FILE *pInputFile, SMS_Header *pSmsHeader, int iFrame, SMS_Data *pSmsFrame);
int sms_writeFrame (FILE *pSmsFile, SMS_Header *pSmsHeader, SMS_Data *pSmsFrame);
int sms_frameSizeB (SMS_Header *pSmsHeader);
int sms_residual (float *pFSynthesis, float *pFOriginal,  float *pFResidual, int sizeWindow, SMS_AnalParams *pAnalParams);
int sms_stocAnalysis (float *pFResidual, int sizeWindow, SMS_Data *pSmsFrame, SMS_AnalParams *pAnalParams);
int sms_openSF (char *pChInputSoundFile, SMS_SndHeader *pSoundHeader);
int sms_getSound (SMS_SndHeader *pSoundHeader, float *pSoundData, long sizeSound, long offset);
#endif


#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_sms_init_w, g_sms_init)
XEN_NARGIFY_0(g_sms_free_w, g_sms_free)
XEN_NARGIFY_0(g_sms_clearSine_w, g_sms_clearSine)
XEN_NARGIFY_0(g_sms_clearSinc_w, g_sms_clearSinc)
XEN_NARGIFY_0(g_sms_random_w, g_sms_random)
XEN_NARGIFY_1(g_sms_power2_w, g_sms_power2)
XEN_NARGIFY_1(g_sms_sine_w, g_sms_sine)
XEN_NARGIFY_1(g_sms_sinc_w, g_sms_sinc)
XEN_NARGIFY_1(g_sms_prepSine_w, g_sms_prepSine)
XEN_NARGIFY_1(g_sms_prepSinc_w, g_sms_prepSinc)
XEN_NARGIFY_1(g_sms_errorString_w, g_sms_errorString)
XEN_NARGIFY_0(g_sms_writeSF_w, g_sms_writeSF)
XEN_NARGIFY_1(g_sms_preEmphasis_w, g_sms_preEmphasis)
XEN_NARGIFY_1(g_sms_deEmphasis_w, g_sms_deEmphasis)

XEN_NARGIFY_3(g_sms_createSF_w, g_sms_createSF)
XEN_NARGIFY_2(g_sms_writeSound_w, g_sms_writeSound)
XEN_NARGIFY_2(g_sms_copyFrame_w, g_sms_copyFrame)

XEN_NARGIFY_1(g_sms_make_floats_w, g_sms_make_floats)
XEN_NARGIFY_1(g_sms_free_floats_w, g_sms_free_floats)

XEN_NARGIFY_0(g_sms_make_SMS_Data_w, g_sms_make_SMS_Data)
XEN_NARGIFY_1(g_sms_free_SMS_Data_w, g_sms_free_SMS_Data)
XEN_NARGIFY_1(g_SMS_Data_nTracks_w, g_SMS_Data_nTracks)
XEN_NARGIFY_1(g_SMS_Data_nCoeff_w, g_SMS_Data_nCoeff)
XEN_NARGIFY_1(g_SMS_Data_sizeData_w, g_SMS_Data_sizeData)



#else
#define g_sms_init_w g_sms_init
#define g_sms_free_w g_sms_free
#define g_sms_clearSine_w g_sms_clearSine
#define g_sms_clearSinc_w g_sms_clearSinc
#define g_sms_random_w g_sms_random
#define g_sms_power2_w g_sms_power2
#define g_sms_sine_w g_sms_sine
#define g_sms_sinc_w g_sms_sinc
#define g_sms_prepSine_w g_sms_prepSine
#define g_sms_prepSinc_w g_sms_prepSinc
#define g_sms_errorString_w g_sms_errorString
#define g_sms_writeSF_w g_sms_writeSF
#define g_sms_preEmphasis_w g_sms_preEmphasis
#define g_sms_deEmphasis_w g_sms_deEmphasis

#define g_sms_createSF_w g_sms_createSF
#define g_sms_writeSound_w g_sms_writeSound
#define g_sms_copyFrame_w g_sms_copyFrame

#define g_sms_make_floats_w g_sms_make_floats
#define g_sms_free_floats_w g_sms_free_floats

#define g_sms_make_SMS_Data_w g_sms_make_SMS_Data
#define g_sms_free_SMS_Data_w g_sms_free_SMS_Data
#define g_SMS_Data_nTracks_w g_SMS_Data_nTracks
#define g_SMS_Data_sizeData_w g_SMS_Data_sizeData

#endif


#if HAVE_S7
  #define DEFINE_INTEGER(Name) s7_define_constant(s7, #Name, C_TO_XEN_INT(Name))
#else
  #define DEFINE_INTEGER(Name) XEN_DEFINE(#Name, C_TO_XEN_INT(Name))
#endif

#define DEFINE_READER(Name, Value, Doc) XEN_DEFINE_PROCEDURE(FIELD_PREFIX #Name, Value, 1, 0, 0, Doc)



void g_init_sms(void)
{
  /* not sure how many of these are actually needed */

  DEFINE_INTEGER(SMS_FORMAT_H);
  DEFINE_INTEGER(SMS_FORMAT_IH);
  DEFINE_INTEGER(SMS_FORMAT_HP);
  DEFINE_INTEGER(SMS_FORMAT_IHP);
  DEFINE_INTEGER(SMS_STYPE_ALL);
  DEFINE_INTEGER(SMS_STYPE_DET);
  DEFINE_INTEGER(SMS_STYPE_STOC);
  DEFINE_INTEGER(SMS_DET_IFFT);
  DEFINE_INTEGER(SMS_DET_SIN);
  DEFINE_INTEGER(SMS_STOC_NONE);
  DEFINE_INTEGER(SMS_STOC_APPROX);
  DEFINE_INTEGER(SMS_STOC_IFFT);
  DEFINE_INTEGER(SMS_OK);
  DEFINE_INTEGER(SMS_NOPEN);
  DEFINE_INTEGER(SMS_NSMS);
  DEFINE_INTEGER(SMS_MALLOC);
  DEFINE_INTEGER(SMS_RDERR);
  DEFINE_INTEGER(SMS_WRERR);
  DEFINE_INTEGER(SMS_FFTWERR);
  DEFINE_INTEGER(SMS_SNDERR);
  DEFINE_INTEGER(SMS_DBG_NONE);
  DEFINE_INTEGER(SMS_DBG_DET);
  DEFINE_INTEGER(SMS_DBG_PEAK_DET);
  DEFINE_INTEGER(SMS_DBG_HARM_DET);
  DEFINE_INTEGER(SMS_DBG_PEAK_CONT);
  DEFINE_INTEGER(SMS_DBG_CLEAN_TRAJ);
  DEFINE_INTEGER(SMS_DBG_SINE_SYNTH);
  DEFINE_INTEGER(SMS_DBG_STOC_ANAL);
  DEFINE_INTEGER(SMS_DBG_STOC_SYNTH);
  DEFINE_INTEGER(SMS_DBG_SMS_ANAL);
  DEFINE_INTEGER(SMS_DBG_ALL);
  DEFINE_INTEGER(SMS_DBG_RESIDUAL);
  DEFINE_INTEGER(SMS_DBG_SYNC);
  DEFINE_INTEGER(SMS_SOUND_TYPE_MELODY);
  DEFINE_INTEGER(SMS_SOUND_TYPE_NOTE);
  DEFINE_INTEGER(SMS_DIR_FWD);
  DEFINE_INTEGER(SMS_DIR_REV);
  DEFINE_INTEGER(SMS_WIN_HAMMING);
  DEFINE_INTEGER(SMS_WIN_BH_62);
  DEFINE_INTEGER(SMS_WIN_BH_70);
  DEFINE_INTEGER(SMS_WIN_BH_74);
  DEFINE_INTEGER(SMS_WIN_BH_92);
  DEFINE_INTEGER(SMS_WIN_HANNING);
  DEFINE_INTEGER(SMS_WIN_IFFT);
  DEFINE_INTEGER(SMS_MIN_GOOD_FRAMES);
  DEFINE_INTEGER(SMS_MAX_DEVIATION);
  DEFINE_INTEGER(SMS_ANAL_DELAY);
  DEFINE_INTEGER(SMS_DELAY_FRAMES);
  DEFINE_INTEGER(SMS_FRAME_EMPTY);
  DEFINE_INTEGER(SMS_FRAME_READY);
  DEFINE_INTEGER(SMS_FRAME_PEAKS_FOUND);
  DEFINE_INTEGER(SMS_FRAME_FUND_FOUND);
  DEFINE_INTEGER(SMS_FRAME_TRAJ_FOUND);
  DEFINE_INTEGER(SMS_FRAME_CLEANED);
  DEFINE_INTEGER(SMS_FRAME_RECOMPUTED);
  DEFINE_INTEGER(SMS_FRAME_DETER_SYNTH);
  DEFINE_INTEGER(SMS_FRAME_STOC_COMPUTED);
  DEFINE_INTEGER(SMS_FRAME_DONE);
  DEFINE_INTEGER(SMS_FRAME_END);
  DEFINE_INTEGER(SMS_MIN_SIZE_FRAME);

  XEN_DEFINE_PROCEDURE(S_sms_init,         g_sms_init_w,         0, 0, 0, H_sms_init);
  XEN_DEFINE_PROCEDURE(S_sms_free,         g_sms_free_w,         0, 0, 0, H_sms_free);
  XEN_DEFINE_PROCEDURE(S_sms_clearSine,    g_sms_clearSine_w,    0, 0, 0, H_sms_clearSine);
  XEN_DEFINE_PROCEDURE(S_sms_clearSinc,    g_sms_clearSinc_w,    0, 0, 0, H_sms_clearSinc);
  XEN_DEFINE_PROCEDURE(S_sms_random,       g_sms_random_w,       0, 0, 0, H_sms_random);
  XEN_DEFINE_PROCEDURE(S_sms_power2,       g_sms_power2_w,       1, 0, 0, H_sms_power2);
  XEN_DEFINE_PROCEDURE(S_sms_sine,         g_sms_sine_w,         1, 0, 0, H_sms_sine);
  XEN_DEFINE_PROCEDURE(S_sms_sinc,         g_sms_sinc_w,         1, 0, 0, H_sms_sinc);
  XEN_DEFINE_PROCEDURE(S_sms_prepSine,     g_sms_prepSine_w,     1, 0, 0, H_sms_prepSine);
  XEN_DEFINE_PROCEDURE(S_sms_prepSinc,     g_sms_prepSinc_w,     1, 0, 0, H_sms_prepSinc);
  XEN_DEFINE_PROCEDURE(S_sms_errorString,  g_sms_errorString_w,  1, 0, 0, H_sms_errorString);
  XEN_DEFINE_PROCEDURE(S_sms_writeSF,      g_sms_writeSF_w,      0, 0, 0, H_sms_writeSF);
  XEN_DEFINE_PROCEDURE(S_sms_preEmphasis,  g_sms_preEmphasis_w,  1, 0, 0, H_sms_preEmphasis);
  XEN_DEFINE_PROCEDURE(S_sms_deEmphasis,   g_sms_deEmphasis_w,   1, 0, 0, H_sms_deEmphasis);

  XEN_DEFINE_PROCEDURE(S_sms_createSF,     g_sms_createSF_w,     3, 0, 0, H_sms_createSF);
  XEN_DEFINE_PROCEDURE(S_sms_writeSound,   g_sms_writeSound_w,   2, 0, 0, H_sms_writeSound);
  XEN_DEFINE_PROCEDURE(S_sms_copyFrame,    g_sms_copyFrame_w,    2, 0, 0, H_sms_copyFrame);

  XEN_DEFINE_PROCEDURE(S_sms_free_floats,  g_sms_free_floats_w,  1, 0, 0, H_sms_free_floats);
  XEN_DEFINE_PROCEDURE(S_sms_make_floats,  g_sms_make_floats_w,  1, 0, 0, H_sms_make_floats);

  XEN_DEFINE_PROCEDURE(S_sms_free_SMS_Data, g_sms_free_SMS_Data_w, 1, 0, 0, H_sms_free_SMS_Data);
  XEN_DEFINE_PROCEDURE(S_sms_make_SMS_Data, g_sms_make_SMS_Data_w, 0, 0, 0, H_sms_make_SMS_Data);

  DEFINE_READER(nTracks,           g_SMS_Data_nTracks_w,           H_SMS_Data_nTracks);
  DEFINE_READER(nCoeff,           g_SMS_Data_nCoeff_w,           H_SMS_Data_nCoeff);
  DEFINE_READER(sizeData,           g_SMS_Data_sizeData_w,           H_SMS_Data_sizeData);
}

#endif
