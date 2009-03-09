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
   it would be better I think to use const char* wherever possible
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


#define S_sms_floats_ref "sms_floats_ref"

static XEN g_sms_floats_ref(XEN ptr, XEN ind)
{
  float *buf;
  #define H_sms_floats_ref "(" S_sms_floats_ref " buf index) returns buf[index]"
  XEN_ASSERT_TYPE(XEN_floats_P(ptr), ptr, XEN_ARG_1, S_sms_floats_ref, "pointer to an array of C floats");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(ind), ind, XEN_ARG_2, S_sms_floats_ref, "an integer");
  buf = (float *)XEN_TO_C_floats(ptr);
  return(C_TO_XEN_DOUBLE(buf[XEN_TO_C_INT(ind)]));
}


#define S_sms_floats_set "sms_floats_set!"

static XEN g_sms_floats_set(XEN ptr, XEN ind, XEN val)
{
  float *buf;
  #define H_sms_floats_set "(" S_sms_floats_set " buf index val) sets buf[index] to val"
  XEN_ASSERT_TYPE(XEN_floats_P(ptr), ptr, XEN_ARG_1, S_sms_floats_set, "pointer to an array of C floats");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(ind), ind, XEN_ARG_2, S_sms_floats_set, "an integer");
  XEN_ASSERT_TYPE(XEN_DOUBLE_P(val), val, XEN_ARG_3, S_sms_floats_set, "a float");
  buf = (float *)XEN_TO_C_floats(ptr);
  buf[XEN_TO_C_INT(ind)] = (float)XEN_TO_C_DOUBLE(val);
  return(val);
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


/* these are not used externally? */
/*
int sms_spectralApprox (float *pFSpec1, int sizeSpec1, int sizeSpec1Used, float *pFSpec2, int sizeSpec2, int nCoefficients);
int sms_quickSpectrum (float *pFWaveform, float *pFWindow, int sizeWindow, float *pFMagSpectrum, float *pFPhaseSpectrum, int sizeFft);
int sms_invQuickSpectrum (float *pFMagSpectrum, float *pFPhaseSpectrum, int sizeFft, float *pFWaveform, int sizeWave);
int sms_invQuickSpectrumW (float *pFMagSpectrum, float *pFPhaseSpectrum, int sizeFft, float *pFWaveform, int sizeWave,  float *pFWindow);
void sms_rdft(int sizeFft, float *pRealArray, int direction );
void sms_getWindow (int sizeWindow, float *pFWindow, int iWindowType);
*/



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


static XEN g_SMS_Data_pFSinFreq(XEN ptr)
{
  #define H_SMS_Data_pFSinFreq "(" FIELD_PREFIX "pFSinFreq SMS_Data) returns the SMS_Data pFSinFreq field (an array)"
  XEN_ASSERT_TYPE(XEN_SMS_Data_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "pFSinFreq", "SMS_Data pointer");
  return(C_TO_XEN_floats((XEN_TO_C_SMS_Data(ptr))->pFSinFreq));
}


static XEN g_SMS_Data_pFSinAmp(XEN ptr)
{
  #define H_SMS_Data_pFSinAmp "(" FIELD_PREFIX "pFSinAmp SMS_Data) returns the SMS_Data pFSinAmp field (an array)"
  XEN_ASSERT_TYPE(XEN_SMS_Data_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "pFSinFreq", "SMS_Data pointer");
  return(C_TO_XEN_floats((XEN_TO_C_SMS_Data(ptr))->pFSinAmp));
}


static XEN g_SMS_Data_pFSinPha(XEN ptr)
{
  #define H_SMS_Data_pFSinPha "(" FIELD_PREFIX "pFSinPha SMS_Data) returns the SMS_Data pFSinPha field (an array)"
  XEN_ASSERT_TYPE(XEN_SMS_Data_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "pFSinPha", "SMS_Data pointer");
  return(C_TO_XEN_floats((XEN_TO_C_SMS_Data(ptr))->pFSinPha));
}


static XEN g_SMS_Data_pFStocGain(XEN ptr)
{
  #define H_SMS_Data_pFStocGain "(" FIELD_PREFIX "pFStocGain SMS_Data) returns the SMS_Data pFStocGain field (an array)"
  XEN_ASSERT_TYPE(XEN_SMS_Data_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "pFStocGain", "SMS_Data pointer");
  return(C_TO_XEN_floats((XEN_TO_C_SMS_Data(ptr))->pFStocGain));
}


static XEN g_SMS_Data_pFStocCoeff(XEN ptr)
{
  #define H_SMS_Data_pFStocCoeff "(" FIELD_PREFIX "pFStocCoeff SMS_Data) returns the SMS_Data pFStocCoeff field (an array)"
  XEN_ASSERT_TYPE(XEN_SMS_Data_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "pFStocCoeff", "SMS_Data pointer");
  return(C_TO_XEN_floats((XEN_TO_C_SMS_Data(ptr))->pFStocCoeff));
}


#if 0
/* not used? */
	float *pSmsData; 

/* todo: */
writers:
        nCoeffs, nTracks
	float *pFSinFreq
	float *pFSinAmp
	float *pFStocGain
	float *pFStocCoeff
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


/* not used? */
/* void sms_sineSynthFrame (SMS_Data *pSmsFrame, float *pFBuffer,  int sizeBuffer, SMS_Data *pLastFrame, int iSamplingRate); */


#define S_sms_allocFrame "sms_allocFrame"

static XEN g_sms_allocFrame(XEN frame, XEN tracks, XEN coeff, XEN phase, XEN type)
{
  #define H_sms_allocFrame "(" S_sms_allocFrame " frame tracks coeff phase type) allocates memory \
for a frame of SMS data.  'frame' is an SMS_Data pointer, 'tracks' is the number of sinusoidal tracks \
in the frame, 'coeff' is the number of stochastic coefficients in the frame, 'phase' determines whether \
phase information is in the frame (non-zero = true), and 'type' is the stochastic resynthesis type."

  XEN_ASSERT_TYPE(XEN_SMS_Data_P(frame), frame, XEN_ARG_1, S_sms_allocFrame, "an SMS_Data pointer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(tracks), tracks, XEN_ARG_2, S_sms_allocFrame, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(coeff), coeff, XEN_ARG_3, S_sms_allocFrame, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(phase), phase, XEN_ARG_4, S_sms_allocFrame, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(type), type, XEN_ARG_5, S_sms_allocFrame, "an integer");
  return(C_TO_XEN_INT(sms_allocFrame(XEN_TO_C_SMS_Data(frame),
				     XEN_TO_C_INT(tracks),
				     XEN_TO_C_INT(coeff),
				     XEN_TO_C_INT(phase),
				     XEN_TO_C_INT(type))));
}


#define S_sms_interpolateFrames "sms_interpolateFrames"

static XEN g_sms_interpolateFrames(XEN frame1, XEN frame2, XEN frame_out, XEN factor)
{
  #define H_sms_interpolateFrames "(" S_sms_interpolateFrames " frame1 frame2 frame-out factor) interpolates between \
two SMS_Data frames ('frame1' and 'frame2').  'frame-out' gets the output, 'factor' is the interpolation factor, \
that is frame1 + factor * (frame2 - frame1).  It returns 'frame-out'."

  XEN_ASSERT_TYPE(XEN_SMS_Data_P(frame1), frame1, XEN_ARG_1, S_sms_interpolateFrames, "an SMS_Data pointer");  
  XEN_ASSERT_TYPE(XEN_SMS_Data_P(frame2), frame2, XEN_ARG_2, S_sms_interpolateFrames, "an SMS_Data pointer");  
  XEN_ASSERT_TYPE(XEN_SMS_Data_P(frame_out), frame_out, XEN_ARG_3, S_sms_interpolateFrames, "an SMS_Data pointer");  
  XEN_ASSERT_TYPE(XEN_DOUBLE_P(factor), factor, XEN_ARG_4, S_sms_interpolateFrames, "a float");

  sms_interpolateFrames(XEN_TO_C_SMS_Data(frame1),
			XEN_TO_C_SMS_Data(frame2),
			XEN_TO_C_SMS_Data(frame_out),
			XEN_TO_C_DOUBLE(factor));
  return(frame_out);
}




/* -------------------------------- SMS_AnalParams -------------------------------- */

#define C_TO_XEN_SMS_AnalParams(Value) ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("SMS_AnalParams"), XEN_WRAP_C_POINTER(Value)) : XEN_FALSE)
#define XEN_TO_C_SMS_AnalParams(Value) ((SMS_AnalParams *)(XEN_UNWRAP_C_POINTER(XEN_CADR(Value))))
#define XEN_SMS_AnalParams_P(Value)    (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                                        (strcmp("SMS_AnalParams", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))

#define S_sms_make_SMS_AnalParams "sms_make_SMS_AnalParams"

static XEN g_sms_make_SMS_AnalParams(void)
{
  #define H_sms_make_SMS_AnalParams "(" S_sms_make_SMS_AnalParams ") returns a new (malloc'd) SMS_AnalParams pointer"
  return(C_TO_XEN_SMS_AnalParams((SMS_AnalParams *)malloc(sizeof(SMS_AnalParams))));
}
  

#define S_sms_free_SMS_AnalParams "sms_free_SMS_AnalParams"

static XEN g_sms_free_SMS_AnalParams(XEN ptr)
{
  #define H_sms_free_SMS_AnalParams "(" S_sms_free_SMS_AnalParams " ptr) frees the pointer"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, S_sms_free_SMS_AnalParams, "SMS_AnalParams pointer");
  free(XEN_TO_C_SMS_AnalParams(ptr));
  return(XEN_FALSE);
}


#define S_sms_initAnalysis "sms_initAnalysis"

static XEN g_sms_initAnalysis(XEN ptr)
{
  #define H_sms_initAnalysis "(" S_sms_initAnalysis " ptr) initializes the SMS_AnalParams structure"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, S_sms_initAnalysis, "SMS_AnalParams pointer");
  return(C_TO_XEN_INT(sms_initAnalysis(XEN_TO_C_SMS_AnalParams(ptr))));
}


#define S_sms_freeAnalysis "sms_freeAnalysis"

static XEN g_sms_freeAnalysis(XEN ptr)
{
  #define H_sms_freeAnalysis "(" S_sms_freeAnalysis " ptr) frees the SMS_AnalParams structure"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, S_sms_freeAnalysis, "SMS_AnalParams pointer");
  sms_freeAnalysis(XEN_TO_C_SMS_AnalParams(ptr));
  return(XEN_FALSE);
}




#if 0
typedef struct 
{
	int iDebugMode; /*!< debug codes enumerated by SMS_DBG \see SMS_DBG */
	int iFormat;          /*!< analysis format code defined by SMS_Format \see SMS_Format */
	int iSoundType;            /*!< type of sound to be analyzed \see SMS_SOUND_TYPE */	
	int iStochasticType;      /*!<  type of stochastic model defined by SMS_StocSynthType \see SMS_StocSynthType */
	int iFrameRate;        /*!< rate in Hz of data frames */
	int nStochasticCoeff;  /*!< number of stochastic coefficients per frame  */
	float fLowestFundamental; /*!< lowest fundamental frequency in Hz */
	float fHighestFundamental;/*!< highest fundamental frequency in Hz */
	float fDefaultFundamental;/*!< default fundamental in Hz */
	float fPeakContToGuide;   /*!< contribution of previous peak to current guide (between 0 and 1) */
	float fFundContToGuide;   /*!< contribution of current fundamental to current guide (between 0 and 1) */
	float fFreqDeviation;     /*!< maximum deviation from peak to peak */				     
	int iSamplingRate;        /*! sampling rate of sound to be analyzed */
	int iDefaultSizeWindow;   /*!< default size of analysis window in samples */
	int sizeHop;              /*!< hop size of analysis window in samples */
	float fSizeWindow;       /*!< size of analysis window in number of periods */
	int nGuides;              /*!< number of guides used for peak detection and continuation \see SMS_Guide */
	int iCleanTracks;           /*!< whether or not to clean sinusoidal tracks */
	float fMinRefHarmMag;     /*!< minimum magnitude in dB for reference peak */
	float fRefHarmMagDiffFromMax; /*!< maximum magnitude difference from reference peak to highest peak */
	int iRefHarmonic;	       /*!< reference harmonic to use in the fundamental detection */
	int iMinTrackLength;	       /*!< minimum length in samples of a given track */
	int iMaxSleepingTime;	   /*!< maximum sleeping time for a track */
	float fHighestFreq;        /*!< highest frequency to be searched */
	float fMinPeakMag;         /*!< minimum magnitude in dB for a good peak */	
	int iAnalysisDirection;    /*!< analysis direction, direct or reverse */	
	int iSizeSound;             /*!< total size of sound to be analyzed in samples */	 	
	int iWindowType;            /*!< type of FFT analysis window \see SMS_WINDOWS */			  	 			 
        int iMaxDelayFrames;     /*!< maximum number of frames to delay before peak continuation */
        SMS_Data prevFrame;   /*!< the previous analysis frame  */
        SMS_SndBuffer soundBuffer;    /*!< signal to be analyzed */
        SMS_SndBuffer synthBuffer; /*!< resynthesized signal used to create the residual */
        SMS_AnalFrame *pFrames;  /*!< an array of frames that have already been analyzed */
        SMS_AnalFrame **ppFrames; /*!< pointers to the frames analyzed (it is circular-shifted once the array is full */
        float fResidualPercentage; /*!< accumalitive residual percentage */
        float *pFSpectrumWindow; /*!< the window used during spectrum analysis */
#ifdef FFTW
        SMS_Fourier fftw; /*!< structure of data used by the FFTW library (floating point) */
#endif
} SMS_AnalParams;
#endif




/* -------------------------------- SMS_SynthParams -------------------------------- */

#define C_TO_XEN_SMS_SynthParams(Value) ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("SMS_SynthParams"), XEN_WRAP_C_POINTER(Value)) : XEN_FALSE)
#define XEN_TO_C_SMS_SynthParams(Value) ((SMS_SynthParams *)(XEN_UNWRAP_C_POINTER(XEN_CADR(Value))))
#define XEN_SMS_SynthParams_P(Value)    (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                                        (strcmp("SMS_SynthParams", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))

#define S_sms_make_SMS_SynthParams "sms_make_SMS_SynthParams"

static XEN g_sms_make_SMS_SynthParams(void)
{
  #define H_sms_make_SMS_SynthParams "(" S_sms_make_SMS_SynthParams ") returns a new (malloc'd) SMS_SynthParams pointer"
  return(C_TO_XEN_SMS_SynthParams((SMS_SynthParams *)malloc(sizeof(SMS_SynthParams))));
}
  

#define S_sms_free_SMS_SynthParams "sms_free_SMS_SynthParams"

static XEN g_sms_free_SMS_SynthParams(XEN ptr)
{
  #define H_sms_free_SMS_SynthParams "(" S_sms_free_SMS_SynthParams " ptr) frees the pointer"
  XEN_ASSERT_TYPE(XEN_SMS_SynthParams_P(ptr), ptr, XEN_ONLY_ARG, S_sms_free_SMS_SynthParams, "SMS_SynthParams pointer");
  free(XEN_TO_C_SMS_SynthParams(ptr));
  return(XEN_FALSE);
}


#define S_sms_freeSynth "sms_freeSynth"

static XEN g_sms_freeSynth(XEN ptr)
{
  #define H_sms_freeSynth "(" S_sms_freeSynth " ptr) frees the SMS_SynthParams structure"
  XEN_ASSERT_TYPE(XEN_SMS_SynthParams_P(ptr), ptr, XEN_ONLY_ARG, S_sms_freeSynth, "SMS_SynthParams pointer");
  sms_freeSynth(XEN_TO_C_SMS_SynthParams(ptr));
  return(XEN_FALSE);
}

#if 0
/*! \struct SMS_SynthParams
 * \brief structure with information for synthesis functions
 *
 * This structure contains all the necessary settings for different types of synthesis.
 * It also holds arrays for windows and the inverse-FFT, as well as the previously
 * synthesized frame.
 *
 */
typedef struct
{
	int iStochasticType;       /*!<  type of stochastic model defined by SMS_StocSynthType 
                                                     \see SMS_StocSynthType */
	int iSynthesisType;        /*!< type of synthesis to perform \see SMS_SynthType */
        int iDetSynthType;         /*!< method for synthesizing deterministic component
                                                 \see SMS_DetSynthType */
	int iOriginalSRate;  /*!< samplerate of the sound model source.  I used to determine the stochastic
                               synthesis approximation */
	int iSamplingRate;         /*!< synthesis samplerate */
	int sizeHop;                   /*!< number of samples to synthesis for each frame */
        int origSizeHop;            /*!< original number of samples used to create each analysis frame */
        float fStocGain;            /*!< gain multiplied to the stachostic component */
        float fTranspose;          /*!< frequency transposing value multiplied by each frequency */
	float *pFDetWindow;    /*!< array to hold the window used for deterministic synthesis  \see SMS_WIN_IFFT */
        float *pFStocWindow; /*!< array to hold the window used for stochastic synthesis (Hanning) */
	SMS_Data prevFrame; /*!< previous data frame, used for smooth interpolation between frames */
#ifdef FFTW
        SMS_Fourier fftw; /*!< structure of data used by the FFTW library (floating point) */
#else
        float *pFFTBuff;  /*!< an array for an inplace inverse FFT transform */
#endif
} SMS_SynthParams;
#endif


/* -------------------------------- SMS_Header -------------------------------- */

#define C_TO_XEN_SMS_Header(Value) ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("SMS_Header"), XEN_WRAP_C_POINTER(Value)) : XEN_FALSE)
#define XEN_TO_C_SMS_Header(Value) ((SMS_Header *)(XEN_UNWRAP_C_POINTER(XEN_CADR(Value))))
#define XEN_SMS_Header_P(Value)    (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                                        (strcmp("SMS_Header", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))

#define S_sms_make_SMS_Header "sms_make_SMS_Header"

static XEN g_sms_make_SMS_Header(void)
{
  #define H_sms_make_SMS_Header "(" S_sms_make_SMS_Header ") returns a new (malloc'd) SMS_Header pointer"
  return(C_TO_XEN_SMS_Header((SMS_Header *)malloc(sizeof(SMS_Header))));
}
  

#define S_sms_free_SMS_Header "sms_free_SMS_Header"

static XEN g_sms_free_SMS_Header(XEN ptr)
{
  #define H_sms_free_SMS_Header "(" S_sms_free_SMS_Header " ptr) frees the pointer"
  XEN_ASSERT_TYPE(XEN_SMS_Header_P(ptr), ptr, XEN_ONLY_ARG, S_sms_free_SMS_Header, "SMS_Header pointer");
  free(XEN_TO_C_SMS_Header(ptr));
  return(XEN_FALSE);
}


#define S_sms_initHeader "sms_initHeader"

static XEN g_sms_initHeader(XEN ptr)
{
  #define H_sms_initHeader "(" S_sms_initHeader " ptr) initializes the SMS_Header structure"
  XEN_ASSERT_TYPE(XEN_SMS_Header_P(ptr), ptr, XEN_ONLY_ARG, S_sms_initHeader, "SMS_Header pointer");
  sms_initHeader(XEN_TO_C_SMS_Header(ptr));
  return(XEN_FALSE);
}


#define S_sms_frameSizeB "sms_frameSizeB"

static XEN g_sms_frameSizeB(XEN ptr)
{
  #define H_sms_frameSizeB "(" S_sms_frameSizeB " ptr) returns the size in bytes of a frame in an SMS file"
  XEN_ASSERT_TYPE(XEN_SMS_Header_P(ptr), ptr, XEN_ONLY_ARG, S_sms_frameSizeB, "SMS_Header pointer");
  return(C_TO_XEN_INT(sms_frameSizeB(XEN_TO_C_SMS_Header(ptr))));
}

#if 0
/*! \struct SMS_Header 
 *  \brief structure for the header of an SMS file 
 *  
 *  This header contains all the information necessary to read an SMS
 *  file, prepare memory and synthesizer parameters.
 *  
 *  The header also contains variable components for additional information
 *  that may be stored along with the analysis, such as descriptors or text.
 *  
 *  The first four members of the Header are necessary in this order to correctly
 *  open the .sms files created by this library.
 *
 *  iSampleRate contains the samplerate of the analysis signal because it is
 *  necessary to know this information to recreate the residual spectrum.
 *  
 *  In the first release, the descriptors are not used, but are here because they
 *  were implemented in previous versions of this code (in the 90's).  With time,
 *  the documentation will be updated to reflect which members of the header
 *  are useful in manipulations, and what functions to use for these manipulatinos
 */
typedef struct 
{
	int iSmsMagic;         /*!< identification constant */
	int iHeadBSize;        /*!< size in bytes of header */
	int nFrames;	         /*!< number of data frames */
	int iFrameBSize;      /*!< size in bytes of each data frame */
        int iSamplingRate;     /*!< samplerate of analysis signal (necessary to recreate residual spectrum */
	int iFormat;           /*!< type of data format \see SMS_Format */
	int nTracks;     /*!< number of sinusoidal tracks per frame */
	int iFrameRate;        /*!< rate in Hz of data frames */
	int iStochasticType;   /*!< type stochastic representation */
	int nStochasticCoeff;  /*!< number of stochastic coefficients per frame  */
	float fAmplitude;      /*!< average amplitude of represented sound.  */
	float fFrequency;      /*!< average fundamental frequency */
	int iBegSteadyState;   /*!< record number of begining of steady state. */
	int iEndSteadyState;   /*!< record number of end of steady state. */
	float fResidualPerc;   /*!< percentage of the residual to original */
	int nLoopRecords;      /*!< number of loop records specified. */
	int nSpecEnvelopePoints; /*!< number of breakpoints in spectral envelope */
	int nTextCharacters;   /*!< number of text characters */
	/* variable part */
	int *pILoopRecords;    /*!< array of record numbers of loop points */
	float *pFSpectralEnvelope; /*!< spectral envelope of partials */
	char *pChTextCharacters; /*!< Text string relating to the sound */
} SMS_Header;
#endif


/* -------------------------------- SMS_SndHeader -------------------------------- */

#define C_TO_XEN_SMS_SndHeader(Value) ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("SMS_SndHeader"), XEN_WRAP_C_POINTER(Value)) : XEN_FALSE)
#define XEN_TO_C_SMS_SndHeader(Value) ((SMS_SndHeader *)(XEN_UNWRAP_C_POINTER(XEN_CADR(Value))))
#define XEN_SMS_SndHeader_P(Value)    (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                                        (strcmp("SMS_SndHeader", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))

#define S_sms_make_SMS_SndHeader "sms_make_SMS_SndHeader"

static XEN g_sms_make_SMS_SndHeader(void)
{
  #define H_sms_make_SMS_SndHeader "(" S_sms_make_SMS_SndHeader ") returns a new (malloc'd) SMS_SndHeader pointer"
  return(C_TO_XEN_SMS_SndHeader((SMS_SndHeader *)malloc(sizeof(SMS_SndHeader))));
}
  

#define S_sms_free_SMS_SndHeader "sms_free_SMS_SndHeader"

static XEN g_sms_free_SMS_SndHeader(XEN ptr)
{
  #define H_sms_free_SMS_SndHeader "(" S_sms_free_SMS_SndHeader " ptr) frees the pointer"
  XEN_ASSERT_TYPE(XEN_SMS_SndHeader_P(ptr), ptr, XEN_ONLY_ARG, S_sms_free_SMS_SndHeader, "SMS_SndHeader pointer");
  free(XEN_TO_C_SMS_SndHeader(ptr));
  return(XEN_FALSE);
}


static XEN g_SMS_SndHeader_nSamples(XEN ptr)
{
  #define H_SMS_SndHeader_nSamples "(" FIELD_PREFIX "nSamples SMS_SndHeader) returns the number of samples in the sound"
  XEN_ASSERT_TYPE(XEN_SMS_SndHeader_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "nSamples", "SMS_SndHeader pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_SndHeader(ptr))->nSamples));
}


static XEN g_SMS_SndHeader_iSamplingRate(XEN ptr)
{
  #define H_SMS_SndHeader_iSamplingRate "(" FIELD_PREFIX "iSamplingRate SMS_SndHeader) returns the sampling rate of the sound"
  XEN_ASSERT_TYPE(XEN_SMS_SndHeader_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iSamplingRate", "SMS_SndHeader pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_SndHeader(ptr))->iSamplingRate));
}


/* channelCount and sizeHeader not used */



/* -------------------------------- FILE* -------------------------------- */

#define C_TO_XEN_FILE(Value) ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("FILE*"), XEN_WRAP_C_POINTER(Value)) : XEN_FALSE)
#define XEN_TO_C_FILE(Value) ((FILE *)(XEN_UNWRAP_C_POINTER(XEN_CADR(Value))))
#define XEN_FILE_P(Value)    (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                                  (strcmp("FILE*", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))




#define S_sms_initSynth "sms_initSynth"

static XEN g_sms_initSynth(XEN ptr1, XEN ptr2)
{
  #define H_sms_initSynth "(" S_sms_initSynth " header synth) initializes the SMS_SynthParams struct.  'header' is \
an SMS_Header pointer, and 'synth' is an SMS_SynthParams pointer"

  XEN_ASSERT_TYPE(XEN_SMS_Header_P(ptr1), ptr1, XEN_ARG_1, S_sms_initSynth, "SMS_Header pointer");
  XEN_ASSERT_TYPE(XEN_SMS_SynthParams_P(ptr2), ptr2, XEN_ARG_2, S_sms_initSynth, "SMS_SynthParams pointer");
  return(C_TO_XEN_INT(sms_initSynth(XEN_TO_C_SMS_Header(ptr1),
				    XEN_TO_C_SMS_SynthParams(ptr2))));
}


#define S_sms_allocFrameH "sms_allocFrameH"

static XEN g_sms_allocFrameH(XEN ptr1, XEN ptr2)
{
  #define H_sms_allocFrameH "(" S_sms_allocFrameH " header frame) allocates an SMS_Data frame from an SMS_Header struct"
  XEN_ASSERT_TYPE(XEN_SMS_Header_P(ptr1), ptr1, XEN_ARG_1, S_sms_allocFrameH, "SMS_Header pointer");
  XEN_ASSERT_TYPE(XEN_SMS_Data_P(ptr2), ptr2, XEN_ARG_2, S_sms_allocFrameH, "SMS_Data pointer");
  return(C_TO_XEN_INT(sms_allocFrameH(XEN_TO_C_SMS_Header(ptr1),
				      XEN_TO_C_SMS_Data(ptr2))));
}


#define S_sms_writeFile "sms_writeFile"

static XEN g_sms_writeFile(XEN ptr1, XEN ptr2)
{
  #define H_sms_writeFile "(" S_sms_writeFile " file header) rewrites an SMS header and closes the file"
  XEN_ASSERT_TYPE(XEN_FILE_P(ptr1), ptr1, XEN_ARG_1, S_sms_writeFile, "FILE* pointer");
  XEN_ASSERT_TYPE(XEN_SMS_Header_P(ptr2), ptr2, XEN_ARG_2, S_sms_writeFile, "SMS_Header pointer");
  return(C_TO_XEN_INT(sms_writeFile(XEN_TO_C_FILE(ptr1),
				    XEN_TO_C_SMS_Header(ptr2))));
}


#define S_sms_openSF "sms_openSF"

static XEN g_sms_openSF(XEN ptr1, XEN ptr2)
{
  #define H_sms_openSF "(" S_sms_openSF " file header) opens a sound file"
  XEN_ASSERT_TYPE(XEN_STRING_P(ptr1), ptr1, XEN_ARG_1, S_sms_openSF, "a string");
  XEN_ASSERT_TYPE(XEN_SMS_SndHeader_P(ptr2), ptr2, XEN_ARG_2, S_sms_openSF, "SMS_SndHeader pointer");
  return(C_TO_XEN_INT(sms_openSF((char *)XEN_TO_C_STRING(ptr1),
				 XEN_TO_C_SMS_SndHeader(ptr2))));
}


#define S_sms_getHeader "sms_getHeader"

static XEN g_sms_getHeader(XEN file)
{
  /* here the calling sequence is different because the trailing 2 args are by reference */
  int result;
  FILE *fp = NULL;
  SMS_Header *hdr = NULL;
  #define H_sms_getHeader "(" S_sms_getHeader " file) reads an SMS header in 'file' and returns '(error-indication SMS_Header FILE*)"
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ONLY_ARG, S_sms_getHeader, "a string");
  result = sms_getHeader((char *)XEN_TO_C_STRING(file), &hdr, &fp);
  return(XEN_LIST_3(C_TO_XEN_INT(result),
		    C_TO_XEN_SMS_Header(hdr),
		    C_TO_XEN_FILE(fp)));
}


#define S_sms_writeHeader "sms_writeHeader"

static XEN g_sms_writeHeader(XEN file, XEN header)
{
  int result;
  FILE *fp = NULL;
  #define H_sms_writeHeader "(" S_sms_writeHeader " file header) writes an SMS header in file.  It returns a \
list: '(error-indication FILE*)"

  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_sms_writeHeader, "a string");
  XEN_ASSERT_TYPE(XEN_SMS_Header_P(header), header, XEN_ARG_2, S_sms_writeHeader, "SMS_Header pointer");
  result = sms_writeHeader((char *)XEN_TO_C_STRING(file),
			   XEN_TO_C_SMS_Header(header),
			   &fp);
  return(XEN_LIST_2(C_TO_XEN_INT(result),
		    C_TO_XEN_FILE(fp)));
}


#define S_sms_getFrame "sms_getFrame"

static XEN g_sms_getFrame(XEN file, XEN header, XEN n, XEN frame)
{
  #define H_sms_getFrame "(" S_sms_getFrame " file header n frame) reads an SMS data frame. 'file' \
is a FILE pointer to an SMS file, 'header' is an SMS_Header pointer, 'n' is the frame number (an integer), \
and 'frame' is an SMS_Data pointer."

  XEN_ASSERT_TYPE(XEN_FILE_P(file), file, XEN_ARG_1, S_sms_getFrame, "a FILE pointer");
  XEN_ASSERT_TYPE(XEN_SMS_Header_P(header), header, XEN_ARG_2, S_sms_getFrame, "an SMS_Header pointer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(n), n, XEN_ARG_3, S_sms_getFrame, "an integer");
  XEN_ASSERT_TYPE(XEN_SMS_Data_P(frame), frame, XEN_ARG_4, S_sms_getFrame, "an SMS_Data pointer");

  return(C_TO_XEN_INT(sms_getFrame(XEN_TO_C_FILE(file),
				   XEN_TO_C_SMS_Header(header),
				   XEN_TO_C_INT(n),
				   XEN_TO_C_SMS_Data(frame))));
}


#define S_sms_writeFrame "sms_writeFrame"

static XEN g_sms_writeFrame(XEN file, XEN header, XEN frame)
{
  #define H_sms_writeFrame "(" S_sms_writeFrame " file header frame) writes a frame to an SMS file. 'file' is \
a FILE pointer to an SMS file, 'header' is an SMS_Header pointer, and 'frame' is an SMS_Data pointer."

  XEN_ASSERT_TYPE(XEN_FILE_P(file), file, XEN_ARG_1, S_sms_writeFrame, "a FILE pointer");
  XEN_ASSERT_TYPE(XEN_SMS_Header_P(header), header, XEN_ARG_2, S_sms_writeFrame, "an SMS_Header pointer");
  XEN_ASSERT_TYPE(XEN_SMS_Data_P(frame), frame, XEN_ARG_3, S_sms_writeFrame, "an SMS_Data pointer");

  return(C_TO_XEN_INT(sms_writeFrame(XEN_TO_C_FILE(file),
				   XEN_TO_C_SMS_Header(header),
				   XEN_TO_C_SMS_Data(frame))));
}


#define S_sms_getSound "sms_getSound"

static XEN g_sms_getSound(XEN header, XEN data, XEN size, XEN offset)
{
  #define H_sms_getSound "(" S_sms_getSound ") gets a chunk of sound from the input file"
  XEN_ASSERT_TYPE(XEN_SMS_SndHeader_P(header), header, XEN_ARG_1, S_sms_getSound, "an SMS_SndHeader pointer");
  XEN_ASSERT_TYPE(XEN_floats_P(data), data, XEN_ARG_2, S_sms_getSound, "a floats pointer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(size), size, XEN_ARG_3, S_sms_getSound, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(offset), offset, XEN_ARG_3, S_sms_getSound, "an integer");

  return(C_TO_XEN_INT(sms_getSound(XEN_TO_C_SMS_SndHeader(header),
				   XEN_TO_C_floats(data),
				   XEN_TO_C_INT(size),
				   XEN_TO_C_INT(offset))));
}


#define S_sms_analyze "sms_analyze"

static XEN g_sms_analyze(XEN data, XEN size, XEN out_data, XEN params)
{
  int result, next_size;
  #define H_sms_analyze "(" S_sms_analyze " data size output params data-size) is the main SMS analysis function. \
The 'data' is a float array of waveform data, 'size' is its size, 'output' is the output SMS_Data frame, 'params' is \
the analysis parameter struct.  It returns a list: '(error-indication next-read-size)."

  XEN_ASSERT_TYPE(XEN_floats_P(data), data, XEN_ARG_1, S_sms_analyze, "a floats pointer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(size), size, XEN_ARG_2, S_sms_analyze, "an integer");
  XEN_ASSERT_TYPE(XEN_SMS_Data_P(out_data), out_data, XEN_ARG_3, S_sms_analyze, "an SMS_Data pointer");
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(params), params, XEN_ARG_4, S_sms_analyze, "an SMS_AnalParams pointer");
  
  result = sms_analyze(XEN_TO_C_floats(data),
		       XEN_TO_C_INT(size),
		       XEN_TO_C_SMS_Data(out_data),
		       XEN_TO_C_SMS_AnalParams(params),
		       &next_size);

  return(XEN_LIST_2(C_TO_XEN_INT(result),
		    C_TO_XEN_INT(next_size)));
}


/* not used
void sms_initAnalParams (SMS_AnalParams *pAnalParams);
void sms_fillSndBuffer (float *pWaveform, long sizeNewData, SMS_AnalParams *pAnalParams);
int sms_spectrum (float *pFWaveform, int sizeWindow, float *pFMagSpectrum, float *pFPhaseSpectrum, SMS_AnalParams *pAnalParams);
int sms_sizeNextWindow (int iCurrentFrame, SMS_AnalParams *pAnalParams);
float sms_fundDeviation (SMS_AnalParams *pAnalParams, int iCurrentFrame);
int sms_peakContinuation (int iFrame, SMS_AnalParams *pAnalParams);
void sms_cleanTracks (int iCurrentFrame, SMS_AnalParams *pAnalParams);
void sms_scaleDet (float *pFSynthBuffer, float *pFOriginalBuffer, float *pFSinAmp, SMS_AnalParams *pAnalParams, int nTracks);
int sms_initFrame (int iCurrentFrame, SMS_AnalParams *pAnalParams,  int sizeWindow);
int sms_residual (float *pFSynthesis, float *pFOriginal,  float *pFResidual, int sizeWindow, SMS_AnalParams *pAnalParams);
int sms_stocAnalysis (float *pFResidual, int sizeWindow, SMS_Data *pSmsFrame, SMS_AnalParams *pAnalParams);
int sms_changeSynthHop( SMS_SynthParams *pSynthParams, int sizeHop);
int sms_synthesize (SMS_Data *pSmsFrame, float*pFSynthesis,  SMS_SynthParams *pSynthParams);
int sms_detectPeaks (float *pFMagSpectrum, float *pAPhaSpectrum, int sizeMag, SMS_Peak *pSpectralPeaks, SMS_AnalParams *pAnalParams);
void sms_harmDetection (SMS_AnalFrame *pFrame, float fRefFundamental, SMS_AnalParams *pAnalParams);
void sms_fillHeader (SMS_Header *pSmsHeader,  int nFrames, SMS_AnalParams *pAnalParams, int nTracks, int iOriginalSRate);
*/


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
XEN_NARGIFY_5(g_sms_allocFrame_w, g_sms_allocFrame)
XEN_NARGIFY_4(g_sms_interpolateFrames_w, g_sms_interpolateFrames)

XEN_NARGIFY_1(g_sms_make_floats_w, g_sms_make_floats)
XEN_NARGIFY_1(g_sms_free_floats_w, g_sms_free_floats)
XEN_NARGIFY_2(g_sms_floats_ref_w, g_sms_floats_ref)
XEN_NARGIFY_3(g_sms_floats_set_w, g_sms_floats_set)

XEN_NARGIFY_0(g_sms_make_SMS_Data_w, g_sms_make_SMS_Data)
XEN_NARGIFY_1(g_sms_free_SMS_Data_w, g_sms_free_SMS_Data)
XEN_NARGIFY_1(g_SMS_Data_nTracks_w, g_SMS_Data_nTracks)
XEN_NARGIFY_1(g_SMS_Data_nCoeff_w, g_SMS_Data_nCoeff)
XEN_NARGIFY_1(g_SMS_Data_sizeData_w, g_SMS_Data_sizeData)
XEN_NARGIFY_1(g_SMS_Data_pFSinFreq_w, g_SMS_Data_pFSinFreq)
XEN_NARGIFY_1(g_SMS_Data_pFSinAmp_w, g_SMS_Data_pFSinAmp)
XEN_NARGIFY_1(g_SMS_Data_pFSinPha_w, g_SMS_Data_pFSinPha)
XEN_NARGIFY_1(g_SMS_Data_pFStocGain_w, g_SMS_Data_pFStocGain)
XEN_NARGIFY_1(g_SMS_Data_pFStocCoeff_w, g_SMS_Data_pFStocCoeff)

XEN_NARGIFY_0(g_sms_make_SMS_AnalParams_w, g_sms_make_SMS_AnalParams)
XEN_NARGIFY_1(g_sms_free_SMS_AnalParams_w, g_sms_free_SMS_AnalParams)
XEN_NARGIFY_1(g_sms_initAnalysis_w, g_sms_initAnalysis)
XEN_NARGIFY_1(g_sms_freeAnalysis_w, g_sms_freeAnalysis)

XEN_NARGIFY_0(g_sms_make_SMS_SynthParams_w, g_sms_make_SMS_SynthParams)
XEN_NARGIFY_1(g_sms_free_SMS_SynthParams_w, g_sms_free_SMS_SynthParams)
XEN_NARGIFY_1(g_sms_freeSynth_w, g_sms_freeSynth)

XEN_NARGIFY_0(g_sms_make_SMS_Header_w, g_sms_make_SMS_Header)
XEN_NARGIFY_1(g_sms_free_SMS_Header_w, g_sms_free_SMS_Header)
XEN_NARGIFY_1(g_sms_initHeader_w, g_sms_initHeader)
XEN_NARGIFY_1(g_sms_getHeader_w, g_sms_getHeader)
XEN_NARGIFY_2(g_sms_writeHeader_w, g_sms_writeHeader)
XEN_NARGIFY_1(g_sms_frameSizeB_w, g_sms_frameSizeB)

XEN_NARGIFY_0(g_sms_make_SMS_SndHeader_w, g_sms_make_SMS_SndHeader)
XEN_NARGIFY_1(g_sms_free_SMS_SndHeader_w, g_sms_free_SMS_SndHeader)
XEN_NARGIFY_1(g_SMS_SndHeader_nSamples_w, g_SMS_SndHeader_nSamples)
XEN_NARGIFY_1(g_SMS_SndHeader_iSamplingRate_w, g_SMS_SndHeader_iSamplingRate)

XEN_NARGIFY_2(g_sms_allocFrameH_w, g_sms_allocFrameH)
XEN_NARGIFY_2(g_sms_initSynth_w, g_sms_initSynth)
XEN_NARGIFY_2(g_sms_writeFile_w, g_sms_writeFile)
XEN_NARGIFY_2(g_sms_openSF_w, g_sms_openSF)
XEN_NARGIFY_4(g_sms_getFrame_w, g_sms_getFrame)
XEN_NARGIFY_3(g_sms_writeFrame_w, g_sms_writeFrame)
XEN_NARGIFY_4(g_sms_getSound_w, g_sms_getSound)
XEN_NARGIFY_4(g_sms_analyze_w, g_sms_analyze)



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
#define g_sms_allocFrame_w g_sms_allocFrame
#define g_sms_interpolateFrames_w g_sms_interpolateFrames

#define g_sms_make_floats_w g_sms_make_floats
#define g_sms_free_floats_w g_sms_free_floats
#define g_sms_floats_ref_w g_sms_floats_ref
#define g_sms_floats_set_w g_sms_floats_set

#define g_sms_make_SMS_Data_w g_sms_make_SMS_Data
#define g_sms_free_SMS_Data_w g_sms_free_SMS_Data
#define g_SMS_Data_nTracks_w g_SMS_Data_nTracks
#define g_SMS_Data_sizeData_w g_SMS_Data_sizeData
#define g_SMS_Data_pFSinFreq_w g_SMS_Data_pFSinFreq
#define g_SMS_Data_pFSinAmp_w g_SMS_Data_pFSinAmp
#define g_SMS_Data_pFSinPha_w g_SMS_Data_pFSinPha
#define g_SMS_Data_pFStocGain_w g_SMS_Data_pFStocGain
#define g_SMS_Data_pFStocCoeff_w g_SMS_Data_pFStocCoeff

#define g_sms_make_SMS_AnalParams_w g_sms_make_SMS_AnalParams
#define g_sms_free_SMS_AnalParams_w g_sms_free_SMS_AnalParams
#define g_sms_initAnalysis_w g_sms_initAnalysis
#define g_sms_freeAnalysis_w g_sms_freeAnalysis

#define g_sms_make_SMS_SynthParams_w g_sms_make_SMS_SynthParams
#define g_sms_free_SMS_SynthParams_w g_sms_free_SMS_SynthParams
#define g_sms_freeSynth_w g_sms_freeSynth

#define g_sms_make_SMS_Header_w g_sms_make_SMS_Header
#define g_sms_free_SMS_Header_w g_sms_free_SMS_Header
#define g_sms_getHeader_w g_sms_getHeader
#define g_sms_writeHeader_w g_sms_writeHeader
#define g_sms_initHeader_w g_sms_initHeader
#define g_sms_frameSizeB_w g_sms_frameSizeB

#define g_sms_make_SMS_SndHeader_w g_sms_make_SMS_SndHeader
#define g_sms_free_SMS_SndHeader_w g_sms_free_SMS_SndHeader
#define g_SMS_SndHeader_nSamples_w g_SMS_SndHeader_nSamples
#define g_SMS_SndHeader_iSamplingRate_w g_SMS_SndHeader_iSamplingRate

#define g_sms_initSynth_w g_sms_initSynth
#define g_sms_allocFrameH_w g_sms_allocFrameH
#define g_sms_writeFile_w g_sms_writeFile
#define g_sms_openSF_w g_sms_openSF
#define g_sms_getFrame_w g_sms_getFrame
#define g_sms_writeFrame_w g_sms_writeFrame
#define g_sms_getSound_w g_sms_getSound)
#define g_sms_analyze_w g_sms_analyze

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
  XEN_DEFINE_PROCEDURE(S_sms_allocFrame,   g_sms_allocFrame_w,   5, 0, 0, H_sms_allocFrame);
  XEN_DEFINE_PROCEDURE(S_sms_interpolateFrames, g_sms_interpolateFrames_w, 4, 0, 0, H_sms_interpolateFrames);

  XEN_DEFINE_PROCEDURE(S_sms_free_floats,  g_sms_free_floats_w,  1, 0, 0, H_sms_free_floats);
  XEN_DEFINE_PROCEDURE(S_sms_make_floats,  g_sms_make_floats_w,  1, 0, 0, H_sms_make_floats);
  XEN_DEFINE_PROCEDURE(S_sms_floats_ref,   g_sms_floats_ref_w,   2, 0, 0, H_sms_floats_ref);
  XEN_DEFINE_PROCEDURE(S_sms_floats_set,   g_sms_floats_set_w,   3, 0, 0, H_sms_floats_set);

  XEN_DEFINE_PROCEDURE(S_sms_free_SMS_Data, g_sms_free_SMS_Data_w, 1, 0, 0, H_sms_free_SMS_Data);
  XEN_DEFINE_PROCEDURE(S_sms_make_SMS_Data, g_sms_make_SMS_Data_w, 0, 0, 0, H_sms_make_SMS_Data);

  DEFINE_READER(nTracks,           g_SMS_Data_nTracks_w,           H_SMS_Data_nTracks);
  DEFINE_READER(nCoeff,           g_SMS_Data_nCoeff_w,           H_SMS_Data_nCoeff);
  DEFINE_READER(sizeData,           g_SMS_Data_sizeData_w,           H_SMS_Data_sizeData);
  DEFINE_READER(pFSinFreq,           g_SMS_Data_pFSinFreq_w,           H_SMS_Data_pFSinFreq);
  DEFINE_READER(pFSinAmp,           g_SMS_Data_pFSinAmp_w,           H_SMS_Data_pFSinAmp);
  DEFINE_READER(pFSinPha,           g_SMS_Data_pFSinPha_w,           H_SMS_Data_pFSinPha);
  DEFINE_READER(pFStocGain,           g_SMS_Data_pFStocGain_w,           H_SMS_Data_pFStocGain);
  DEFINE_READER(pFStocCoeff,           g_SMS_Data_pFStocCoeff_w,           H_SMS_Data_pFStocCoeff);

  XEN_DEFINE_PROCEDURE(S_sms_free_SMS_AnalParams, g_sms_free_SMS_AnalParams_w, 1, 0, 0, H_sms_free_SMS_AnalParams);
  XEN_DEFINE_PROCEDURE(S_sms_make_SMS_AnalParams, g_sms_make_SMS_AnalParams_w, 0, 0, 0, H_sms_make_SMS_AnalParams);
  XEN_DEFINE_PROCEDURE(S_sms_initAnalysis, g_sms_initAnalysis_w, 1, 0, 0, H_sms_initAnalysis);
  XEN_DEFINE_PROCEDURE(S_sms_freeAnalysis, g_sms_freeAnalysis_w, 1, 0, 0, H_sms_freeAnalysis);

  XEN_DEFINE_PROCEDURE(S_sms_free_SMS_SynthParams, g_sms_free_SMS_SynthParams_w, 1, 0, 0, H_sms_free_SMS_SynthParams);
  XEN_DEFINE_PROCEDURE(S_sms_make_SMS_SynthParams, g_sms_make_SMS_SynthParams_w, 0, 0, 0, H_sms_make_SMS_SynthParams);
  XEN_DEFINE_PROCEDURE(S_sms_freeSynth, g_sms_freeSynth_w, 1, 0, 0, H_sms_freeSynth);

  XEN_DEFINE_PROCEDURE(S_sms_free_SMS_Header, g_sms_free_SMS_Header_w, 1, 0, 0, H_sms_free_SMS_Header);
  XEN_DEFINE_PROCEDURE(S_sms_make_SMS_Header, g_sms_make_SMS_Header_w, 0, 0, 0, H_sms_make_SMS_Header);
  XEN_DEFINE_PROCEDURE(S_sms_initHeader, g_sms_initHeader_w, 1, 0, 0, H_sms_initHeader);
  XEN_DEFINE_PROCEDURE(S_sms_getHeader, g_sms_getHeader_w, 1, 0, 0, H_sms_getHeader);
  XEN_DEFINE_PROCEDURE(S_sms_writeHeader, g_sms_writeHeader_w, 2, 0, 0, H_sms_writeHeader);
  XEN_DEFINE_PROCEDURE(S_sms_frameSizeB, g_sms_frameSizeB_w, 1, 0, 0, H_sms_frameSizeB);

  XEN_DEFINE_PROCEDURE(S_sms_free_SMS_SndHeader, g_sms_free_SMS_SndHeader_w, 1, 0, 0, H_sms_free_SMS_SndHeader);
  XEN_DEFINE_PROCEDURE(S_sms_make_SMS_SndHeader, g_sms_make_SMS_SndHeader_w, 0, 0, 0, H_sms_make_SMS_SndHeader);
  DEFINE_READER(nSamples,           g_SMS_SndHeader_nSamples_w,           H_SMS_SndHeader_nSamples);
  DEFINE_READER(iSamplingRate,           g_SMS_SndHeader_iSamplingRate_w,           H_SMS_SndHeader_iSamplingRate);

  XEN_DEFINE_PROCEDURE(S_sms_initSynth, g_sms_initSynth_w, 2, 0, 0, H_sms_initSynth);
  XEN_DEFINE_PROCEDURE(S_sms_allocFrameH, g_sms_allocFrameH_w, 2, 0, 0, H_sms_allocFrameH);
  XEN_DEFINE_PROCEDURE(S_sms_writeFile, g_sms_writeFile_w, 2, 0, 0, H_sms_writeFile);
  XEN_DEFINE_PROCEDURE(S_sms_openSF, g_sms_openSF_w, 2, 0, 0, H_sms_openSF);
  XEN_DEFINE_PROCEDURE(S_sms_getFrame, g_sms_getFrame_w, 4, 0, 0, H_sms_getFrame);
  XEN_DEFINE_PROCEDURE(S_sms_writeFrame, g_sms_writeFrame_w, 3, 0, 0, H_sms_writeFrame);
  XEN_DEFINE_PROCEDURE(S_sms_getSound, g_sms_getSound_w, 4, 0, 0, H_sms_getSound);
  XEN_DEFINE_PROCEDURE(S_sms_analyze, g_sms_analyze_w, 4, 0, 0, H_sms_analyze);

}

#endif
