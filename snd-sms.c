/* libSMS support for Snd */

#include "snd.h"

#if HAVE_LIBSMS && HAVE_EXTENSION_LANGUAGE

#include <sms.h>

/* libsms by Xavier Serra and R. Eakin */

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

#define C_TO_XEN_SMS_Header(Value) ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("SMS_Header"), XEN_WRAP_C_POINTER(Value)) : XEN_FALSE)
#define XEN_TO_C_SMS_Header(Value) ((SMS_Header *)(XEN_UNWRAP_C_POINTER(XEN_CADR(Value))))
#define XEN_SMS_Header_P(Value)    (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                                        (strcmp("SMS_Header", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))

#define C_TO_XEN_SMS_SndHeader(Value) ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("SMS_SndHeader"), XEN_WRAP_C_POINTER(Value)) : XEN_FALSE)
#define XEN_TO_C_SMS_SndHeader(Value) ((SMS_SndHeader *)(XEN_UNWRAP_C_POINTER(XEN_CADR(Value))))
#define XEN_SMS_SndHeader_P(Value)    (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                                        (strcmp("SMS_SndHeader", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))

#define C_TO_XEN_SMS_AnalParams(Value) ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("SMS_AnalParams"), XEN_WRAP_C_POINTER(Value)) : XEN_FALSE)
#define XEN_TO_C_SMS_AnalParams(Value) ((SMS_AnalParams *)(XEN_UNWRAP_C_POINTER(XEN_CADR(Value))))
#define XEN_SMS_AnalParams_P(Value)    (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                                        (strcmp("SMS_AnalParams", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))

#define C_TO_XEN_SMS_SynthParams(Value) ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("SMS_SynthParams"), XEN_WRAP_C_POINTER(Value)) : XEN_FALSE)
#define XEN_TO_C_SMS_SynthParams(Value) ((SMS_SynthParams *)(XEN_UNWRAP_C_POINTER(XEN_CADR(Value))))
#define XEN_SMS_SynthParams_P(Value)    (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                                        (strcmp("SMS_SynthParams", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))



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
  XEN_ASSERT_TYPE(XEN_SMS_Data_P(ptr) || XEN_SMS_Header_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "nTracks", "SMS_Data or SMS_Header pointer");
  if (XEN_SMS_Header_P(ptr))
    return(C_TO_XEN_INT((XEN_TO_C_SMS_Header(ptr))->nTracks));
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


/* unused: prevFrame soundBuffer synthBuffer pFrames ppFrames pFSpectrumWindow fftw */

static XEN g_SMS_AnalParams_iDebugMode(XEN ptr)
{
  #define H_SMS_AnalParams_iDebugMode "(" FIELD_PREFIX "iDebugMode SMS_AnalParams) is the debug mode (SMS_DBG)"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iDebugMode", "SMS_AnalParams pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_AnalParams(ptr))->iDebugMode));
}


static XEN g_SMS_AnalParams_iFormat(XEN ptr)
{
  #define H_SMS_AnalParams_iFormat "(" FIELD_PREFIX "iFormat SMS_AnalParams) is the analysis format code"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr) || XEN_SMS_Header_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iFormat", "SMS_AnalParams or SMS_Header pointer");
  if (XEN_SMS_Header_P(ptr))
    return(C_TO_XEN_INT((XEN_TO_C_SMS_Header(ptr))->iFormat));
  return(C_TO_XEN_INT((XEN_TO_C_SMS_AnalParams(ptr))->iFormat));
}


static XEN g_SMS_AnalParams_iSoundType(XEN ptr)
{
  #define H_SMS_AnalParams_iSoundType "(" FIELD_PREFIX "iSoundType SMS_AnalParams) is the type of sound to be analyzed"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iSoundType", "SMS_AnalParams pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_AnalParams(ptr))->iSoundType));
}


static XEN g_SMS_AnalParams_iFrameRate(XEN ptr)
{
  #define H_SMS_AnalParams_iFrameRate "(" FIELD_PREFIX "iFrameRate SMS_AnalParams) is the rate in Hz of the data frames"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr) || XEN_SMS_Header_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iFrameRate", "SMS_AnalParams or SMS_Header pointer");
  if (XEN_SMS_Header_P(ptr))
    return(C_TO_XEN_INT((XEN_TO_C_SMS_Header(ptr))->iFrameRate));
  return(C_TO_XEN_INT((XEN_TO_C_SMS_AnalParams(ptr))->iFrameRate));
}


static XEN g_SMS_AnalParams_nStochasticCoeff(XEN ptr)
{
  #define H_SMS_AnalParams_nStochasticCoeff "(" FIELD_PREFIX "nStochasticCoeff SMS_AnalParams) is the number of stochastic coefficients per frame"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr) || XEN_SMS_Header_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "nStochasticCoeff", "SMS_AnalParams or SMS_Header pointer");
  if (XEN_SMS_Header_P(ptr))
    return(C_TO_XEN_INT((XEN_TO_C_SMS_Header(ptr))->nStochasticCoeff));
  return(C_TO_XEN_INT((XEN_TO_C_SMS_AnalParams(ptr))->nStochasticCoeff));
}


static XEN g_SMS_AnalParams_iDefaultSizeWindow(XEN ptr)
{
  #define H_SMS_AnalParams_iDefaultSizeWindow "(" FIELD_PREFIX "iDefaultSizeWindow SMS_AnalParams) is the size of the analysis window in samples"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iDefaultSizeWindow", "SMS_AnalParams pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_AnalParams(ptr))->iDefaultSizeWindow));
}


static XEN g_SMS_AnalParams_nGuides(XEN ptr)
{
  #define H_SMS_AnalParams_nGuides "(" FIELD_PREFIX "nGuides SMS_AnalParams) is the number of guides used for peak detection"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "nGuides", "SMS_AnalParams pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_AnalParams(ptr))->nGuides));
}


static XEN g_SMS_AnalParams_iCleanTracks(XEN ptr)
{
  #define H_SMS_AnalParams_iCleanTracks "(" FIELD_PREFIX "iCleanTracks SMS_AnalParams) is the number of guides used for peak detection"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iCleanTracks", "SMS_AnalParams pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_AnalParams(ptr))->iCleanTracks));
}


static XEN g_SMS_AnalParams_iRefHarmonic(XEN ptr)
{
  #define H_SMS_AnalParams_iRefHarmonic "(" FIELD_PREFIX "iRefHarmonic SMS_AnalParams) is the reference harmonic to use for fundamental detection"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iRefHarmonic", "SMS_AnalParams pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_AnalParams(ptr))->iRefHarmonic));
}


static XEN g_SMS_AnalParams_iMinTrackLength(XEN ptr)
{
  #define H_SMS_AnalParams_iMinTrackLength "(" FIELD_PREFIX "iMinTrackLength SMS_AnalParams) is the minimum length in samples of a track"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iMinTrackLength", "SMS_AnalParams pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_AnalParams(ptr))->iMinTrackLength));
}


static XEN g_SMS_AnalParams_iMaxSleepingTime(XEN ptr)
{
  #define H_SMS_AnalParams_iMaxSleepingTime "(" FIELD_PREFIX "iMaxSleepingTime SMS_AnalParams) is the maximum sleeping time for a track"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iMaxSleepingTime", "SMS_AnalParams pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_AnalParams(ptr))->iMaxSleepingTime));
}


static XEN g_SMS_AnalParams_iSizeSound(XEN ptr)
{
  #define H_SMS_AnalParams_iSizeSound "(" FIELD_PREFIX "iSizeSound SMS_AnalParams) is the total size in samples of the sound to be analyzed"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iSizeSound", "SMS_AnalParams pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_AnalParams(ptr))->iSizeSound));
}


static XEN g_SMS_AnalParams_iWindowType(XEN ptr)
{
  #define H_SMS_AnalParams_iWindowType "(" FIELD_PREFIX "iWindowType SMS_AnalParams) is the type of FFT analysis window"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iWindowType", "SMS_AnalParams pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_AnalParams(ptr))->iWindowType));
}


static XEN g_SMS_AnalParams_iMaxDelayFrames(XEN ptr)
{
  #define H_SMS_AnalParams_iMaxDelayFrames "(" FIELD_PREFIX "iMaxDelayFrames SMS_AnalParams) is the maximum number of frames to delay before peak continuation"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iMaxDelayFrames", "SMS_AnalParams pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_AnalParams(ptr))->iMaxDelayFrames));
}


static XEN g_SMS_AnalParams_iAnalysisDirection(XEN ptr)
{
  #define H_SMS_AnalParams_iAnalysisDirection "(" FIELD_PREFIX "iAnalysisDirection SMS_AnalParams) is the analysis direction"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iAnalysisDirection", "SMS_AnalParams pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_AnalParams(ptr))->iAnalysisDirection));
}


static XEN g_SMS_AnalParams_fLowestFundamental(XEN ptr)
{
  #define H_SMS_AnalParams_fLowestFundamental "(" FIELD_PREFIX "fLowestFundamental SMS_AnalParams) is the lowest fundamental frequency in Hz"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "fLowestFundamental", "SMS_AnalParams pointer");
  return(C_TO_XEN_DOUBLE((XEN_TO_C_SMS_AnalParams(ptr))->fLowestFundamental));
}


static XEN g_SMS_AnalParams_fHighestFundamental(XEN ptr)
{
  #define H_SMS_AnalParams_fHighestFundamental "(" FIELD_PREFIX "fHighestFundamental SMS_AnalParams) is the highest fundamental frequency in Hz"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "fHighestFundamental", "SMS_AnalParams pointer");
  return(C_TO_XEN_DOUBLE((XEN_TO_C_SMS_AnalParams(ptr))->fHighestFundamental));
}


static XEN g_SMS_AnalParams_fDefaultFundamental(XEN ptr)
{
  #define H_SMS_AnalParams_fDefaultFundamental "(" FIELD_PREFIX "fDefaultFundamental SMS_AnalParams) is the default fundamental in Hz"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "fDefaultFundamental", "SMS_AnalParams pointer");
  return(C_TO_XEN_DOUBLE((XEN_TO_C_SMS_AnalParams(ptr))->fDefaultFundamental));
}


static XEN g_SMS_AnalParams_fPeakContToGuide(XEN ptr)
{
  #define H_SMS_AnalParams_fPeakContToGuide "(" FIELD_PREFIX "fPeakContToGuide SMS_AnalParams) is the contribution of the previous peak to the current guide (between 0 and 1)"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "fPeakContToGuide", "SMS_AnalParams pointer");
  return(C_TO_XEN_DOUBLE((XEN_TO_C_SMS_AnalParams(ptr))->fPeakContToGuide));
}


static XEN g_SMS_AnalParams_fFundContToGuide(XEN ptr)
{
  #define H_SMS_AnalParams_fFundContToGuide "(" FIELD_PREFIX "fFundContToGuide SMS_AnalParams) is the contribution of the fundamental to the current guide (between 0 and 1)"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "fFundContToGuide", "SMS_AnalParams pointer");
  return(C_TO_XEN_DOUBLE((XEN_TO_C_SMS_AnalParams(ptr))->fFundContToGuide));
}


static XEN g_SMS_AnalParams_fFreqDeviation(XEN ptr)
{
  #define H_SMS_AnalParams_fFreqDeviation "(" FIELD_PREFIX "fFreqDeviation SMS_AnalParams) is the maximum deviation from peak to peak"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "fFreqDeviation", "SMS_AnalParams pointer");
  return(C_TO_XEN_DOUBLE((XEN_TO_C_SMS_AnalParams(ptr))->fFreqDeviation));
}


static XEN g_SMS_AnalParams_fSizeWindow(XEN ptr)
{
  #define H_SMS_AnalParams_fSizeWindow "(" FIELD_PREFIX "fSizeWindow SMS_AnalParams) is the size in periods of the analysis window"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "fSizeWindow", "SMS_AnalParams pointer");
  return(C_TO_XEN_DOUBLE((XEN_TO_C_SMS_AnalParams(ptr))->fSizeWindow));
}


static XEN g_SMS_AnalParams_fMinRefHarmMag(XEN ptr)
{
  #define H_SMS_AnalParams_fMinRefHarmMag "(" FIELD_PREFIX "fMinRefHarmMag SMS_AnalParams) is the minimum magnitude in dB of the reference peak"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "fMinRefHarmMag", "SMS_AnalParams pointer");
  return(C_TO_XEN_DOUBLE((XEN_TO_C_SMS_AnalParams(ptr))->fMinRefHarmMag));
}


static XEN g_SMS_AnalParams_fRefHarmMagDiffFromMax(XEN ptr)
{
  #define H_SMS_AnalParams_fRefHarmMagDiffFromMax "(" FIELD_PREFIX "fRefHarmMagDiffFromMax SMS_AnalParams) is the maximum difference from the \
reference peak to the highest peak"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "fRefHarmMagDiffFromMax", "SMS_AnalParams pointer");
  return(C_TO_XEN_DOUBLE((XEN_TO_C_SMS_AnalParams(ptr))->fRefHarmMagDiffFromMax));
}


static XEN g_SMS_AnalParams_fHighestFreq(XEN ptr)
{
  #define H_SMS_AnalParams_fHighestFreq "(" FIELD_PREFIX "fHighestFreq SMS_AnalParams) is the highest frequency to be searched"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "fHighestFreq", "SMS_AnalParams pointer");
  return(C_TO_XEN_DOUBLE((XEN_TO_C_SMS_AnalParams(ptr))->fHighestFreq));
}


static XEN g_SMS_AnalParams_fMinPeakMag(XEN ptr)
{
  #define H_SMS_AnalParams_fMinPeakMag "(" FIELD_PREFIX "MinPeakMagq SMS_AnalParams) is the minimum magnitude for a good peak (in dB)"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "fMinPeakMag", "SMS_AnalParams pointer");
  return(C_TO_XEN_DOUBLE((XEN_TO_C_SMS_AnalParams(ptr))->fMinPeakMag));
}


static XEN g_SMS_AnalParams_fResidualPercentage(XEN ptr)
{
  #define H_SMS_AnalParams_fResidualPercentage "(" FIELD_PREFIX "ResidualPercentageq SMS_AnalParams) is the accumalated residual percentage"
  XEN_ASSERT_TYPE(XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "fResidualPercentage", "SMS_AnalParams pointer");
  return(C_TO_XEN_DOUBLE((XEN_TO_C_SMS_AnalParams(ptr))->fResidualPercentage));
}





/* -------------------------------- SMS_SynthParams -------------------------------- */

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


static XEN g_SMS_SynthParams_iStochasticType(XEN ptr)
{
  #define H_SMS_SynthParams_iStochasticType "(" FIELD_PREFIX "iStochasticType SMS_SynthParams) is the type of the stochastic model"
  XEN_ASSERT_TYPE(XEN_SMS_SynthParams_P(ptr) || 
		  XEN_SMS_Header_P(ptr) ||
		  XEN_SMS_AnalParams_P(ptr), 
		  ptr, XEN_ONLY_ARG, FIELD_PREFIX "iStochasticType", "SMS_SynthParams, SMS_Header, or SMS_AnalParams pointer");
  if (XEN_SMS_SynthParams_P(ptr))
    return(C_TO_XEN_INT((XEN_TO_C_SMS_SynthParams(ptr))->iStochasticType));
  if (XEN_SMS_Header_P(ptr))
    return(C_TO_XEN_INT((XEN_TO_C_SMS_Header(ptr))->iStochasticType));
  return(C_TO_XEN_INT((XEN_TO_C_SMS_AnalParams(ptr))->iStochasticType));
}


static XEN g_SMS_SynthParams_iSynthesisType(XEN ptr)
{
  #define H_SMS_SynthParams_iSynthesisType "(" FIELD_PREFIX "iSynthesisType SMS_SynthParams) is the type of the synthesis to perform"
  XEN_ASSERT_TYPE(XEN_SMS_SynthParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iStochasticType", "SMS_SynthParams pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_SynthParams(ptr))->iSynthesisType));
}


static XEN g_SMS_SynthParams_iDetSynthType(XEN ptr)
{
  #define H_SMS_SynthParams_iDetSynthType "(" FIELD_PREFIX "iDetSynthType SMS_SynthParams) is the method for synthesizing the deterministic component"
  XEN_ASSERT_TYPE(XEN_SMS_SynthParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iDetSynthType", "SMS_SynthParams pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_SynthParams(ptr))->iDetSynthType));
}


static XEN g_SMS_SynthParams_iOriginalSRate(XEN ptr)
{
  #define H_SMS_SynthParams_iOriginalSRate "(" FIELD_PREFIX "iOriginalSRate SMS_SynthParams) is the sound source sampling rate"
  XEN_ASSERT_TYPE(XEN_SMS_SynthParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iOriginalSRate", "SMS_SynthParams pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_SynthParams(ptr))->iOriginalSRate));
}


static XEN g_SMS_SynthParams_iSamplingRate(XEN ptr)
{
  #define H_SMS_SynthParams_iSamplingRate "(" FIELD_PREFIX "iSamplingRate SMS_SynthParams) is the synthesis ampling rate"
  XEN_ASSERT_TYPE(XEN_SMS_SynthParams_P(ptr) || 
		  XEN_SMS_AnalParams_P(ptr) ||
		  XEN_SMS_SndHeader_P(ptr) ||
		  XEN_SMS_Header_P(ptr), 
		  ptr, XEN_ONLY_ARG, FIELD_PREFIX "iSamplingRate", "SMS_SynthParams, SMS_Header, SMS_SndHeader, or SMS_AnalParams pointer");
  if (XEN_SMS_SynthParams_P(ptr))
    return(C_TO_XEN_INT((XEN_TO_C_SMS_SynthParams(ptr))->iSamplingRate));
  if (XEN_SMS_Header_P(ptr))
    return(C_TO_XEN_INT((XEN_TO_C_SMS_Header(ptr))->iSamplingRate));
  if (XEN_SMS_SndHeader_P(ptr))
    return(C_TO_XEN_INT((XEN_TO_C_SMS_SndHeader(ptr))->iSamplingRate));
  return(C_TO_XEN_INT((XEN_TO_C_SMS_AnalParams(ptr))->iSamplingRate));
}


static XEN g_SMS_SynthParams_sizeHop(XEN ptr)
{
  #define H_SMS_SynthParams_sizeHop "(" FIELD_PREFIX "sizeHop SMS_SynthParams) is the number of samples to synthesize for each frame"
  XEN_ASSERT_TYPE(XEN_SMS_SynthParams_P(ptr) || XEN_SMS_AnalParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "sizeHop", "SMS_SynthParams or SMS_AnalParams pointer");
  if (XEN_SMS_SynthParams_P(ptr))
    return(C_TO_XEN_INT((XEN_TO_C_SMS_SynthParams(ptr))->sizeHop));
  return(C_TO_XEN_INT((XEN_TO_C_SMS_AnalParams(ptr))->sizeHop));
}


static XEN g_SMS_SynthParams_origSizeHop(XEN ptr)
{
  #define H_SMS_SynthParams_origSizeHop "(" FIELD_PREFIX "origSizeHop SMS_SynthParams) is the original number of samples per frame"
  XEN_ASSERT_TYPE(XEN_SMS_SynthParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "origSizeHop", "SMS_SynthParams pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_SynthParams(ptr))->origSizeHop));
}


static XEN g_SMS_SynthParams_fStocGain(XEN ptr)
{
  #define H_SMS_SynthParams_fStocGain "(" FIELD_PREFIX "fStocGain SMS_SynthParams) is the gain on the stochastic component"
  XEN_ASSERT_TYPE(XEN_SMS_SynthParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "fStocGain", "SMS_SynthParams pointer");
  return(C_TO_XEN_DOUBLE((XEN_TO_C_SMS_SynthParams(ptr))->fStocGain));
}


static XEN g_SMS_SynthParams_fTranspose(XEN ptr)
{
  #define H_SMS_SynthParams_fTranspose "(" FIELD_PREFIX "fTranspose SMS_SynthParams) is the frequency transposing multiplier"
  XEN_ASSERT_TYPE(XEN_SMS_SynthParams_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "fTranspose", "SMS_SynthParams pointer");
  return(C_TO_XEN_DOUBLE((XEN_TO_C_SMS_SynthParams(ptr))->fTranspose));
}

/* unused: pFDetWindow pFStocWindow prevFrame fftw pFFTBuff */



/* -------------------------------- SMS_Header -------------------------------- */

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


/* unused: nLoopRecords nSpecEnvelopePoints pILoopRecords pFSpectralEnvelope */

static XEN g_SMS_Header_iSmsMagic(XEN ptr)
{
  #define H_SMS_Header_iSmsMagic "(" FIELD_PREFIX "iSmsMagic SMS_Header) is the SMS file identifier"
  XEN_ASSERT_TYPE(XEN_SMS_Header_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iSmsMagic", "SMS_Header pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_Header(ptr))->iSmsMagic));
}


static XEN g_SMS_Header_iHeadBSize(XEN ptr)
{
  #define H_SMS_Header_iHeadBSize "(" FIELD_PREFIX "iHeadBSize SMS_Header) is the header size in bytes"
  XEN_ASSERT_TYPE(XEN_SMS_Header_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iHeadBSize", "SMS_Header pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_Header(ptr))->iHeadBSize));
}


static XEN g_SMS_Header_nFrames(XEN ptr)
{
  #define H_SMS_Header_nFrames "(" FIELD_PREFIX "nFrames SMS_Header) is the number of data frames"
  XEN_ASSERT_TYPE(XEN_SMS_Header_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "nFrames", "SMS_Header pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_Header(ptr))->nFrames));
}


static XEN g_SMS_Header_iFrameBSize(XEN ptr)
{
  #define H_SMS_Header_iFrameBSize "(" FIELD_PREFIX "iFrameBSize SMS_Header) is the data frame size in bytes"
  XEN_ASSERT_TYPE(XEN_SMS_Header_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iFrameBSize", "SMS_Header pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_Header(ptr))->iFrameBSize));
}


static XEN g_SMS_Header_iBegSteadyState(XEN ptr)
{
  #define H_SMS_Header_iBegSteadyState "(" FIELD_PREFIX "iBegSteadyState SMS_Header) is the record number of the beginning of the steady state"
  XEN_ASSERT_TYPE(XEN_SMS_Header_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iBegSteadyState", "SMS_Header pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_Header(ptr))->iBegSteadyState));
}


static XEN g_SMS_Header_iEndSteadyState(XEN ptr)
{
  #define H_SMS_Header_iEndSteadyState "(" FIELD_PREFIX "iEndSteadyState SMS_Header) is the record number of the end of the steady state"
  XEN_ASSERT_TYPE(XEN_SMS_Header_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "iEndSteadyState", "SMS_Header pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_Header(ptr))->iEndSteadyState));
}


static XEN g_SMS_Header_nTextCharacters(XEN ptr)
{
  #define H_SMS_Header_nTextCharacters "(" FIELD_PREFIX "nTextCharacters SMS_Header) is the number of text characters"
  XEN_ASSERT_TYPE(XEN_SMS_Header_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "nTextCharacters", "SMS_Header pointer");
  return(C_TO_XEN_INT((XEN_TO_C_SMS_Header(ptr))->nTextCharacters));
}


static XEN g_SMS_Header_fAmplitude(XEN ptr)
{
  #define H_SMS_Header_fAmplitude "(" FIELD_PREFIX "fAmplitude SMS_Header) is the average amplitude of the sound"
  XEN_ASSERT_TYPE(XEN_SMS_Header_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "fAmplitude", "SMS_Header pointer");
  return(C_TO_XEN_DOUBLE((XEN_TO_C_SMS_Header(ptr))->fAmplitude));
}


static XEN g_SMS_Header_fFrequency(XEN ptr)
{
  #define H_SMS_Header_fFrequency "(" FIELD_PREFIX "fFrequency SMS_Header) is the average fundamental frequency of the sound"
  XEN_ASSERT_TYPE(XEN_SMS_Header_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "fFrequency", "SMS_Header pointer");
  return(C_TO_XEN_DOUBLE((XEN_TO_C_SMS_Header(ptr))->fFrequency));
}


static XEN g_SMS_Header_fResidualPerc(XEN ptr)
{
  #define H_SMS_Header_fResidualPerc "(" FIELD_PREFIX "fResidualPerc SMS_Header) is the percentage of residual to original"
  XEN_ASSERT_TYPE(XEN_SMS_Header_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "fResidualPerc", "SMS_Header pointer");
  return(C_TO_XEN_DOUBLE((XEN_TO_C_SMS_Header(ptr))->fResidualPerc));
}


static XEN g_SMS_Header_pChTextCharacters(XEN ptr)
{
  #define H_SMS_Header_pChTextCharacters "(" FIELD_PREFIX "pChTextCharacters SMS_Header) is the sound's text string"
  XEN_ASSERT_TYPE(XEN_SMS_Header_P(ptr), ptr, XEN_ONLY_ARG, FIELD_PREFIX "pChTextCharacters", "SMS_Header pointer");
  return(C_TO_XEN_STRING((XEN_TO_C_SMS_Header(ptr))->pChTextCharacters));
}



/* -------------------------------- SMS_SndHeader -------------------------------- */

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


/* channelCount and sizeHeader not used */



/* -------------------------------- FILE* -------------------------------- */

#define C_TO_XEN_FILE(Value) ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("FILE*"), XEN_WRAP_C_POINTER(Value)) : XEN_FALSE)
#define XEN_TO_C_FILE(Value) ((FILE *)(XEN_UNWRAP_C_POINTER(XEN_CADR(Value))))
#define XEN_FILE_P(Value)    (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                                  (strcmp("FILE*", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))


#define S_sms_fclose "sms_fclose"

static XEN g_sms_fclose(XEN fp)
{
  #define H_sms_fclose "(" S_sms_fclose " fp) fcloses the FILE* fp"
  XEN_ASSERT_TYPE(XEN_FILE_P(fp), fp, XEN_ONLY_ARG, S_sms_fclose, "a FILE pointer");
  fclose(XEN_TO_C_FILE(fp));
  return(XEN_FALSE);
}


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

XEN_NARGIFY_1(g_sms_fclose_w, g_sms_fclose)
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
XEN_NARGIFY_1(g_SMS_AnalParams_iDebugMode_w, g_SMS_AnalParams_iDebugMode) 
XEN_NARGIFY_1(g_SMS_AnalParams_iFormat_w, g_SMS_AnalParams_iFormat)
XEN_NARGIFY_1(g_SMS_AnalParams_iSoundType_w, g_SMS_AnalParams_iSoundType)
XEN_NARGIFY_1(g_SMS_AnalParams_iFrameRate_w, g_SMS_AnalParams_iFrameRate)
XEN_NARGIFY_1(g_SMS_AnalParams_nStochasticCoeff_w, g_SMS_AnalParams_nStochasticCoeff) 
XEN_NARGIFY_1(g_SMS_AnalParams_iDefaultSizeWindow_w, g_SMS_AnalParams_iDefaultSizeWindow)
XEN_NARGIFY_1(g_SMS_AnalParams_nGuides_w, g_SMS_AnalParams_nGuides)
XEN_NARGIFY_1(g_SMS_AnalParams_iCleanTracks_w, g_SMS_AnalParams_iCleanTracks)
XEN_NARGIFY_1(g_SMS_AnalParams_iRefHarmonic_w, g_SMS_AnalParams_iRefHarmonic)
XEN_NARGIFY_1(g_SMS_AnalParams_iMinTrackLength_w, g_SMS_AnalParams_iMinTrackLength)
XEN_NARGIFY_1(g_SMS_AnalParams_iMaxSleepingTime_w, g_SMS_AnalParams_iMaxSleepingTime)
XEN_NARGIFY_1(g_SMS_AnalParams_iSizeSound_w, g_SMS_AnalParams_iSizeSound)
XEN_NARGIFY_1(g_SMS_AnalParams_iWindowType_w, g_SMS_AnalParams_iWindowType)
XEN_NARGIFY_1(g_SMS_AnalParams_iMaxDelayFrames_w, g_SMS_AnalParams_iMaxDelayFrames)
XEN_NARGIFY_1(g_SMS_AnalParams_iAnalysisDirection_w, g_SMS_AnalParams_iAnalysisDirection)
XEN_NARGIFY_1(g_SMS_AnalParams_fLowestFundamental_w, g_SMS_AnalParams_fLowestFundamental)
XEN_NARGIFY_1(g_SMS_AnalParams_fHighestFundamental_w, g_SMS_AnalParams_fHighestFundamental)
XEN_NARGIFY_1(g_SMS_AnalParams_fDefaultFundamental_w, g_SMS_AnalParams_fDefaultFundamental)
XEN_NARGIFY_1(g_SMS_AnalParams_fPeakContToGuide_w, g_SMS_AnalParams_fPeakContToGuide)
XEN_NARGIFY_1(g_SMS_AnalParams_fFundContToGuide_w, g_SMS_AnalParams_fFundContToGuide)
XEN_NARGIFY_1(g_SMS_AnalParams_fFreqDeviation_w, g_SMS_AnalParams_fFreqDeviation)
XEN_NARGIFY_1(g_SMS_AnalParams_fSizeWindow_w, g_SMS_AnalParams_fSizeWindow)
XEN_NARGIFY_1(g_SMS_AnalParams_fMinRefHarmMag_w, g_SMS_AnalParams_fMinRefHarmMag)
XEN_NARGIFY_1(g_SMS_AnalParams_fRefHarmMagDiffFromMax_w, g_SMS_AnalParams_fRefHarmMagDiffFromMax)
XEN_NARGIFY_1(g_SMS_AnalParams_fHighestFreq_w, g_SMS_AnalParams_fHighestFreq)
XEN_NARGIFY_1(g_SMS_AnalParams_fMinPeakMag_w, g_SMS_AnalParams_fMinPeakMag)
XEN_NARGIFY_1(g_SMS_AnalParams_fResidualPercentage_w, g_SMS_AnalParams_fResidualPercentage)

XEN_NARGIFY_0(g_sms_make_SMS_SynthParams_w, g_sms_make_SMS_SynthParams)
XEN_NARGIFY_1(g_sms_free_SMS_SynthParams_w, g_sms_free_SMS_SynthParams)
XEN_NARGIFY_1(g_sms_freeSynth_w, g_sms_freeSynth)
XEN_NARGIFY_1(g_SMS_SynthParams_iStochasticType_w, g_SMS_SynthParams_iStochasticType) 
XEN_NARGIFY_1(g_SMS_SynthParams_iSynthesisType_w, g_SMS_SynthParams_iSynthesisType) 
XEN_NARGIFY_1(g_SMS_SynthParams_iDetSynthType_w, g_SMS_SynthParams_iDetSynthType) 
XEN_NARGIFY_1(g_SMS_SynthParams_iOriginalSRate_w, g_SMS_SynthParams_iOriginalSRate) 
XEN_NARGIFY_1(g_SMS_SynthParams_iSamplingRate_w, g_SMS_SynthParams_iSamplingRate) 
XEN_NARGIFY_1(g_SMS_SynthParams_sizeHop_w, g_SMS_SynthParams_sizeHop) 
XEN_NARGIFY_1(g_SMS_SynthParams_origSizeHop_w, g_SMS_SynthParams_origSizeHop) 
XEN_NARGIFY_1(g_SMS_SynthParams_fStocGain_w, g_SMS_SynthParams_fStocGain) 
XEN_NARGIFY_1(g_SMS_SynthParams_fTranspose_w, g_SMS_SynthParams_fTranspose) 

XEN_NARGIFY_0(g_sms_make_SMS_Header_w, g_sms_make_SMS_Header)
XEN_NARGIFY_1(g_sms_free_SMS_Header_w, g_sms_free_SMS_Header)
XEN_NARGIFY_1(g_sms_initHeader_w, g_sms_initHeader)
XEN_NARGIFY_1(g_sms_getHeader_w, g_sms_getHeader)
XEN_NARGIFY_2(g_sms_writeHeader_w, g_sms_writeHeader)
XEN_NARGIFY_1(g_sms_frameSizeB_w, g_sms_frameSizeB)

XEN_NARGIFY_1(g_SMS_Header_iSmsMagic_w, g_SMS_Header_iSmsMagic)
XEN_NARGIFY_1(g_SMS_Header_iHeadBSize_w, g_SMS_Header_iHeadBSize)
XEN_NARGIFY_1(g_SMS_Header_nFrames_w, g_SMS_Header_nFrames)
XEN_NARGIFY_1(g_SMS_Header_iFrameBSize_w, g_SMS_Header_iFrameBSize)
XEN_NARGIFY_1(g_SMS_Header_iBegSteadyState_w, g_SMS_Header_iBegSteadyState)
XEN_NARGIFY_1(g_SMS_Header_iEndSteadyState_w, g_SMS_Header_iEndSteadyState)
XEN_NARGIFY_1(g_SMS_Header_nTextCharacters_w, g_SMS_Header_nTextCharacters)
XEN_NARGIFY_1(g_SMS_Header_fAmplitude_w, g_SMS_Header_fAmplitude)
XEN_NARGIFY_1(g_SMS_Header_fFrequency_w, g_SMS_Header_fFrequency)
XEN_NARGIFY_1(g_SMS_Header_fResidualPerc_w, g_SMS_Header_fResidualPerc)
XEN_NARGIFY_1(g_SMS_Header_pChTextCharacters_w, g_SMS_Header_pChTextCharacters)

XEN_NARGIFY_0(g_sms_make_SMS_SndHeader_w, g_sms_make_SMS_SndHeader)
XEN_NARGIFY_1(g_sms_free_SMS_SndHeader_w, g_sms_free_SMS_SndHeader)
XEN_NARGIFY_1(g_SMS_SndHeader_nSamples_w, g_SMS_SndHeader_nSamples)

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

#define g_sms_fclose_w g_sms_fclose
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
#define g_SMS_AnalParams_iDebugMode_w g_SMS_AnalParams_iDebugMode 
#define g_SMS_AnalParams_iFormat_w g_SMS_AnalParams_iFormat
#define g_SMS_AnalParams_iSoundType_w g_SMS_AnalParams_iSoundType
#define g_SMS_AnalParams_iFrameRate_w g_SMS_AnalParams_iFrameRate
#define g_SMS_AnalParams_nStochasticCoeff_w g_SMS_AnalParams_nStochasticCoeff 
#define g_SMS_AnalParams_iDefaultSizeWindow_w g_SMS_AnalParams_iDefaultSizeWindow
#define g_SMS_AnalParams_nGuides_w g_SMS_AnalParams_nGuides
#define g_SMS_AnalParams_iCleanTracks_w g_SMS_AnalParams_iCleanTracks
#define g_SMS_AnalParams_iRefHarmonic_w g_SMS_AnalParams_iRefHarmonic
#define g_SMS_AnalParams_iMinTrackLength_w g_SMS_AnalParams_iMinTrackLength
#define g_SMS_AnalParams_iMaxSleepingTime_w g_SMS_AnalParams_iMaxSleepingTime
#define g_SMS_AnalParams_iSizeSound_w g_SMS_AnalParams_iSizeSound
#define g_SMS_AnalParams_iWindowType_w g_SMS_AnalParams_iWindowType
#define g_SMS_AnalParams_iMaxDelayFrames_w g_SMS_AnalParams_iMaxDelayFrames
#define g_SMS_AnalParams_iAnalysisDirection_w g_SMS_AnalParams_iAnalysisDirection
#define g_SMS_AnalParams_fLowestFundamental_w g_SMS_AnalParams_fLowestFundamental
#define g_SMS_AnalParams_fHighestFundamental_w g_SMS_AnalParams_fHighestFundamental
#define g_SMS_AnalParams_fDefaultFundamental_w g_SMS_AnalParams_fDefaultFundamental
#define g_SMS_AnalParams_fPeakContToGuide_w g_SMS_AnalParams_fPeakContToGuide
#define g_SMS_AnalParams_fFundContToGuide_w g_SMS_AnalParams_fFundContToGuide
#define g_SMS_AnalParams_fFreqDeviation_w g_SMS_AnalParams_fFreqDeviation
#define g_SMS_AnalParams_fSizeWindow_w g_SMS_AnalParams_fSizeWindow
#define g_SMS_AnalParams_fMinRefHarmMag_w g_SMS_AnalParams_fMinRefHarmMag
#define g_SMS_AnalParams_fRefHarmMagDiffFromMax_w g_SMS_AnalParams_fRefHarmMagDiffFromMax
#define g_SMS_AnalParams_fHighestFreq_w g_SMS_AnalParams_fHighestFreq
#define g_SMS_AnalParams_fMinPeakMag_w g_SMS_AnalParams_fMinPeakMag
#define g_SMS_AnalParams_fResidualPercentage_w g_SMS_AnalParams_fResidualPercentage

#define g_sms_make_SMS_SynthParams_w g_sms_make_SMS_SynthParams
#define g_sms_free_SMS_SynthParams_w g_sms_free_SMS_SynthParams
#define g_sms_freeSynth_w g_sms_freeSynth
#define g_SMS_SynthParams_iStochasticType_w g_SMS_SynthParams_iStochasticType
#define g_SMS_SynthParams_iSynthesisType_w g_SMS_SynthParams_iSynthesisType
#define g_SMS_SynthParams_iDetSynthType_w g_SMS_SynthParams_iDetSynthType
#define g_SMS_SynthParams_iOriginalSRate_w g_SMS_SynthParams_iOriginalSRate
#define g_SMS_SynthParams_iSamplingRate_w g_SMS_SynthParams_iSamplingRate
#define g_SMS_SynthParams_sizeHop_w g_SMS_SynthParams_sizeHop
#define g_SMS_SynthParams_origSizeHop_w g_SMS_SynthParams_origSizeHop
#define g_SMS_SynthParams_fStocGain_w g_SMS_SynthParams_fStocGain
#define g_SMS_SynthParams_fTranspose_w g_SMS_SynthParams_fTranspose

#define g_sms_make_SMS_Header_w g_sms_make_SMS_Header
#define g_sms_free_SMS_Header_w g_sms_free_SMS_Header
#define g_sms_getHeader_w g_sms_getHeader
#define g_sms_writeHeader_w g_sms_writeHeader
#define g_sms_initHeader_w g_sms_initHeader
#define g_sms_frameSizeB_w g_sms_frameSizeB

#define g_SMS_Header_iSmsMagic_w g_SMS_Header_iSmsMagic
#define g_SMS_Header_iHeadBSize_w g_SMS_Header_iHeadBSize
#define g_SMS_Header_nFrames_w g_SMS_Header_nFrames
#define g_SMS_Header_iFrameBSize_w g_SMS_Header_iFrameBSize
#define g_SMS_Header_iBegSteadyState_w g_SMS_Header_iBegSteadyState
#define g_SMS_Header_iEndSteadyState_w g_SMS_Header_iEndSteadyState
#define g_SMS_Header_nTextCharacters_w g_SMS_Header_nTextCharacters
#define g_SMS_Header_fAmplitude_w g_SMS_Header_fAmplitude
#define g_SMS_Header_fFrequency_w g_SMS_Header_fFrequency
#define g_SMS_Header_fResidualPerc_w g_SMS_Header_fResidualPerc
#define g_SMS_Header_pChTextCharacters_w g_SMS_Header_pChTextCharacters

#define g_sms_make_SMS_SndHeader_w g_sms_make_SMS_SndHeader
#define g_sms_free_SMS_SndHeader_w g_sms_free_SMS_SndHeader
#define g_SMS_SndHeader_nSamples_w g_SMS_SndHeader_nSamples

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

  XEN_DEFINE_PROCEDURE(S_sms_fclose,       g_sms_fclose_w,       1, 0, 0, H_sms_fclose);
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

  DEFINE_READER(iDebugMode, g_SMS_AnalParams_iDebugMode_w, H_SMS_AnalParams_iDebugMode);
  DEFINE_READER(iFormat, g_SMS_AnalParams_iFormat_w, H_SMS_AnalParams_iFormat);
  DEFINE_READER(iSoundType, g_SMS_AnalParams_iSoundType_w, H_SMS_AnalParams_iSoundType);
  DEFINE_READER(iFrameRate, g_SMS_AnalParams_iFrameRate_w, H_SMS_AnalParams_iFrameRate);
  DEFINE_READER(nStochasticCoeff, g_SMS_AnalParams_nStochasticCoeff_w, H_SMS_AnalParams_nStochasticCoeff);
  DEFINE_READER(iDefaultSizeWindow, g_SMS_AnalParams_iDefaultSizeWindow_w, H_SMS_AnalParams_iDefaultSizeWindow);
  DEFINE_READER(nGuides, g_SMS_AnalParams_nGuides_w, H_SMS_AnalParams_nGuides);
  DEFINE_READER(iCleanTracks, g_SMS_AnalParams_iCleanTracks_w, H_SMS_AnalParams_iCleanTracks);
  DEFINE_READER(iRefHarmonic, g_SMS_AnalParams_iRefHarmonic_w, H_SMS_AnalParams_iRefHarmonic);
  DEFINE_READER(iMinTrackLength, g_SMS_AnalParams_iMinTrackLength_w, H_SMS_AnalParams_iMinTrackLength);
  DEFINE_READER(iMaxSleepingTime, g_SMS_AnalParams_iMaxSleepingTime_w, H_SMS_AnalParams_iMaxSleepingTime);
  DEFINE_READER(iSizeSound, g_SMS_AnalParams_iSizeSound_w, H_SMS_AnalParams_iSizeSound);
  DEFINE_READER(iWindowType, g_SMS_AnalParams_iWindowType_w, H_SMS_AnalParams_iWindowType);
  DEFINE_READER(iMaxDelayFrames, g_SMS_AnalParams_iMaxDelayFrames_w, H_SMS_AnalParams_iMaxDelayFrames);
  DEFINE_READER(iAnalysisDirection, g_SMS_AnalParams_iAnalysisDirection_w, H_SMS_AnalParams_iAnalysisDirection);
  DEFINE_READER(fLowestFundamental, g_SMS_AnalParams_fLowestFundamental_w, H_SMS_AnalParams_fLowestFundamental);
  DEFINE_READER(fHighestFundamental, g_SMS_AnalParams_fHighestFundamental_w, H_SMS_AnalParams_fHighestFundamental);
  DEFINE_READER(fDefaultFundamental, g_SMS_AnalParams_fDefaultFundamental_w, H_SMS_AnalParams_fDefaultFundamental);
  DEFINE_READER(fPeakContToGuide, g_SMS_AnalParams_fPeakContToGuide_w, H_SMS_AnalParams_fPeakContToGuide);
  DEFINE_READER(fFundContToGuide, g_SMS_AnalParams_fFundContToGuide_w, H_SMS_AnalParams_fFundContToGuide);
  DEFINE_READER(fFreqDeviation, g_SMS_AnalParams_fFreqDeviation_w, H_SMS_AnalParams_fFreqDeviation);
  DEFINE_READER(fSizeWindow, g_SMS_AnalParams_fSizeWindow_w, H_SMS_AnalParams_fSizeWindow);
  DEFINE_READER(fMinRefHarmMag, g_SMS_AnalParams_fMinRefHarmMag_w, H_SMS_AnalParams_fMinRefHarmMag);
  DEFINE_READER(fRefHarmMagDiffFromMax, g_SMS_AnalParams_fRefHarmMagDiffFromMax_w, H_SMS_AnalParams_fRefHarmMagDiffFromMax);
  DEFINE_READER(fHighestFreq, g_SMS_AnalParams_fHighestFreq_w, H_SMS_AnalParams_fHighestFreq);
  DEFINE_READER(fMinPeakMag, g_SMS_AnalParams_fMinPeakMag_w, H_SMS_AnalParams_fMinPeakMag);
  DEFINE_READER(fResidualPercentage, g_SMS_AnalParams_fResidualPercentage_w, H_SMS_AnalParams_fResidualPercentage);

  XEN_DEFINE_PROCEDURE(S_sms_free_SMS_SynthParams, g_sms_free_SMS_SynthParams_w, 1, 0, 0, H_sms_free_SMS_SynthParams);
  XEN_DEFINE_PROCEDURE(S_sms_make_SMS_SynthParams, g_sms_make_SMS_SynthParams_w, 0, 0, 0, H_sms_make_SMS_SynthParams);
  XEN_DEFINE_PROCEDURE(S_sms_freeSynth, g_sms_freeSynth_w, 1, 0, 0, H_sms_freeSynth);

  DEFINE_READER(iStochasticType, g_SMS_SynthParams_iStochasticType_w, H_SMS_SynthParams_iStochasticType);
  DEFINE_READER(iSynthesisType, g_SMS_SynthParams_iSynthesisType_w, H_SMS_SynthParams_iSynthesisType);
  DEFINE_READER(iDetSynthType, g_SMS_SynthParams_iDetSynthType_w, H_SMS_SynthParams_iDetSynthType);
  DEFINE_READER(iOriginalSRate, g_SMS_SynthParams_iOriginalSRate_w, H_SMS_SynthParams_iOriginalSRate);
  DEFINE_READER(iSamplingRate, g_SMS_SynthParams_iSamplingRate_w, H_SMS_SynthParams_iSamplingRate);
  DEFINE_READER(sizeHop, g_SMS_SynthParams_sizeHop_w, H_SMS_SynthParams_sizeHop);
  DEFINE_READER(origSizeHop, g_SMS_SynthParams_origSizeHop_w, H_SMS_SynthParams_origSizeHop);
  DEFINE_READER(fStocGain, g_SMS_SynthParams_fStocGain_w, H_SMS_SynthParams_fStocGain);
  DEFINE_READER(fTranspose, g_SMS_SynthParams_fTranspose_w, H_SMS_SynthParams_fTranspose);

  XEN_DEFINE_PROCEDURE(S_sms_free_SMS_Header, g_sms_free_SMS_Header_w, 1, 0, 0, H_sms_free_SMS_Header);
  XEN_DEFINE_PROCEDURE(S_sms_make_SMS_Header, g_sms_make_SMS_Header_w, 0, 0, 0, H_sms_make_SMS_Header);
  XEN_DEFINE_PROCEDURE(S_sms_initHeader, g_sms_initHeader_w, 1, 0, 0, H_sms_initHeader);
  XEN_DEFINE_PROCEDURE(S_sms_getHeader, g_sms_getHeader_w, 1, 0, 0, H_sms_getHeader);
  XEN_DEFINE_PROCEDURE(S_sms_writeHeader, g_sms_writeHeader_w, 2, 0, 0, H_sms_writeHeader);
  XEN_DEFINE_PROCEDURE(S_sms_frameSizeB, g_sms_frameSizeB_w, 1, 0, 0, H_sms_frameSizeB);

  DEFINE_READER(iSmsMagic, g_SMS_Header_iSmsMagic_w, H_SMS_Header_iSmsMagic);
  DEFINE_READER(iHeadBSize, g_SMS_Header_iHeadBSize_w, H_SMS_Header_iHeadBSize);
  DEFINE_READER(nFrames, g_SMS_Header_nFrames_w, H_SMS_Header_nFrames);
  DEFINE_READER(iFrameBSize, g_SMS_Header_iFrameBSize_w, H_SMS_Header_iFrameBSize);
  DEFINE_READER(iBegSteadyState, g_SMS_Header_iBegSteadyState_w, H_SMS_Header_iBegSteadyState);
  DEFINE_READER(iEndSteadyState, g_SMS_Header_iEndSteadyState_w, H_SMS_Header_iEndSteadyState);
  DEFINE_READER(nTextCharacters, g_SMS_Header_nTextCharacters_w, H_SMS_Header_nTextCharacters);
  DEFINE_READER(fAmplitude, g_SMS_Header_fAmplitude_w, H_SMS_Header_fAmplitude);
  DEFINE_READER(fFrequency, g_SMS_Header_fFrequency_w, H_SMS_Header_fFrequency);
  DEFINE_READER(fResidualPerc, g_SMS_Header_fResidualPerc_w, H_SMS_Header_fResidualPerc);
  DEFINE_READER(pChTextCharacters, g_SMS_Header_pChTextCharacters_w, H_SMS_Header_pChTextCharacters);

  XEN_DEFINE_PROCEDURE(S_sms_free_SMS_SndHeader, g_sms_free_SMS_SndHeader_w, 1, 0, 0, H_sms_free_SMS_SndHeader);
  XEN_DEFINE_PROCEDURE(S_sms_make_SMS_SndHeader, g_sms_make_SMS_SndHeader_w, 0, 0, 0, H_sms_make_SMS_SndHeader);

  DEFINE_READER(nSamples,           g_SMS_SndHeader_nSamples_w,           H_SMS_SndHeader_nSamples);

  XEN_DEFINE_PROCEDURE(S_sms_initSynth, g_sms_initSynth_w, 2, 0, 0, H_sms_initSynth);
  XEN_DEFINE_PROCEDURE(S_sms_allocFrameH, g_sms_allocFrameH_w, 2, 0, 0, H_sms_allocFrameH);
  XEN_DEFINE_PROCEDURE(S_sms_writeFile, g_sms_writeFile_w, 2, 0, 0, H_sms_writeFile);
  XEN_DEFINE_PROCEDURE(S_sms_openSF, g_sms_openSF_w, 2, 0, 0, H_sms_openSF);
  XEN_DEFINE_PROCEDURE(S_sms_getFrame, g_sms_getFrame_w, 4, 0, 0, H_sms_getFrame);
  XEN_DEFINE_PROCEDURE(S_sms_writeFrame, g_sms_writeFrame_w, 3, 0, 0, H_sms_writeFrame);
  XEN_DEFINE_PROCEDURE(S_sms_getSound, g_sms_getSound_w, 4, 0, 0, H_sms_getSound);
  XEN_DEFINE_PROCEDURE(S_sms_analyze, g_sms_analyze_w, 4, 0, 0, H_sms_analyze);

  XEN_YES_WE_HAVE("sms");
}

#endif
