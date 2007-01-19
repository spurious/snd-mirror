/* Tie sndlib into Xen */

#include <mus-config.h>

#if USE_SND
  #include "snd.h"
#else
  #define PRINT_BUFFER_SIZE 512
  #define LABEL_BUFFER_SIZE 64
  #if HAVE_RUBY
    #define PROC_FALSE "false"
    #define PROC_TRUE "true"
  #endif
  #if HAVE_SCHEME || HAVE_FORTH
    #define PROC_FALSE "#f"
    #define PROC_TRUE "#t"
  #endif
#endif

#include <stddef.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <string.h>
#include <errno.h>

#include "_sndlib.h"
#include "sndlib-strings.h"
#include "sndlib2xen.h"
#include "vct.h"

#ifndef S_setB
  #if HAVE_RUBY
    #define S_setB "set_"
  #endif
  #if HAVE_SCHEME
    #define S_setB "set! "
  #endif
  #if HAVE_FORTH
    #define S_setB "set-"
  #endif
#endif

static char *tmpstr = NULL; /* easier to handle this way than with dynamic winds and so on */
static char *local_mus_expand_filename(char *name)
{
  if (tmpstr) {FREE(tmpstr); tmpstr = NULL;}
  tmpstr = mus_expand_filename(name);
  return(tmpstr);
}

static XEN g_mus_sound_loop_info(XEN gfilename)
{
  #define H_mus_sound_loop_info "(" S_mus_sound_loop_info " filename): synth loop info for sound as a list: (start1 \
end1 start2 end2 base-note base-detune mode1 mode2)"
  int *res;
  XEN sres = XEN_EMPTY_LIST;
  XEN_ASSERT_TYPE(XEN_STRING_P(gfilename), gfilename, XEN_ONLY_ARG, S_mus_sound_loop_info, "a string"); 
  res = mus_sound_loop_info(local_mus_expand_filename(XEN_TO_C_STRING(gfilename)));
  if (res)
    {
      sres = XEN_LIST_8(C_TO_XEN_INT(res[0]), C_TO_XEN_INT(res[1]), C_TO_XEN_INT(res[2]),
			C_TO_XEN_INT(res[3]), C_TO_XEN_INT(res[4]), C_TO_XEN_INT(res[5]),
			C_TO_XEN_INT(res[6]), C_TO_XEN_INT(res[7]));
      FREE(res);
    }
  return(sres);
}

static XEN gmus_sound(const char *caller, int (*func)(const char *file), XEN gfilename)
{
  XEN_ASSERT_TYPE(XEN_STRING_P(gfilename), gfilename, XEN_ONLY_ARG, caller, "a string"); 
  return(C_TO_XEN_INT((*func)(local_mus_expand_filename(XEN_TO_C_STRING(gfilename)))));
}

static XEN gmus_sound_set(const char *caller, int (*func)(const char *file, int newval), XEN gfilename, XEN val)
{
  XEN_ASSERT_TYPE(XEN_STRING_P(gfilename), gfilename, XEN_ARG_1, caller, "a string"); 
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_2, caller, "an integer");
  return(C_TO_XEN_INT((*func)(local_mus_expand_filename(XEN_TO_C_STRING(gfilename)), XEN_TO_C_INT(val))));
}

static XEN glmus_sound(const char *caller, off_t (*func)(const char *file), XEN gfilename)
{
  XEN_ASSERT_TYPE(XEN_STRING_P(gfilename), gfilename, XEN_ONLY_ARG, caller, "a string"); 
  return(C_TO_XEN_OFF_T((*func)(local_mus_expand_filename(XEN_TO_C_STRING(gfilename)))));
}

static XEN glmus_sound_set(const char *caller, int (*func)(const char *file, off_t newval), XEN gfilename, XEN val)
{
  XEN_ASSERT_TYPE(XEN_STRING_P(gfilename), gfilename, XEN_ARG_1, caller, "a string"); 
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, caller, "a number");
  return(C_TO_XEN_OFF_T((*func)(local_mus_expand_filename(XEN_TO_C_STRING(gfilename)), XEN_TO_C_OFF_T(val))));
}

static XEN g_mus_sound_samples(XEN filename) 
{
  #define H_mus_sound_samples "(" S_mus_sound_samples " filename): samples (frames * channels) in sound file"
  return(glmus_sound(S_mus_sound_samples, mus_sound_samples, filename));
}

static XEN g_mus_sound_set_samples(XEN filename, XEN val) 
{
  return(glmus_sound_set(S_setB S_mus_sound_samples, mus_sound_set_samples, filename, val));
}

static XEN g_mus_sound_frames(XEN filename) 
{
  #define H_mus_sound_frames "(" S_mus_sound_frames " filename): frames (samples / channel) in sound file"
  return(glmus_sound(S_mus_sound_frames, mus_sound_frames, filename));
}

static XEN g_mus_sound_datum_size(XEN filename) 
{
  #define H_mus_sound_datum_size "(" S_mus_sound_datum_size " filename): bytes per sample used by the data in sound file (data format dependent)"
  return(gmus_sound(S_mus_sound_datum_size, mus_sound_datum_size, filename));
}

static XEN g_mus_sound_data_location(XEN filename) 
{
  #define H_mus_sound_data_location "(" S_mus_sound_data_location " filename): location (in bytes) of first sample of sound data"
  return(glmus_sound(S_mus_sound_data_location, mus_sound_data_location, filename));
}

static XEN g_mus_sound_set_data_location(XEN filename, XEN val) 
{
  return(glmus_sound_set(S_setB S_mus_sound_data_location, mus_sound_set_data_location, filename, val));
}

static XEN g_mus_sound_chans(XEN filename) 
{
  #define H_mus_sound_chans "(" S_mus_sound_chans " filename): channels of data in sound file"
  return(gmus_sound(S_mus_sound_chans, mus_sound_chans, filename));
}

static XEN g_mus_sound_set_chans(XEN filename, XEN val) 
{
  return(gmus_sound_set(S_setB S_mus_sound_chans, mus_sound_set_chans, filename, val));
}

static XEN g_mus_sound_srate(XEN filename) 
{
  #define H_mus_sound_srate "(" S_mus_sound_srate " filename): sampling rate of sound file"
  return(gmus_sound(S_mus_sound_srate, mus_sound_srate, filename));
}

static XEN g_mus_sound_set_srate(XEN filename, XEN val) 
{
  return(gmus_sound_set(S_setB S_mus_sound_srate, mus_sound_set_srate, filename, val));
}

static XEN g_mus_sound_header_type(XEN filename) 
{
  #define H_mus_sound_header_type "(" S_mus_sound_header_type " filename): header type (e.g. " S_mus_aifc ") of sound file"
  return(gmus_sound(S_mus_sound_header_type, mus_sound_header_type, filename));
}

static XEN g_mus_sound_set_header_type(XEN filename, XEN val) 
{
  return(gmus_sound_set(S_setB S_mus_sound_header_type, mus_sound_set_header_type, filename, val));
}

static XEN g_mus_sound_data_format(XEN filename) 
{
  #define H_mus_sound_data_format "(" S_mus_sound_data_format " filename): data format (e.g. " S_mus_bshort ") of data in sound file"
  return(gmus_sound(S_mus_sound_data_format, mus_sound_data_format, filename));
}

static XEN g_mus_sound_set_data_format(XEN filename, XEN val) 
{
  return(gmus_sound_set(S_setB S_mus_sound_data_format, mus_sound_set_data_format, filename, val));
}

static XEN g_mus_sound_length(XEN filename) 
{
  #define H_mus_sound_length "(" S_mus_sound_length " filename): sound file length in bytes"
  return(glmus_sound(S_mus_sound_length, mus_sound_length, filename));
}

static XEN g_mus_sound_type_specifier(XEN filename) 
{
  #define H_mus_sound_type_specifier "(" S_mus_sound_type_specifier " filename): original sound file header type identifier (e.g. 0x2e736e64)"
  return(gmus_sound(S_mus_sound_type_specifier, mus_sound_type_specifier, filename));
}

static XEN g_mus_sound_forget(XEN filename) 
{
  #define H_mus_sound_forget "(" S_mus_sound_forget " filename): remove 'filename' from sound cache.  If you create, then later \
delete a sound file, " S_mus_sound_forget " can be used to clear it from sndlib's cache of sound files"
  return(gmus_sound(S_mus_sound_forget, mus_sound_forget, filename));
}

static XEN g_mus_sound_prune(void) 
{
  #define H_mus_sound_prune "(" S_mus_sound_prune "): remove all defunct entries from sndlib's sound file cache."
  return(C_TO_XEN_INT(mus_sound_prune()));
}

static XEN g_mus_sound_comment(XEN gfilename) 
{
  #define H_mus_sound_comment "(" S_mus_sound_comment " filename): comment (a string) found in sound file's header"
  char *res = NULL; 
  XEN newstr;
  XEN_ASSERT_TYPE(XEN_STRING_P(gfilename), gfilename, XEN_ONLY_ARG, S_mus_sound_comment, "a string"); 
  res = mus_sound_comment(local_mus_expand_filename(XEN_TO_C_STRING(gfilename)));
  newstr = C_TO_XEN_STRING(res);
  if (res) FREE(res);
  return(newstr);
}

static XEN g_mus_sound_write_date(XEN filename) 
{
  #define H_mus_sound_write_date "(" S_mus_sound_write_date " filename): write date of sound file"
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ONLY_ARG, S_mus_sound_write_date, "a string"); 
  return(C_TO_XEN_ULONG((unsigned long)mus_sound_write_date(local_mus_expand_filename(XEN_TO_C_STRING(filename)))));
}

static XEN g_mus_header_raw_defaults(void)
{
  #define H_mus_header_raw_defaults "(" S_mus_header_raw_defaults "): returns list '(srate chans format) of current raw sound default attributes"
  int srate, chans, data_format;
  mus_header_raw_defaults(&srate, &chans, &data_format);
  return(XEN_LIST_3(C_TO_XEN_INT(srate),
		    C_TO_XEN_INT(chans),
		    C_TO_XEN_INT(data_format)));
}

static XEN g_mus_header_set_raw_defaults(XEN lst)
{
  XEN_ASSERT_TYPE((XEN_LIST_P(lst)) && (XEN_LIST_LENGTH(lst) == 3), lst, XEN_ONLY_ARG, S_mus_header_raw_defaults, "a list: '(srate chans data-format)");
  XEN_ASSERT_TYPE(XEN_CAR(lst), XEN_CAR(lst), XEN_ARG_1, S_mus_header_raw_defaults, "an integer = srate");
  XEN_ASSERT_TYPE(XEN_CADR(lst), XEN_CADR(lst), XEN_ARG_2, S_mus_header_raw_defaults, "an integer = chans");
  XEN_ASSERT_TYPE(XEN_CADDR(lst), XEN_CADDR(lst), XEN_ARG_3, S_mus_header_raw_defaults, "an integer = data-format");
  mus_header_set_raw_defaults(XEN_TO_C_INT(XEN_CAR(lst)),
			      XEN_TO_C_INT(XEN_CADR(lst)),
			      XEN_TO_C_INT(XEN_CADDR(lst)));
  return(lst);
}

static XEN g_mus_header_type_name(XEN type) 
{
  #define H_mus_header_type_name "(" S_mus_header_type_name " type): header type (e.g. " S_mus_aiff ") as a string"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(type), type, XEN_ONLY_ARG, S_mus_header_type_name, "an integer (header-type id)"); 
  return(C_TO_XEN_STRING(mus_header_type_name(XEN_TO_C_INT(type))));
}

static XEN g_mus_header_type_to_string(XEN type) 
{
  #define H_mus_header_type_to_string "(" S_mus_header_type_to_string " type): header type (e.g. " S_mus_aiff ") as a string"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(type), type, XEN_ONLY_ARG, S_mus_header_type_to_string, "an integer (header-type id)"); 
  return(C_TO_XEN_STRING(mus_header_type_to_string(XEN_TO_C_INT(type))));
}

static XEN g_mus_data_format_name(XEN format) 
{
  #define H_mus_data_format_name "(" S_mus_data_format_name " format): data format (e.g. " S_mus_bshort ") as a string"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(format), format, XEN_ONLY_ARG, S_mus_data_format_name, "an integer (data-format id)"); 
  return(C_TO_XEN_STRING(mus_data_format_name(XEN_TO_C_INT(format))));
}

static XEN g_mus_data_format_to_string(XEN format) 
{
  #define H_mus_data_format_to_string "(" S_mus_data_format_to_string " format): data format (e.g. " S_mus_bshort ") as a string"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(format), format, XEN_ONLY_ARG, S_mus_data_format_to_string, "an integer (data-format id)"); 
  return(C_TO_XEN_STRING(mus_data_format_to_string(XEN_TO_C_INT(format))));
}

static XEN g_mus_bytes_per_sample(XEN format) 
{
  #define H_mus_bytes_per_sample "(" S_mus_bytes_per_sample " format): number of bytes per sample in \
format (e.g. " S_mus_bshort " = 2)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(format), format, XEN_ONLY_ARG, S_mus_bytes_per_sample, "an integer (data-format id)"); 
  return(C_TO_XEN_INT(mus_bytes_per_sample(XEN_TO_C_INT(format))));
}

static XEN g_mus_audio_report(void) 
{
  #define H_mus_audio_report "(" S_mus_audio_report "): string describing current audio hardware setup"
  return(C_TO_XEN_STRING(mus_audio_report()));
}

static XEN g_mus_sound_duration(XEN gfilename) 
{
  #define H_mus_sound_duration "(" S_mus_sound_duration " filename): duration (in seconds) of sound file"
  float res;
  XEN_ASSERT_TYPE(XEN_STRING_P(gfilename), gfilename, XEN_ONLY_ARG, S_mus_sound_duration, "a string"); 
  res = mus_sound_duration(local_mus_expand_filename(XEN_TO_C_STRING(gfilename)));
  return(C_TO_XEN_DOUBLE(res));

}

static XEN g_mus_sun_set_outputs(XEN speakers, XEN headphones, XEN line_out)
{
  #define H_mus_sun_set_outputs "(" S_mus_sun_set_outputs " speaker headphones line-out): set the current Sun audio outputs. \
Each entry should be either 0 (turn off device) or 1 (turn it on)."
  XEN_ASSERT_TYPE(XEN_INTEGER_P(speakers), speakers, XEN_ARG_1, S_mus_sun_set_outputs, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(headphones), headphones, XEN_ARG_2, S_mus_sun_set_outputs, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(line_out), line_out, XEN_ARG_3, S_mus_sun_set_outputs, "an integer");
#ifdef MUS_SUN
  mus_sun_set_outputs(XEN_TO_C_INT(speakers),
		      XEN_TO_C_INT(headphones),
		      XEN_TO_C_INT(line_out));
#endif
  return(XEN_FALSE);
}

static XEN g_mus_netbsd_set_outputs(XEN speakers, XEN headphones, XEN line_out)
{
  #define H_mus_netbsd_set_outputs "(" S_mus_netbsd_set_outputs " speaker headphones line-out): set the current netBSD audio outputs. \
Each entry should be either 0 (turn off device) or 1 (turn it on)."
  XEN_ASSERT_TYPE(XEN_INTEGER_P(speakers), speakers, XEN_ARG_1, S_mus_netbsd_set_outputs, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(headphones), headphones, XEN_ARG_2, S_mus_netbsd_set_outputs, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(line_out), line_out, XEN_ARG_3, S_mus_netbsd_set_outputs, "an integer");
#ifdef MUS_NETBSD
  mus_netbsd_set_outputs(XEN_TO_C_INT(speakers),
			 XEN_TO_C_INT(headphones),
			 XEN_TO_C_INT(line_out));
#endif
  return(XEN_FALSE);
}

static XEN g_mus_oss_set_buffers(XEN num, XEN size)
{
  #define H_mus_oss_set_buffers "(" S_mus_oss_set_buffers " num size): set Linux OSS 'fragment' number and size. \
If Snd's controls seem sluggish, try (" S_mus_oss_set_buffers " 4 12) or even (" S_mus_oss_set_buffers " 2 12). \
This reduces the on-card buffering, but may introduce clicks."

#if (HAVE_OSS || HAVE_ALSA)
  XEN_ASSERT_TYPE(XEN_INTEGER_P(num), num, XEN_ARG_1, S_mus_oss_set_buffers, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(size), size, XEN_ARG_2, S_mus_oss_set_buffers, "an integer");
  mus_oss_set_buffers(XEN_TO_C_INT(num),
			    XEN_TO_C_INT(size));
#endif
  return(XEN_FALSE);
}

static XEN g_mus_alsa_buffers(void)
{
  #define H_mus_alsa_buffers "(" S_mus_alsa_buffers ") -> current number of ALSA periods."
#if HAVE_ALSA
  return(C_TO_XEN_INT(mus_alsa_buffers()));
#endif
  return(XEN_FALSE);
}

static XEN g_mus_alsa_set_buffers(XEN val)
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_mus_alsa_buffers, "an integer");
#if HAVE_ALSA
  return(C_TO_XEN_INT(mus_alsa_set_buffers(XEN_TO_C_INT(val))));
#endif
  return(XEN_FALSE);
}

static XEN g_mus_alsa_buffer_size(void)
{
  #define H_mus_alsa_buffer_size "(" S_mus_alsa_buffer_size ") -> current size of ALSA buffers."
#if HAVE_ALSA
  return(C_TO_XEN_INT(mus_alsa_buffer_size()));
#endif
  return(XEN_FALSE);
}

static XEN g_mus_alsa_set_buffer_size(XEN val)
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_mus_alsa_buffer_size, "an integer");
#if HAVE_ALSA
  return(C_TO_XEN_INT(mus_alsa_set_buffer_size(XEN_TO_C_INT(val))));
#endif
  return(XEN_FALSE);
}

static XEN g_mus_alsa_device(void)
{
  #define H_mus_alsa_device "(" S_mus_alsa_device ") -> current ALSA device."
#if HAVE_ALSA
  return(C_TO_XEN_STRING(mus_alsa_device()));
#endif
  return(XEN_FALSE);
}

static XEN g_mus_alsa_set_device(XEN val)
{
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_mus_alsa_device, "a string (ALSA device name)");
#if HAVE_ALSA
  return(C_TO_XEN_STRING(mus_alsa_set_device(XEN_TO_C_STRING(val))));
#endif
  return(XEN_FALSE);
}

static XEN g_mus_alsa_playback_device(void)
{
  #define H_mus_alsa_playback_device "(" S_mus_alsa_playback_device ") -> current ALSA playback device."
#if HAVE_ALSA
  return(C_TO_XEN_STRING(mus_alsa_playback_device()));
#endif
  return(XEN_FALSE);
}

static XEN g_mus_alsa_set_playback_device(XEN val)
{
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_mus_alsa_playback_device, "a string (ALSA device name)");
#if HAVE_ALSA
  return(C_TO_XEN_STRING(mus_alsa_set_playback_device(XEN_TO_C_STRING(val))));
#endif
  return(XEN_FALSE);
}

static XEN g_mus_alsa_capture_device(void)
{
  #define H_mus_alsa_capture_device "(" S_mus_alsa_capture_device ") -> current ALSA capture device."
#if HAVE_ALSA
  return(C_TO_XEN_STRING(mus_alsa_capture_device()));
#endif
  return(XEN_FALSE);
}

static XEN g_mus_alsa_set_capture_device(XEN val)
{
  XEN_ASSERT_TYPE(XEN_STRING_P(val), val, XEN_ONLY_ARG, S_setB S_mus_alsa_capture_device, "a string (ALSA device name)");
#if HAVE_ALSA
  return(C_TO_XEN_STRING(mus_alsa_set_capture_device(XEN_TO_C_STRING(val))));
#endif
  return(XEN_FALSE);
}

static XEN g_mus_alsa_squelch_warning(void)
{
  #define H_mus_alsa_squelch_warning "(" S_mus_alsa_squelch_warning ") -> whether to squelch ALSA srate mismatch warnings."
#if HAVE_ALSA
  return(C_TO_XEN_BOOLEAN(mus_alsa_squelch_warning()));
#endif
  return(XEN_FALSE);
}

static XEN g_mus_alsa_set_squelch_warning(XEN val)
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_mus_alsa_squelch_warning, "a boolean");
#if HAVE_ALSA
  return(C_TO_XEN_BOOLEAN(mus_alsa_set_squelch_warning(XEN_TO_C_BOOLEAN(val))));
#endif
  return(XEN_FALSE);
}


static XEN g_mus_sound_maxamp_exists(XEN file)
{
  #define H_mus_sound_maxamp_exists "(" S_mus_sound_maxamp_exists " filename): " PROC_TRUE " if sound's maxamp data is available \
in the sound cache; if it isn't, a call on " S_mus_sound_maxamp " has to open and read the data to get the maxamp."
  bool val;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ONLY_ARG, S_mus_sound_maxamp_exists, "a string");
  val = mus_sound_maxamp_exists(local_mus_expand_filename(XEN_TO_C_STRING(file)));
  return(C_TO_XEN_BOOLEAN(val));
}

static XEN g_mus_sound_maxamp(XEN file)
{
  #define H_mus_sound_maxamp "(" S_mus_sound_maxamp " filename): maxamps in sound (a list of paired amps (floats) and locations (samples))"
  int chans;
  char *filename;
  XEN res = XEN_EMPTY_LIST;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ONLY_ARG, S_mus_sound_maxamp, "a string");
  filename = local_mus_expand_filename(XEN_TO_C_STRING(file));
  chans = mus_sound_chans(filename);
  if (chans > 0)
    {
      off_t rtn;
      int i;
      mus_sample_t *vals;
      off_t *times;
      vals = (mus_sample_t *)CALLOC(chans, sizeof(mus_sample_t));
      times = (off_t *)CALLOC(chans, sizeof(off_t));
      rtn = mus_sound_maxamps(filename, chans, vals, times);
      if (rtn != MUS_ERROR)
	for (i = chans - 1; i >= 0; i--)
	  res = XEN_CONS(C_TO_XEN_OFF_T(times[i]),
		  XEN_CONS(C_TO_XEN_DOUBLE(MUS_SAMPLE_TO_FLOAT(vals[i])), res));
      FREE(vals);
      FREE(times);
    }
  else XEN_ERROR(BAD_HEADER,
		 XEN_LIST_2(C_TO_XEN_STRING(S_mus_sound_maxamp),
			    C_TO_XEN_STRING("chans <= 0")));
  return(res);
}

static XEN g_mus_sound_set_maxamp(XEN file, XEN vals)
{
  int chans;
  char *filename;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_setB S_mus_sound_maxamp, "a string");
  XEN_ASSERT_TYPE(XEN_LIST_P(vals), vals, XEN_ARG_2, S_setB S_mus_sound_maxamp, "a list");
  filename = local_mus_expand_filename(XEN_TO_C_STRING(file));
  chans = mus_sound_chans(filename);
  /* presumably any actual error here will be trapped via mus-error (raised in mus_header_read via read_sound_file_header),
   *   so chans <= 0 is actually in the header?
   */
  if (chans > 0)
    {
      XEN lst;
      int i, j, len;
      mus_sample_t *mvals;
      off_t *times;
      len = XEN_LIST_LENGTH(vals);
      if (len < (chans * 2))
	XEN_WRONG_TYPE_ARG_ERROR(S_setB S_mus_sound_maxamp, 2, vals, "max amp list length must = 2 * chans");
      if (len > chans * 2) len = chans * 2;
      mvals = (mus_sample_t *)CALLOC(chans, sizeof(mus_sample_t));
      times = (off_t *)CALLOC(chans, sizeof(off_t));
      for (i = 0, j = 0, lst = XEN_COPY_ARG(vals); i < len; i += 2, j++, lst = XEN_CDDR(lst))
	{
	  times[j] = XEN_TO_C_OFF_T_OR_ELSE(XEN_CAR(lst), 0);
	  mvals[j] = MUS_DOUBLE_TO_SAMPLE(XEN_TO_C_DOUBLE(XEN_CADR(lst)));
	}
      mus_sound_set_maxamps(filename, chans, mvals, times);
      FREE(mvals);
      FREE(times);
    }
  else XEN_ERROR(BAD_HEADER,
		 XEN_LIST_2(C_TO_XEN_STRING(S_setB S_mus_sound_maxamp),
			    C_TO_XEN_STRING("chans <= 0")));
  return(vals);
}

static XEN g_mus_sound_open_input(XEN file)
{
  #define H_mus_sound_open_input "(" S_mus_sound_open_input " filename): open filename for (low-level) sound input, \
return file descriptor (an integer)"
  int fd;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ONLY_ARG, S_mus_sound_open_input, "a string");
  fd = mus_sound_open_input(local_mus_expand_filename(XEN_TO_C_STRING(file)));
  return(C_TO_XEN_INT(fd));
}

static XEN g_mus_sound_open_output(XEN file, XEN srate, XEN chans, XEN data_format, XEN header_type, XEN comment)
{

  #define H_mus_sound_open_output "(" S_mus_sound_open_output " filename :optional (srate 22050) (chans 1) data-format header-type (comment \"\")): \
open filename for (low-level) sound output with the given srate and so on; return the file descriptor (an integer). \
The file size is normally set later via " S_mus_sound_close_output ". srate is an integer, comment is a string, \
data-format is a sndlib format indicator such as " S_mus_bshort ", if " PROC_FALSE " if defaults to a format compatible with sndlib, \
header-type is a sndlib type indicator such as " S_mus_aiff "; sndlib currently only writes 5 or so header types."

  int fd = -1, df;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_mus_sound_open_output, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(srate), srate, XEN_ARG_2, S_mus_sound_open_output, "an integer or " PROC_FALSE);
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(chans), chans, XEN_ARG_3, S_mus_sound_open_output, "an integer or " PROC_FALSE);
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(data_format), data_format, XEN_ARG_4, S_mus_sound_open_output, "a data-format or " PROC_FALSE);
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(header_type), header_type, XEN_ARG_5, S_mus_sound_open_output, "a header-type or " PROC_FALSE);
  XEN_ASSERT_TYPE((XEN_STRING_P(comment) || (XEN_NOT_BOUND_P(comment))), comment, XEN_ARG_6, S_mus_sound_open_output, "a string");

  df = XEN_TO_C_INT_OR_ELSE(data_format, MUS_OUT_FORMAT);
  if (MUS_DATA_FORMAT_OK(df))
    {
      int ht;
#if MUS_LITTLE_ENDIAN
      ht = XEN_TO_C_INT_OR_ELSE(header_type, MUS_RIFF);
#else
      ht = XEN_TO_C_INT_OR_ELSE(header_type, MUS_NEXT);
#endif
      if (MUS_HEADER_TYPE_OK(ht))
	{
	  int chns;
	  chns = XEN_TO_C_INT_OR_ELSE(chans, 1);
	  if (chns > 0)
	    {
	      char *com = NULL;
	      if (XEN_STRING_P(comment)) com = XEN_TO_C_STRING(comment);
	      fd = mus_sound_open_output(local_mus_expand_filename(XEN_TO_C_STRING(file)),
					 XEN_TO_C_INT_OR_ELSE(srate, 22050),
					 chns, df, ht, com);
	    }
	  else XEN_OUT_OF_RANGE_ERROR(S_mus_sound_open_output, 3, chans, "chans ~A <= 0?");
	}
      else XEN_OUT_OF_RANGE_ERROR(S_mus_sound_open_output, 5, header_type, "~A: invalid header type");
    }
  else XEN_OUT_OF_RANGE_ERROR(S_mus_sound_open_output, 4, data_format, "~A: invalid data format");
  return(xen_return_first(C_TO_XEN_INT(fd), comment));
}

static XEN g_mus_sound_reopen_output(XEN file, XEN chans, XEN data_format, XEN header_type, XEN data_loc)
{

  #define H_mus_sound_reopen_output "(" S_mus_sound_reopen_output " filename :optional (chans 1) data-format header-type data-location): \
reopen (without alteration) filename for output \
data-format and header-type are sndlib indicators such as " S_mus_bshort " or " S_mus_aiff ". \
data-location should be retrieved from a previous call to " S_mus_sound_data_location "."

  int fd = -1, df;
  char *filename;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_mus_sound_reopen_output, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(chans), chans, XEN_ARG_2, S_mus_sound_reopen_output, "an integer or " PROC_FALSE);
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(data_format), data_format, XEN_ARG_3, S_mus_sound_reopen_output, "a data-format or " PROC_FALSE);
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(header_type), header_type, XEN_ARG_4, S_mus_sound_reopen_output, "a header-type or " PROC_FALSE);
  XEN_ASSERT_TYPE(XEN_OFF_T_P(data_loc) || XEN_FALSE_P(data_loc) || XEN_NOT_BOUND_P(data_loc), 
		  data_loc, XEN_ARG_5, S_mus_sound_reopen_output, "an integer or " PROC_FALSE);

  filename = local_mus_expand_filename(XEN_TO_C_STRING(file));

  /* use existing settings if unbound as args */
  if (XEN_INTEGER_P(data_format))
    df = XEN_TO_C_INT(data_format);
  else
    {
      df = mus_sound_data_format(filename);
      if (df == MUS_ERROR)
	df = MUS_OUT_FORMAT;
    }
  if (MUS_DATA_FORMAT_OK(df))
    {
      int ht;
      if (XEN_INTEGER_P(header_type))
	ht = XEN_TO_C_INT(header_type);
      else
	{
	  ht = mus_sound_header_type(filename);
	  if (ht == MUS_ERROR)
#if MUS_LITTLE_ENDIAN
	    ht = MUS_RIFF;
#else
	    ht = MUS_NEXT;
#endif
	}
      if (MUS_HEADER_TYPE_OK(ht))
	{
	  int chns;
	  if (XEN_INTEGER_P(chans))
	    chns = XEN_TO_C_INT(chans);
	  else
	    {
	      chns = mus_sound_chans(filename);
	      if (chns == MUS_ERROR)
		chns = 1;
	    }
	  if (chns > 0)
	    {
	      off_t dloc;
	      if (XEN_OFF_T_P(data_loc))
		dloc = XEN_TO_C_OFF_T(data_loc);
	      else
		{
		  dloc = mus_sound_data_location(filename);
		  if (dloc == MUS_ERROR)
		    dloc = 0;
		}
	      fd = mus_sound_reopen_output(filename, chns, df, ht, dloc);
	    }
	  else XEN_OUT_OF_RANGE_ERROR(S_mus_sound_reopen_output, 2, chans, "chans <= 0?");
	}
      else XEN_OUT_OF_RANGE_ERROR(S_mus_sound_reopen_output, 4, header_type, "~A: invalid header type");
    }
  else XEN_OUT_OF_RANGE_ERROR(S_mus_sound_reopen_output, 3, data_format, "~A: invalid data format");
  return(C_TO_XEN_INT(fd));
}

static XEN g_mus_sound_close_input(XEN fd)
{
  #define H_mus_sound_close_input "(" S_mus_sound_close_input " fd): close (low-level) file fd that was opened \
by " S_mus_sound_open_input "."
  int nfd;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ONLY_ARG, S_mus_sound_close_input, "an integer");
  nfd = XEN_TO_C_INT(fd);
  if ((nfd < 0) || (nfd == fileno(stdin)) || (nfd == fileno(stdout)) || (nfd == fileno(stderr)))
    XEN_OUT_OF_RANGE_ERROR(S_mus_sound_close_input, 1, fd, "~A: invalid file number");
  return(C_TO_XEN_INT(mus_sound_close_input(XEN_TO_C_INT(fd))));
}

static XEN g_mus_sound_close_output(XEN fd, XEN bytes)
{
  #define H_mus_sound_close_output "(" S_mus_sound_close_output " fd bytes): close (low-level) file fd \
that was opened by " S_mus_sound_open_output " after updating its header (if any) to reflect bytes, the new file data size"

  int nfd;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ARG_1, S_mus_sound_close_output, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(bytes), bytes, XEN_ARG_2, S_mus_sound_close_output, "a number");
  nfd = XEN_TO_C_INT(fd);
  if ((nfd < 0) || (nfd == fileno(stdin)) || (nfd == fileno(stdout)) || (nfd == fileno(stderr)))
    XEN_OUT_OF_RANGE_ERROR(S_mus_sound_close_output, 1, fd, "~A: invalid file number");
  return(C_TO_XEN_INT(mus_sound_close_output(XEN_TO_C_INT(fd),
					     XEN_TO_C_OFF_T_OR_ELSE(bytes, 0))));
}

static XEN g_mus_sound_read(XEN fd, XEN beg, XEN end, XEN chans, XEN sv)
{
  #define H_mus_sound_read "(" S_mus_sound_read " fd beg end chans sdata): read sound data from file fd, \
filling sound-data sdata's buffers starting at beg (buffer location), going to end"

  sound_data *sd;
  int bg, nd;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ARG_1, S_mus_sound_read, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(beg), beg, XEN_ARG_2, S_mus_sound_read, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(end), end, XEN_ARG_3, S_mus_sound_read, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chans), chans, XEN_ARG_4, S_mus_sound_read, "an integer");
  XEN_ASSERT_TYPE(sound_data_p(sv), sv, XEN_ARG_5, S_mus_sound_read, "a sound-data object");
  sd = (sound_data *)(XEN_OBJECT_REF(sv));
  bg = XEN_TO_C_INT_OR_ELSE(beg, 0);
  nd = XEN_TO_C_INT_OR_ELSE(end, 0);
  if ((nd - bg) >= sd->length)
    XEN_ERROR(XEN_ERROR_TYPE("out-of-range"),
	      XEN_LIST_3(C_TO_XEN_STRING(S_mus_sound_read),
			 C_TO_XEN_STRING("end (~A) - beg (~A) >= sound-data array length (~A)"),
			 XEN_LIST_3(end, beg, C_TO_XEN_INT(sd->length))));
#if SNDLIB_USE_FLOATS
  return(C_TO_XEN_INT(mus_file_read(XEN_TO_C_INT(fd), bg, nd, XEN_TO_C_INT(chans), sd->data)));
#else
  {
    mus_sample_t **sdata;
    int i, j, result;
    sdata = (mus_sample_t **)CALLOC(sd->chans, sizeof(mus_sample_t *));
    for (i = 0; i < sd->chans; i++) sdata[i] = (mus_sample_t *)CALLOC(sd->length, sizeof(mus_sample_t));
    result = mus_file_read(XEN_TO_C_INT(fd), bg, nd, XEN_TO_C_INT(chans), sdata);
    for (i = 0; i < sd->chans; i++)
      for (j = 0; j < sd->length; j++)
	sd->data[i][j] = MUS_SAMPLE_TO_DOUBLE(sdata[i][j]);
    for (i = 0; i < sd->chans; i++)
      FREE(sdata[i]);
    FREE(sdata);
    return(C_TO_XEN_INT(result));
  }
#endif
}

static XEN g_mus_sound_write(XEN fd, XEN beg, XEN end, XEN chans, XEN sv)
{
  #define H_mus_sound_write "(" S_mus_sound_write " fd beg end chans sdata): write sound-data sdata's data to file fd, \
starting at beg (buffer location), going to end"

  sound_data *sd;
  int bg, nd;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ARG_1, S_mus_sound_write, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(beg), beg, XEN_ARG_2, S_mus_sound_write, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(end), end, XEN_ARG_3, S_mus_sound_write, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chans), chans, XEN_ARG_4, S_mus_sound_write, "an integer");
  XEN_ASSERT_TYPE(sound_data_p(sv), sv, XEN_ARG_5, S_mus_sound_write, "a sound-data object");
  /* even here we can write memory that doesn't belong to us if clipping */
  sd = (sound_data *)(XEN_OBJECT_REF(sv));
  bg = XEN_TO_C_INT_OR_ELSE(beg, 0);
  nd = XEN_TO_C_INT_OR_ELSE(end, 0);
  if ((nd - bg) >= sd->length)
    XEN_ERROR(XEN_ERROR_TYPE("out-of-range"),
	      XEN_LIST_3(C_TO_XEN_STRING(S_mus_sound_write),
			 C_TO_XEN_STRING("end (~A) - beg (~A) >= sound-data array length (~A)"),
			 XEN_LIST_3(end, beg, C_TO_XEN_INT(sd->length))));
#if SNDLIB_USE_FLOATS
  return(C_TO_XEN_INT(mus_file_write(XEN_TO_C_INT(fd), bg, nd, XEN_TO_C_INT(chans), sd->data)));
#else
  {
    mus_sample_t **sdata;
    int i, j, result;
    sdata = (mus_sample_t **)CALLOC(sd->chans, sizeof(mus_sample_t *));
    for (i = 0; i < sd->chans; i++) sdata[i] = (mus_sample_t *)CALLOC(sd->length, sizeof(mus_sample_t));
    for (i = 0; i < sd->chans; i++)
      for (j = 0; j < sd->length; j++)
	sdata[i][j] = MUS_DOUBLE_TO_SAMPLE(sd->data[i][j]);
    result = mus_file_write(XEN_TO_C_INT(fd), bg, nd, XEN_TO_C_INT(chans), sdata);
    for (i = 0; i < sd->chans; i++)
      FREE(sdata[i]);
    FREE(sdata);
    return(C_TO_XEN_INT(result));
  }
#endif
}

static XEN g_mus_sound_seek_frame(XEN fd, XEN offset)
{
  #define H_mus_sound_seek_frame "(" S_mus_sound_seek_frame " fd frame): move the current read/write location in file fd \
to the frame offset"

  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ARG_1, S_mus_sound_seek_frame, "an integer");
  XEN_ASSERT_TYPE(XEN_OFF_T_P(offset), offset, XEN_ARG_2, S_mus_sound_seek_frame, "an integer");
  return(C_TO_XEN_OFF_T(mus_file_seek_frame(XEN_TO_C_INT(fd),
					    XEN_TO_C_OFF_T(offset))));
}

/* since mus-audio-read|write assume sound-data vectors communicating with Scheme,
 *   we can't assume that the user has had a chance to deal with type problems.
 *   So, we keep track of each audio line's current read|write format and
 *   translate in mus_audio_read|write if necessary
 */

static int audio_io_size = 0;
static int *audio_io_lines = NULL;
static int *audio_io_formats = NULL;

static int audio_io_format(int line)
{
  int i;
  for (i = 0; i < audio_io_size; i++)
    if (audio_io_lines[i] == line)
      return(audio_io_formats[i]);
  return(MUS_UNKNOWN);
}

#define audio_io_read_format(Line) (audio_io_format(Line) & 0xffff)
#define audio_io_write_format(Line) (audio_io_format(Line) >> 16)

static void audio_io_set_format(int line, int format)
{
  int i, old_size;
  for (i = 0; i < audio_io_size; i++)
    if (audio_io_lines[i] == line)
      {
	audio_io_formats[i] = format;
	return;
      }
  /* we get here if the table is not big enough */
  old_size = audio_io_size;
  audio_io_size += 8;
  if (old_size == 0)
    {
      audio_io_lines = (int *)MALLOC(audio_io_size * sizeof(int));
      audio_io_formats = (int *)MALLOC(audio_io_size * sizeof(int));
    }
  else
    {
      audio_io_lines = (int *)REALLOC(audio_io_lines, audio_io_size * sizeof(int));
      audio_io_formats = (int *)REALLOC(audio_io_formats, audio_io_size * sizeof(int));
    }
  for (i = old_size + 1; i < audio_io_size; i++)
    {
      audio_io_lines[i] = -1;
      audio_io_formats[i] = MUS_UNKNOWN;
    }
  audio_io_lines[old_size] = line;
  audio_io_formats[old_size] = format;
}

#define audio_io_set_read_format(Line, Format) audio_io_set_format(Line, ((audio_io_format(Line) & 0xffff0000) | Format))
#define audio_io_set_write_format(Line, Format) audio_io_set_format(Line, ((audio_io_format(Line) & 0xffff) | (Format << 16)))

static XEN g_mus_audio_open_output(XEN dev, XEN srate, XEN chans, XEN format, XEN size)
{
  #if HAVE_SCHEME
    #define audio_open_example "(" S_mus_audio_open_output " " S_mus_audio_default " 22050 1 " S_mus_lshort " 256)"
  #endif
  #if HAVE_RUBY
    #define audio_open_example "mus_audio_open_output(Mus_audio_default, 22050, 1, Mus_lshort, 256)"
  #endif
  #if HAVE_FORTH
    #define audio_open_example "mus-audio-default 22050 1 mus-lshort 256 mus-audio-open-output"
  #endif

  #define H_mus_audio_open_output "(" S_mus_audio_open_output " device srate chans format bytes): \
open the audio device ready for output at the given srate and so on; \
return the audio line number:\n  " audio_open_example

  int line, idev, ifmt, isize, israte, ichans;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(dev), dev, XEN_ARG_1, S_mus_audio_open_output, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(srate), srate, XEN_ARG_2, S_mus_audio_open_output, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chans), chans, XEN_ARG_3, S_mus_audio_open_output, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(format), format, XEN_ARG_4, S_mus_audio_open_output, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(size), size, XEN_ARG_5, S_mus_audio_open_output, "a number");
  idev = XEN_TO_C_INT(dev);
  israte = XEN_TO_C_INT_OR_ELSE(srate, 0);
  ichans = XEN_TO_C_INT(chans);
  ifmt = XEN_TO_C_INT(format);
  isize = XEN_TO_C_INT_OR_ELSE(size, 0);
  if (!(MUS_AUDIO_DEVICE_OK(MUS_AUDIO_DEVICE(idev))))
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_open_output, 1, dev, "~A: invalid device");
  if (!(MUS_DATA_FORMAT_OK(ifmt)))
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_open_output, 4, format, "~A: invalid data format");
  if (isize < 0)
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_open_output, 5, size, "size ~A < 0?");
  if (israte <= 0)
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_open_output, 2, srate, "srate ~A <= 0?");
  if ((ichans <= 0) || (ichans > 256))
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_open_output, 3, chans, "chans ~A <= 0 or > 256?");
  line = mus_audio_open_output(idev, israte, ichans, ifmt, isize);
  audio_io_set_write_format(line, ifmt);
  return(C_TO_XEN_INT(line));
}

static XEN g_mus_audio_open_input(XEN dev, XEN srate, XEN chans, XEN format, XEN size)
{
  #define H_mus_audio_open_input "(" S_mus_audio_open_input " (device srate chans format bufsize): \
open the audio device ready for input with the indicated attributes; return the audio line number"

  int line, idev, ifmt, isize, israte, ichans;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(dev), dev, XEN_ARG_1, S_mus_audio_open_input, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(srate), srate, XEN_ARG_2, S_mus_audio_open_input, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chans), chans, XEN_ARG_3, S_mus_audio_open_input, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(format), format, XEN_ARG_4, S_mus_audio_open_input, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(size), size, XEN_ARG_5, S_mus_audio_open_input, "a number");
  idev = XEN_TO_C_INT(dev);
  israte = XEN_TO_C_INT_OR_ELSE(srate, 0);
  ichans = XEN_TO_C_INT(chans);
  ifmt = XEN_TO_C_INT(format);
  isize = XEN_TO_C_INT_OR_ELSE(size, 0);
  if (!(MUS_AUDIO_DEVICE_OK(MUS_AUDIO_DEVICE(idev))))
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_open_input, 1, dev, "~A: invalid device");
  if (!(MUS_DATA_FORMAT_OK(ifmt)))
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_open_input, 4, format, "~A: invalid data format");
  if (isize < 0)
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_open_input, 5, size, "size ~A < 0?");
  if (israte <= 0)
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_open_input, 2, srate, "srate ~A <= 0?");
  if (ichans <= 0)
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_open_input, 3, chans, "chans ~A <= 0?");
  line = mus_audio_open_input(idev, israte, ichans, ifmt, isize);
  audio_io_set_read_format(line, ifmt);
  return(C_TO_XEN_INT(line));
}

static XEN g_mus_audio_close(XEN line)
{
  int res;
  #define H_mus_audio_close "(" S_mus_audio_close " line): close the audio hardware line"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(line), line, XEN_ONLY_ARG, S_mus_audio_close, "an integer");
  res = mus_audio_close(XEN_TO_C_INT(line));
  return(C_TO_XEN_INT(res));
}

static XEN g_mus_audio_systems(void) 
{
  #define H_mus_audio_systems "(" S_mus_audio_systems "): number of audio systems; normally each sound card is a separate 'system'"
  return(C_TO_XEN_INT(mus_audio_systems()));
}


/* these take a sndlib buffer (sound_data) and handle the conversion to the interleaved char* internally */
/* so, they take "frames", not "bytes", and a sound_data object, not char* etc */

static XEN g_mus_audio_write(XEN line, XEN sdata, XEN frames)
{
  #define H_mus_audio_write "(" S_mus_audio_write " line sdata frames): write frames of data (channels * frames = samples) \
to the audio line from sound-data sdata."

  char *obuf;
  sound_data *sd;
  int outbytes, val, frms, fmt, fd;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(line), line, XEN_ARG_1, S_mus_audio_write, "an integer");
  XEN_ASSERT_TYPE(sound_data_p(sdata), sdata, XEN_ARG_2, S_mus_audio_write, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(frames), frames, XEN_ARG_3, S_mus_audio_write, "an integer");
  sd = (sound_data *)XEN_OBJECT_REF(sdata);
  frms = XEN_TO_C_INT(frames);
  if (frms > sd->length)
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_write, 3, frames, "frames ~A > sound-data buffer length");
  fd = XEN_TO_C_INT(line);
  fmt = audio_io_write_format(fd);
  outbytes = frms * sd->chans * mus_bytes_per_sample(fmt);
  obuf = (char *)CALLOC(outbytes, sizeof(char));
#if SNDLIB_USE_FLOATS
  mus_file_write_buffer(fmt, 0, frms - 1, sd->chans, sd->data, obuf, true); /* true -> clipped */
#else
  {
    mus_sample_t **sdata;
    int i, j;
    sdata = (mus_sample_t **)CALLOC(sd->chans, sizeof(mus_sample_t *));
    for (i = 0; i < sd->chans; i++) sdata[i] = (mus_sample_t *)CALLOC(sd->length, sizeof(mus_sample_t));
    for (i = 0; i < sd->chans; i++)
      for (j = 0; j < sd->length; j++)
	sdata[i][j] = MUS_DOUBLE_TO_SAMPLE(sd->data[i][j]);
    mus_file_write_buffer(fmt, 0, frms - 1, sd->chans, sdata, obuf, true);
    for (i = 0; i < sd->chans; i++)
      FREE(sdata[i]);
    FREE(sdata);
  }
#endif
  val = mus_audio_write(fd, obuf, outbytes);
  FREE(obuf);
  return(xen_return_first(C_TO_XEN_INT(val), sdata));
}

static XEN g_mus_audio_read(XEN line, XEN sdata, XEN frames)
{
  #define H_mus_audio_read "(" S_mus_audio_read " line sdata frames): read frames of data (channels * frames = samples) \
from the audio line into sound-data sdata."

  char *inbuf;
  sound_data *sd;
  int val, inbytes, frms, fd, fmt;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(line), line, XEN_ARG_1, S_mus_audio_read, "an integer");
  XEN_ASSERT_TYPE(sound_data_p(sdata), sdata, XEN_ARG_2, S_mus_audio_read, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(frames), frames, XEN_ARG_3, S_mus_audio_read, "an integer");
  sd = (sound_data *)XEN_OBJECT_REF(sdata);
  frms = XEN_TO_C_INT(frames);
  fd = XEN_TO_C_INT(line);
  fmt = audio_io_read_format(fd);
  inbytes = frms * sd->chans * mus_bytes_per_sample(fmt);
  inbuf = (char *)CALLOC(inbytes, sizeof(char));
  val = mus_audio_read(fd, inbuf, inbytes);
#if SNDLIB_USE_FLOATS
  mus_file_read_buffer(fmt, 0, sd->chans, frms, sd->data, inbuf); /* frms here because this is "nints" not "end"! */
#else
  {
    mus_sample_t **sdata;
    int i, j;
    sdata = (mus_sample_t **)CALLOC(sd->chans, sizeof(mus_sample_t *));
    for (i = 0; i < sd->chans; i++) sdata[i] = (mus_sample_t *)CALLOC(sd->length, sizeof(mus_sample_t));
    mus_file_read_buffer(fmt, 0, sd->chans, frms, sdata, inbuf);
    for (i = 0; i < sd->chans; i++)
      for (j = 0; j < sd->length; j++)
	sd->data[i][j] = MUS_SAMPLE_TO_DOUBLE(sdata[i][j]);
    for (i = 0; i < sd->chans; i++)
      FREE(sdata[i]);
    FREE(sdata);
  }
#endif
  FREE(inbuf);
  return(xen_return_first(C_TO_XEN_INT(val), sdata));
}

static XEN g_mus_audio_mixer_read(XEN dev, XEN field, XEN chan, XEN vals)
{
#if HAVE_SCHEME
  #define mixer_read_example "  (let ((vals (" S_make_vct " 32)))\n    (" S_mus_audio_mixer_read " " S_mus_audio_default " " S_mus_audio_format " 32 vals))\n"
  #define mixer_read_vct_example "(" S_vct_ref " vals 0)"
#endif
#if HAVE_RUBY
  #define mixer_read_example "  vals = make_vct(32)\n  mus_audio_mixer_read(Mus_audio_default, Mus_audio_format, 32, vals)\n"
  #define mixer_read_vct_example "vals[0]"
#endif
#if HAVE_FORTH
  #define mixer_read_example "  32 0.0 make-vct value vals\n  mus-audio-default mus-audio-format 32 vals mus-audio-mixer-read\n"
  #define mixer_read_vct_example "vals 0 vct-ref"
#endif

  #define H_mus_audio_mixer_read "(" S_mus_audio_mixer_read " device field channel vals): read some portion of the sound card mixer state.\
The device is the nominal audio device, normally " S_mus_audio_default ". The field describes what info we are requesting: \
to get the devices max available chans, the field would be " S_mus_audio_channel ". The channel arg, when relevant, specifies \
which channel we want info on or the 'vals' vct length. \
The requested info will be written into 'vals': \n\n" mixer_read_example " \n\
sets " mixer_read_vct_example " to the default device's desired audio sample data format."

  int val, i, len;
  float *fvals;
  vct *v = NULL;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(dev), dev, XEN_ARG_1, S_mus_audio_mixer_read, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(field), field, XEN_ARG_2, S_mus_audio_mixer_read, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_3, S_mus_audio_mixer_read, "an integer");
  XEN_ASSERT_TYPE(MUS_VCT_P(vals), vals, XEN_ARG_4, S_mus_audio_mixer_read, "a vct");
  if (!(MUS_AUDIO_DEVICE_OK(MUS_AUDIO_DEVICE(XEN_TO_C_INT(dev)))))
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_mixer_read, 1, dev, "~A: invalid device");
  if (!(MUS_AUDIO_DEVICE_OK(MUS_AUDIO_DEVICE(XEN_TO_C_INT(field)))))
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_mixer_read, 2, field, "~A: invalid field");
  v = XEN_TO_VCT(vals);
  len = v->length;
  fvals = (float *)CALLOC(len, sizeof(float));
  val = mus_audio_mixer_read(XEN_TO_C_INT(dev),
			     XEN_TO_C_INT(field),
			     XEN_TO_C_INT(chan),
			     fvals);
  for (i = 0; i < len; i++) 
    v->data[i] = fvals[i];
  FREE(fvals);
  return(xen_return_first(C_TO_XEN_INT(val), vals));
}

static XEN g_mus_audio_mixer_write(XEN dev, XEN field, XEN chan, XEN vals)
{
  #define H_mus_audio_mixer_write "(" S_mus_audio_mixer_write " device field channel vals): change some portion of the sound card mixer state"
  int i, len, res;
  float *fvals;
  vct *v = NULL;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(dev), dev, XEN_ARG_1, S_mus_audio_mixer_write, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(field), field, XEN_ARG_2, S_mus_audio_mixer_write, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_3, S_mus_audio_mixer_write, "an integer");
  XEN_ASSERT_TYPE(MUS_VCT_P(vals), vals, XEN_ARG_4, S_mus_audio_mixer_write, "a vct");
  if (!(MUS_AUDIO_DEVICE_OK(MUS_AUDIO_DEVICE(XEN_TO_C_INT(dev)))))
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_mixer_write, 1, dev, "~A: invalid device");
  if (!(MUS_AUDIO_DEVICE_OK(MUS_AUDIO_DEVICE(XEN_TO_C_INT(field)))))
    XEN_OUT_OF_RANGE_ERROR(S_mus_audio_mixer_write, 2, field, "~A: invalid field");
  v = XEN_TO_VCT(vals);
  len = v->length;
  fvals = (float *)CALLOC(len, sizeof(float));
  for (i = 0; i < len; i++) 
    fvals[i] = v->data[i];
  res = mus_audio_mixer_write(XEN_TO_C_INT(dev),
			      XEN_TO_C_INT(field),
			      XEN_TO_C_INT(chan),
			      fvals);
  FREE(fvals);
  return(xen_return_first(C_TO_XEN_INT(res), vals));
}


/* global default clipping and prescaler values */

static XEN g_mus_clipping(void)
{
  #define H_mus_clipping "(" S_mus_clipping "): whether sound data written to a file should be clipped"
  return(C_TO_XEN_BOOLEAN(mus_clipping()));
}

static XEN g_mus_set_clipping(XEN clipped)
{
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(clipped), clipped, XEN_ONLY_ARG, S_setB S_mus_clipping, "a boolean");
  return(C_TO_XEN_BOOLEAN(mus_set_clipping(XEN_TO_C_BOOLEAN(clipped))));
}

static XEN g_mus_prescaler(void)
{
  #define H_mus_prescaler "(" S_mus_prescaler "): default prescaler (normally 1.0)"
  return(C_TO_XEN_DOUBLE(mus_prescaler()));
}

static XEN g_mus_set_prescaler(XEN val)
{
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_mus_prescaler, "a number");
  return(C_TO_XEN_DOUBLE(mus_set_prescaler(XEN_TO_C_DOUBLE(val))));
}

/* file local clipping and prescaler values */

static XEN g_mus_file_clipping(XEN fd)
{
  #define H_mus_file_clipping "(" S_mus_file_clipping " fd): whether sound data written to file 'fd' should be clipped"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ONLY_ARG, S_mus_file_clipping, "an integer");
  return(C_TO_XEN_BOOLEAN(mus_file_clipping(XEN_TO_C_INT(fd))));
}

static XEN g_mus_file_set_clipping(XEN fd, XEN clipped)
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ARG_1, S_setB S_mus_file_clipping, "an integer");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(clipped), clipped, XEN_ARG_2, S_setB S_mus_file_clipping, "a boolean");
  return(C_TO_XEN_BOOLEAN(mus_file_set_clipping(XEN_TO_C_INT(fd), XEN_TO_C_BOOLEAN(clipped))));
}

static XEN g_mus_file_prescaler(XEN fd)
{
  #define H_mus_file_prescaler "(" S_mus_file_prescaler " fd): prescaler associated with file 'fd'"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ONLY_ARG, S_mus_file_prescaler, "an integer");
  return(C_TO_XEN_DOUBLE(mus_file_prescaler(XEN_TO_C_INT(fd))));
}

static XEN g_mus_file_set_prescaler(XEN fd, XEN val)
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ARG_1, S_setB S_mus_file_prescaler, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_1, S_setB S_mus_prescaler, "a number");
  return(C_TO_XEN_DOUBLE(mus_file_set_prescaler(XEN_TO_C_INT(fd), XEN_TO_C_DOUBLE(val))));
}


static XEN g_mus_expand_filename(XEN file)
{
  #define H_mus_expand_filename "(" S_mus_expand_filename " name): expand 'name' into a canonical or absolute filename, that is, \
one in which all directories in the path are explicit."
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ONLY_ARG, S_mus_expand_filename, "a string");
  return(C_TO_XEN_STRING(local_mus_expand_filename(XEN_TO_C_STRING(file))));
}

static XEN g_mus_sound_report_cache(XEN file)
{
  #define H_mus_sound_report_cache "(" S_mus_sound_report_cache " (name)): print the current sound \
cache info to the file given or stdout"

  XEN res = XEN_FALSE;
  if (XEN_NOT_BOUND_P(file))
    mus_sound_report_cache(stdout);
  else
    {
      FILE *fd;
      char *name;
      name = XEN_TO_C_STRING(file);
      fd = FOPEN(local_mus_expand_filename(name), "w");
      if (fd)
	{
	  mus_sound_report_cache(fd);
	  FCLOSE(fd, name);
	}
      else
	{
	  XEN_ERROR(XEN_ERROR_TYPE("cannot-save"),
		    XEN_LIST_3(C_TO_XEN_STRING(S_mus_sound_report_cache),
			       file,
			       C_TO_XEN_STRING(STRERROR(errno))));
	}
      return(file);
    }
  return(res);
}

static XEN g_mus_error_type_to_string(XEN err)
{
  #define H_mus_error_type_to_string "(" S_mus_error_type_to_string " err): string description of err (a sndlib error type)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(err), err, XEN_ONLY_ARG, S_mus_error_type_to_string, "an integer");
  return(C_TO_XEN_STRING((char *)mus_error_type_to_string(XEN_TO_C_INT(err))));
}

static XEN g_array_to_file(XEN filename, XEN data, XEN len, XEN srate, XEN channels)
{
  #define H_array_to_file "(" S_array_to_file " filename data len srate channels): write 'data', \
a vct of interleaved samples, to the sound file 'filename' set up to have the given \
srate and channels.  'len' samples are written."

  /* this exists for compatibility with the Common Lisp version of CLM. Ideally, we'd get rid
   *   of it and provide vct<->file and sound-data<->file instead so that the channels aren't
   *   handled through interleaving.  But that means extensive changes to the Lisp code...
   */

  int olen, samps;
  vct *v;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ARG_1, S_array_to_file, "a string");
  XEN_ASSERT_TYPE(MUS_VCT_P(data), data, XEN_ARG_2, S_array_to_file, "a vct");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(len), len, XEN_ARG_3, S_array_to_file, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(srate), srate, XEN_ARG_4, S_array_to_file, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(channels), channels, XEN_ARG_5, S_array_to_file, "an integer");
  v = XEN_TO_VCT(data);
  samps = XEN_TO_C_INT_OR_ELSE(len, 1);
  if (samps <= 0)
    XEN_OUT_OF_RANGE_ERROR(S_array_to_file, 3, len, "samples ~A <= 0?");
  if (samps > v->length)
    samps = v->length;
  olen = mus_float_array_to_file(XEN_TO_C_STRING(filename),
				 v->data,
				 samps,
				 XEN_TO_C_INT_OR_ELSE(srate, 0),
				 XEN_TO_C_INT(channels));
  return(xen_return_first(C_TO_XEN_INT(olen), filename));
}

static XEN g_file_to_array(XEN filename, XEN chan, XEN start, XEN samples, XEN data)
{
  #define H_file_to_array "(" S_file_to_array " filename chan start samples data): read the sound file \
'filename' placing samples from channel 'chan' into the vct 'data' starting in the file \
at frame 'start' and reading 'samples' samples altogether."

  int chn, samps;
  vct *v;
  char *name = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ARG_1, S_file_to_array, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_2, S_file_to_array, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(start), start, XEN_ARG_3, S_file_to_array, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(samples), samples, XEN_ARG_4, S_file_to_array, "a number");
  XEN_ASSERT_TYPE((MUS_VCT_P(data)), data, XEN_ARG_5, S_file_to_array, "a vct");
  name = XEN_TO_C_STRING(filename);
  if (!(mus_file_probe(name)))
    XEN_ERROR(NO_SUCH_FILE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_file_to_array),
			 filename,
			 C_TO_XEN_STRING(STRERROR(errno))));
  v = XEN_TO_VCT(data);
  samps = XEN_TO_C_INT_OR_ELSE(samples, 1);
  if (samps <= 0) 
    XEN_OUT_OF_RANGE_ERROR(S_file_to_array, 4, samples, "samples ~A <= 0?");
  chn = XEN_TO_C_INT(chan);
  if ((chn < 0) || (chn > mus_sound_chans(name)))
    XEN_ERROR(NO_SUCH_CHANNEL,
	      XEN_LIST_3(C_TO_XEN_STRING(S_file_to_array),
			 C_TO_XEN_STRING("invalid chan: ~A, ~A has ~A chans"),
			 XEN_LIST_3(chan,
				    filename,
				    C_TO_XEN_INT(mus_sound_chans(name)))));
  if (mus_sound_chans(name) <= 0)
    XEN_ERROR(BAD_HEADER,
	      XEN_LIST_3(C_TO_XEN_STRING(S_file_to_array),
			 filename,
			 C_TO_XEN_STRING("chans <= 0")));
  if (samps > v->length)
    samps = v->length;
  mus_file_to_float_array(name,
		    chn,
		    XEN_TO_C_OFF_T_OR_ELSE(start, 0),
		    samps,
		    v->data);
  return(xen_return_first(data, filename));
}


static XEN new_sound_hook;
static void g_new_sound_hook(const char *filename)
{
  if (XEN_HOOKED(new_sound_hook))
    {
      XEN procs = XEN_HOOK_PROCEDURES(new_sound_hook);
      while (XEN_NOT_NULL_P(procs))
	{
	  XEN_CALL_1(XEN_CAR(procs), 
		     C_TO_XEN_STRING(filename),
		     S_new_sound_hook);
	  procs = XEN_CDR (procs);
	}
    }
}


#if MUS_DEBUGGING && HAVE_SCHEME
static XEN g_mus_header_original_format_name(XEN format, XEN type)
{
  return(C_TO_XEN_STRING(mus_header_original_format_name(XEN_TO_C_INT(format), XEN_TO_C_INT(type))));
}
#endif


#if HAVE_OSS
#define S_mus_audio_reinitialize "mus-audio-reinitialize"
static XEN g_mus_audio_reinitialize(void)
{
  #define H_mus_audio_reinitialize "(" S_mus_audio_reinitialize "): force audio device re-initialization"
  return(C_TO_XEN_INT(mus_audio_reinitialize()));
}
#endif



static XEN_OBJECT_TYPE sound_data_tag = 0;
bool sound_data_p(XEN obj) {return(XEN_OBJECT_TYPE_P(obj, sound_data_tag));}
#define SOUND_DATA_P(Obj) XEN_OBJECT_TYPE_P(Obj, sound_data_tag)

static XEN g_sound_data_p(XEN obj) 
{
  #define H_sound_data_p "(" S_sound_data_p " obj): is 'obj' is a sound-data object"
  return(C_TO_XEN_BOOLEAN(sound_data_p(obj)));
}

void sound_data_free(sound_data *sd)
{
  if (sd)
    {
      if ((sd->data) && (!(sd->wrapped)))
	{
	  int i;
	  for (i = 0; i < sd->chans; i++) if (sd->data[i]) FREE(sd->data[i]);
	  FREE(sd->data);
	}
      sd->data = NULL;
      sd->chans = 0;
      FREE(sd);
    }
}

XEN_MAKE_OBJECT_FREE_PROCEDURE(sound_data, free_sound_data, sound_data_free)

char *sound_data_to_string(sound_data *sd)
{
  char *buf;
  int len, chans;
  char flt[24];
  if (sd == NULL) return(NULL);
  len = mus_vct_print_length();
  if (len > sd->length) len = sd->length;
  chans = sd->chans;
  buf = (char *)CALLOC(64 + len * 24 * chans, sizeof(char));
  sprintf(buf, "#<sound-data[chans=%d, length=%d]:", sd->chans, sd->length);
  if (len > 0)
    {
      int i, chn;
      for (chn = 0; chn < chans; chn++)
	{
	  mus_snprintf(flt, 24, "\n    (%.3f", sd->data[chn][0]);
	  strcat(buf, flt);
	  for (i = 1; i < len; i++)
	    {
	      mus_snprintf(flt, 24, " %.3f", sd->data[chn][i]);
	      strcat(buf, flt);
	    }
	  if (sd->length > mus_vct_print_length())
	    strcat(buf, " ...");
	  strcat(buf, ")");
	}
    }
  strcat(buf, ">");
  return(buf);
}

XEN_MAKE_OBJECT_PRINT_PROCEDURE(sound_data, print_sound_data, sound_data_to_string)

bool sound_data_equalp(sound_data *v1, sound_data *v2)
{
  if (v1 == v2) return(true);
  if ((v1) && (v2) &&
      (v1->chans == v2->chans) &&
      (v1->length == v2->length))
    {
      int chn;
      for (chn = 0; chn < v1->chans; chn++)
	if (!(mus_arrays_are_equal(v1->data[chn], v2->data[chn],
				   mus_float_equal_fudge_factor(),
				   v1->length)))
	  return(false);
      return(true);
    }
  return(false);
}

static XEN equalp_sound_data(XEN obj1, XEN obj2)
{
#if HAVE_RUBY || HAVE_FORTH
  if ((!(SOUND_DATA_P(obj1))) || (!(SOUND_DATA_P(obj2)))) return(XEN_FALSE);
#endif
  return(xen_return_first(C_TO_XEN_BOOLEAN(sound_data_equalp((sound_data *)XEN_OBJECT_REF(obj1), (sound_data *)XEN_OBJECT_REF(obj2))), obj1, obj2));
}

static XEN g_sound_data_length(XEN obj)
{
  #define H_sound_data_length "(" S_sound_data_length " sd): length (in samples) of each channel of sound-data sd"
  sound_data *sd;
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ONLY_ARG, S_sound_data_length, "a sound-data object");
  sd = (sound_data *)XEN_OBJECT_REF(obj);
  return(C_TO_XEN_INT(sd->length));
}

static XEN g_sound_data_chans(XEN obj)
{
  #define H_sound_data_chans "(" S_sound_data_chans " sd): number of channels in sound-data sd"
  sound_data *sd;
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ONLY_ARG, S_sound_data_chans, "a sound-data object");
  sd = (sound_data *)XEN_OBJECT_REF(obj);
  return(C_TO_XEN_INT(sd->chans));
}

sound_data *c_make_sound_data(int chans, int frames)
{
  int i;
  sound_data *sd;
  sd = (sound_data *)MALLOC(sizeof(sound_data));
#if MUS_DEBUGGING && USE_SND
  set_printable(PRINT_SOUND_DATA);
#endif
  sd->length = frames;
  sd->chans = chans;
  sd->wrapped = false;
  sd->data = (Float **)CALLOC(chans, sizeof(Float *));
  for (i = 0; i < chans; i++)
    sd->data[i] = (Float *)CALLOC(frames, sizeof(Float));
  return(sd);
}

XEN make_sound_data(int chans, int frames)
{
  #define H_make_sound_data "(" S_make_sound_data " chans frames): return a new sound-data object with 'chans' channels, each having 'frames' samples"
  sound_data *sd;
  sd = c_make_sound_data(chans, frames);
  XEN_MAKE_AND_RETURN_OBJECT(sound_data_tag, sd, 0, free_sound_data);
}

XEN wrap_sound_data(int chans, int frames, Float **data)
{
  sound_data *sd;
  sd = (sound_data *)MALLOC(sizeof(sound_data));
  sd->length = frames;
  sd->chans = chans;
  sd->wrapped = true;
  sd->data = data;
  XEN_MAKE_AND_RETURN_OBJECT(sound_data_tag, sd, 0, free_sound_data);
}

static XEN g_make_sound_data(XEN chans, XEN frames)
{
  int chns, frms;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chans), chans, XEN_ARG_1, S_make_sound_data, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(frames), frames, XEN_ARG_2, S_make_sound_data, "an integer");
  chns = XEN_TO_C_INT(chans);
  frms = XEN_TO_C_INT(frames);
  if (chns <= 0)
    XEN_OUT_OF_RANGE_ERROR(S_make_sound_data, 1, chans, "chans ~A <= 0?");
  if (chns > (1 << 26))
    XEN_OUT_OF_RANGE_ERROR(S_make_sound_data, 1, chans, "chans arg ~A too large");
  if (frms <= 0)
    XEN_OUT_OF_RANGE_ERROR(S_make_sound_data, 2, frames, "frames ~A <= 0?");
  if (frms > (1 << 26))
    XEN_OUT_OF_RANGE_ERROR(S_make_sound_data, 2, frames, "frames arg ~A too large");
  return(make_sound_data(chns, frms));
			 
}

static XEN g_sound_data_ref(XEN obj, XEN chan, XEN frame_num)
{
  #define H_sound_data_ref "(" S_sound_data_ref " sd chan i): sample in channel chan at location i of sound-data sd: sd[chan][i]"
  sound_data *sd;
  int loc, chn;
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ARG_1, S_sound_data_ref, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_2, S_sound_data_ref, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(frame_num), frame_num, XEN_ARG_3, S_sound_data_ref, "an integer");
  sd = (sound_data *)XEN_OBJECT_REF(obj);
  chn = XEN_TO_C_INT(chan);
  if (chn < 0)
    XEN_OUT_OF_RANGE_ERROR(S_sound_data_ref, 2, chan, "~A: invalid channel");
  if (chn >= sd->chans)
    XEN_ERROR(XEN_ERROR_TYPE("out-of-range"),
	      XEN_LIST_3(C_TO_XEN_STRING(S_sound_data_ref),
			 C_TO_XEN_STRING("chan: ~A >= sound-data chans: ~A"),
			 XEN_LIST_2(chan, C_TO_XEN_INT(sd->chans))));
  loc = XEN_TO_C_INT(frame_num);
  if (loc < 0)
    XEN_OUT_OF_RANGE_ERROR(S_sound_data_ref, 3, frame_num, "~A: invalid frame");
  if (loc >= sd->length)
    XEN_ERROR(XEN_ERROR_TYPE("out-of-range"),
	      XEN_LIST_3(C_TO_XEN_STRING(S_sound_data_ref),
			 C_TO_XEN_STRING("frame: ~A >= sound-data length: ~A"),
			 XEN_LIST_2(frame_num, C_TO_XEN_INT(sd->length))));
  return(C_TO_XEN_DOUBLE(sd->data[chn][loc]));
}

static XEN g_sound_data_maxamp(XEN obj)
{
  #define H_sound_data_maxamp "(" S_sound_data_maxamp " sd): list of maxamps of data in sd"
  sound_data *sd;
  int i, j, chans, len;
  XEN lst = XEN_EMPTY_LIST;
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ARG_1, S_sound_data_maxamp, "a sound-data object");
  sd = (sound_data *)XEN_OBJECT_REF(obj);
  chans = sd->chans;
  len = sd->length;
  for (i = chans - 1; i >= 0; i--)
    {
      Float mx;
      Float *buf;
      mx = -10000.0;
      buf = sd->data[i];
      for (j = 0; j < len; j++)
	{
	  if (buf[j] > mx)
	    mx = buf[j];
	  else
	    if (-buf[j] > mx)
	      mx = -buf[j];
	}
      lst = XEN_CONS(C_TO_XEN_DOUBLE(mx), lst);
    }
  return(lst);
}

Float sound_data_peak(sound_data *sd)
{
  int i, chn;
  Float mx = 0.0;
  for (chn = 0; chn < sd->chans; chn++)
    for (i = 0; i < sd->length; i++) 
      if (fabs(sd->data[chn][i]) > mx)
	mx = fabs(sd->data[chn][i]);
  return(mx);
}

static XEN g_sound_data_peak(XEN obj)
{
  #define H_sound_data_peak "(" S_sound_data_peak " sd): overall maxamp of data in sd"
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ARG_1, S_sound_data_maxamp, "a sound-data object");
  return(C_TO_XEN_DOUBLE(sound_data_peak((sound_data *)XEN_OBJECT_REF(obj))));
}


#if HAVE_APPLICABLE_SMOB || HAVE_FORTH
static XEN sound_data_apply(XEN obj, XEN chan, XEN i)
{
  return(g_sound_data_ref(obj, chan, i));
}
#endif

static XEN g_sound_data_set(XEN obj, XEN chan, XEN frame_num, XEN val)
{
  #define H_sound_data_setB "(" S_sound_data_setB " sd chan i val): set sound-data sd's i-th element in channel chan to val: sd[chan][i] = val"
  sound_data *sd;
  int loc, chn;
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ARG_1, S_sound_data_setB, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_2, S_sound_data_setB, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(frame_num), frame_num, XEN_ARG_3, S_sound_data_setB, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_4, S_sound_data_setB, "a number");
  sd = (sound_data *)XEN_OBJECT_REF(obj);
  chn = XEN_TO_C_INT(chan);
  if (chn < 0)
    XEN_OUT_OF_RANGE_ERROR(S_sound_data_setB, 2, chan, "~A: invalid channel");
  if (chn >= sd->chans)
    XEN_ERROR(XEN_ERROR_TYPE("out-of-range"),
	      XEN_LIST_3(C_TO_XEN_STRING(S_sound_data_setB),
			 C_TO_XEN_STRING("chan: ~A >= sound-data chans: ~A"),
			 XEN_LIST_2(chan, C_TO_XEN_INT(sd->chans))));
  loc = XEN_TO_C_INT(frame_num);
  if (loc < 0)
    XEN_OUT_OF_RANGE_ERROR(S_sound_data_setB, 3, frame_num, "~A: invalid frame");
  if (loc >= sd->length)
    XEN_ERROR(XEN_ERROR_TYPE("out-of-range"),
	      XEN_LIST_3(C_TO_XEN_STRING(S_sound_data_setB),
			 C_TO_XEN_STRING("frame: ~A >= sound-data length: ~A"),
			 XEN_LIST_2(frame_num, C_TO_XEN_INT(sd->length))));
  sd->data[chn][loc] = XEN_TO_C_DOUBLE(val);
  return(val);
}

sound_data *sound_data_scale(sound_data *sd, Float scaler)
{
  int i, chn;
  if (scaler == 0.0)
    {
      for (chn = 0; chn < sd->chans; chn++)
	mus_clear_array(sd->data[chn], sd->length);
    }
  else
    {
      if (scaler != 1.0)
	for (chn = 0; chn < sd->chans; chn++)
	  for (i = 0; i < sd->length; i++) 
	    sd->data[chn][i] *= scaler;
    }
  return(sd);
}

static XEN g_sound_data_scaleB(XEN sdobj, XEN scl)
{
  #define H_sound_data_scaleB "(" S_sound_data_scaleB " sd scl): scales (multiplies) sound-data sd's data by scl"
  XEN_ASSERT_TYPE(SOUND_DATA_P(sdobj), sdobj, XEN_ARG_1, S_sound_data_scaleB, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(scl), scl, XEN_ARG_2, S_sound_data_scaleB, "a number");
  sound_data_scale((sound_data *)XEN_OBJECT_REF(sdobj), XEN_TO_C_DOUBLE(scl));
  return(sdobj);
}

sound_data *sound_data_fill(sound_data *sd, Float scaler)
{
  int i, chn;
  if (scaler == 0.0)
    {
      for (chn = 0; chn < sd->chans; chn++)
	mus_clear_array(sd->data[chn], sd->length);
    }
  else
    {
      for (chn = 0; chn < sd->chans; chn++)
	for (i = 0; i < sd->length; i++) 
	  sd->data[chn][i] = scaler;
    }
  return(sd);
}

static XEN g_sound_data_fillB(XEN sdobj, XEN scl)
{
  #define H_sound_data_fillB "(" S_sound_data_fillB " sd value): fills the sound-data object sd with value"
  XEN_ASSERT_TYPE(SOUND_DATA_P(sdobj), sdobj, XEN_ARG_1, S_sound_data_fillB, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(scl), scl, XEN_ARG_2, S_sound_data_fillB, "a number");
  sound_data_fill((sound_data *)XEN_OBJECT_REF(sdobj), XEN_TO_C_DOUBLE(scl));
  return(sdobj);
}

sound_data *sound_data_copy(sound_data *sd)
{
  int i;
  sound_data *sdnew;
  sdnew = c_make_sound_data(sd->chans, sd->length);
  for (i = 0; i < sd->chans; i++)
    memcpy((void *)(sdnew->data[i]), (void *)(sd->data[i]), sd->length * sizeof(Float));
  return(sdnew);
}

static XEN g_sound_data_copy(XEN obj)
{
  sound_data *sdnew;
  #define H_sound_data_copy "(" S_sound_data_copy " sd) returns a copy of the sound-data object sd"
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ONLY_ARG, S_sound_data_copy, "a sound-data object");
  sdnew = sound_data_copy((sound_data *)XEN_OBJECT_REF(obj));
  XEN_MAKE_AND_RETURN_OBJECT(sound_data_tag, sdnew, 0, free_sound_data);
}

sound_data *sound_data_reverse(sound_data *sd)
{
  int chn, i, j;
  Float tmp;
  for (chn = 0; chn < sd->chans; chn++)
    for (i = 0, j = sd->length - 1; i < j; i++, j--)
      {
	tmp = sd->data[chn][i];
	sd->data[chn][i] = sd->data[chn][j];
	sd->data[chn][j] = tmp;
      }
  return(sd);
}

static XEN g_sound_data_reverseB(XEN obj)
{
  #define H_sound_data_reverseB "(" S_sound_data_reverseB " sd) reverses the elements (within each channel) of sound-data object sd"
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ONLY_ARG, S_sound_data_reverseB, "a sound-data object");
  sound_data_reverse((sound_data *)XEN_OBJECT_REF(obj));
  return(obj);
}

sound_data *sound_data_add(sound_data *sd1, sound_data *sd2)
{
  int i, j, chns, len;
  chns = sd1->chans;
  if (chns > sd2->chans) chns = sd2->chans;
  len = sd1->length;
  if (len > sd2->length) len = sd2->length;
  for (i = 0; i < chns; i++)
    for (j = 0; j < len; j++)
      sd1->data[i][j] += sd2->data[i][j];
  return(sd1);
}

static XEN g_sound_data_addB(XEN obj1, XEN obj2)
{
  #define H_sound_data_addB "(" S_sound_data_addB " sd1 sd2) adds (element-wise) sd2 to sd1, returning sd1"
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj1), obj1, XEN_ARG_1, S_sound_data_addB, "a sound-data object");
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj2), obj2, XEN_ARG_2, S_sound_data_addB, "a sound-data object");
  sound_data_add((sound_data *)XEN_OBJECT_REF(obj1), (sound_data *)XEN_OBJECT_REF(obj2));
  return(obj1);
}

sound_data *sound_data_multiply(sound_data *sd1, sound_data *sd2)
{
  int i, j, chns, len;
  chns = sd1->chans;
  if (chns > sd2->chans) chns = sd2->chans;
  len = sd1->length;
  if (len > sd2->length) len = sd2->length;
  for (i = 0; i < chns; i++)
    for (j = 0; j < len; j++)
      sd1->data[i][j] *= sd2->data[i][j];
  return(sd1);
}

static XEN g_sound_data_multiplyB(XEN obj1, XEN obj2)
{
  #define H_sound_data_multiplyB "(" S_sound_data_multiplyB " sd1 sd2) multiplies (element-wise) sd1 by sd2, returning sd1"
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj1), obj1, XEN_ARG_1, S_sound_data_multiplyB, "a sound-data object");
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj2), obj2, XEN_ARG_2, S_sound_data_multiplyB, "a sound-data object");
  sound_data_multiply((sound_data *)XEN_OBJECT_REF(obj1), (sound_data *)XEN_OBJECT_REF(obj2));
  return(obj1);
}

sound_data *sound_data_offset(sound_data *sd, Float off)
{
  int i, j;
  if (off != 0.0)
    {
      for (i = 0; i < sd->chans; i++)
	for (j = 0; j < sd->length; j++)
	  sd->data[i][j] += off;
    }
  return(sd);
}

static XEN g_sound_data_offsetB(XEN obj, XEN offset)
{
  #define H_sound_data_offsetB "(" S_sound_data_offsetB " sd val) adds val to each element of sd, returning sd"
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ARG_1, S_sound_data_offsetB, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(offset), offset, XEN_ARG_2, S_sound_data_offsetB, "a number");
  sound_data_offset((sound_data *)XEN_OBJECT_REF(obj), XEN_TO_C_DOUBLE(offset));
  return(obj);
}

static XEN g_sound_data_add(XEN obj1, XEN obj2)
{
  #define H_sound_data_add "(" S_sound_data_add " obj1 obj2) adds obj1 to obj2, either or both of which can be sound-data objects"
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj1) || XEN_NUMBER_P(obj1), obj1, XEN_ARG_1, S_sound_data_add, "a sound-data object or a number");
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj2) || XEN_NUMBER_P(obj2), obj2, XEN_ARG_2, S_sound_data_add, "a sound-data object or a number");
  if (SOUND_DATA_P(obj1))
    {
      if (SOUND_DATA_P(obj2))
	return(g_sound_data_addB(obj1, obj2));
      return(g_sound_data_offsetB(obj1, obj2));
    }
  if (SOUND_DATA_P(obj2))
    return(g_sound_data_offsetB(obj2, obj1));
  return(C_TO_XEN_DOUBLE(XEN_TO_C_DOUBLE(obj1) + XEN_TO_C_DOUBLE(obj2)));
}

static XEN g_sound_data_multiply(XEN obj1, XEN obj2)
{
  #define H_sound_data_multiply "(" S_sound_data_multiply " obj1 obj2) multiplies obj1 by obj2, either or both of which can be sound-data objects"
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj1) || XEN_NUMBER_P(obj1), obj1, XEN_ARG_1, S_sound_data_multiply, "a sound-data object or a number");
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj2) || XEN_NUMBER_P(obj2), obj2, XEN_ARG_2, S_sound_data_multiply, "a sound-data object or a number");
  if (SOUND_DATA_P(obj1))
    {
      if (SOUND_DATA_P(obj2))
	return(g_sound_data_multiplyB(obj1, obj2));
      return(g_sound_data_scaleB(obj1, obj2));
    }
  if (SOUND_DATA_P(obj2))
    return(g_sound_data_scaleB(obj2, obj1));
  return(C_TO_XEN_DOUBLE(XEN_TO_C_DOUBLE(obj1) * XEN_TO_C_DOUBLE(obj2)));
}



static XEN g_sound_data_to_vct(XEN sdobj, XEN chan, XEN vobj)
{
  #define H_sound_data_to_vct "(" S_sound_data_to_vct " sd chan v): copies sound-data sd's channel chan data into vct v"
  vct *v;
  sound_data *sd;
  int len, chn;
  XEN_ASSERT_TYPE(SOUND_DATA_P(sdobj), sdobj, XEN_ARG_1, S_sound_data_to_vct, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chan), chan, XEN_ARG_2, S_sound_data_to_vct, "an integer");
  XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(vobj) || MUS_VCT_P(vobj), vobj, XEN_ARG_3, S_sound_data_to_vct, "a vct");
  sd = (sound_data *)XEN_OBJECT_REF(sdobj);
  chn = XEN_TO_C_INT_OR_ELSE(chan, 0);
  if (chn < 0)
    XEN_OUT_OF_RANGE_ERROR(S_sound_data_to_vct, 2, chan, "~A: invalid channel");
  if (chn >= sd->chans)
    XEN_ERROR(XEN_ERROR_TYPE("out-of-range"),
	      XEN_LIST_3(C_TO_XEN_STRING(S_sound_data_to_vct),
			 C_TO_XEN_STRING("chan: ~A >= sound-data chans: ~A"),
			 XEN_LIST_2(chan, C_TO_XEN_INT(sd->chans))));
  if (!(MUS_VCT_P(vobj))) 
    vobj = xen_make_vct(sd->length, (Float *)CALLOC(sd->length, sizeof(Float)));
  v = XEN_TO_VCT(vobj);
  if (sd->length < v->length) 
    len = sd->length; 
  else len = v->length;
  memcpy((void *)(v->data), (void *)(sd->data[chn]), len * sizeof(Float));
  return(vobj);
}

static XEN g_vct_to_sound_data(XEN vobj, XEN sdobj, XEN chan)
{
  #define H_vct_to_sound_data "(" S_vct_to_sound_data " v sd chan): copies vct v's data into sound-data sd's channel chan"
  vct *v;
  sound_data *sd;
  XEN obj = XEN_FALSE;
  int len, chn;
  XEN_ASSERT_TYPE(MUS_VCT_P(vobj), vobj, XEN_ARG_1, S_vct_to_sound_data, "a vct");
  XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(sdobj) || SOUND_DATA_P(sdobj), sdobj, XEN_ARG_2, S_vct_to_sound_data, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chan), chan, XEN_ARG_3, S_vct_to_sound_data, "an integer");
  v = XEN_TO_VCT(vobj);
  chn = XEN_TO_C_INT_OR_ELSE(chan, 0);
  if (chn < 0)
    XEN_OUT_OF_RANGE_ERROR(S_vct_to_sound_data, 3, chan, "~A: invalid channel");
  if (!(SOUND_DATA_P(sdobj)))
    {
      if (chn > 0)
	XEN_OUT_OF_RANGE_ERROR(S_vct_to_sound_data, 3, chan, "~A: invalid channel");
      obj = make_sound_data(1, v->length);
      sd = (sound_data *)XEN_OBJECT_REF(obj);
    }
  else
    {
      sd = (sound_data *)XEN_OBJECT_REF(sdobj);
      if (chn >= sd->chans)
	XEN_ERROR(XEN_ERROR_TYPE("out-of-range"),
		  XEN_LIST_3(C_TO_XEN_STRING(S_vct_to_sound_data),
			     C_TO_XEN_STRING("chan: ~A >= sound-data chans: ~A"),
			     XEN_LIST_2(chan, C_TO_XEN_INT(sd->chans))));
      obj = sdobj;
    }
  if (sd->length < v->length) 
    len = sd->length; 
  else len = v->length;
  memcpy((void *)(sd->data[chn]), (void *)(v->data), len * sizeof(Float));
  return(obj);
}

#define S_sound_data_to_sound_data "sound-data->sound-data"
static XEN g_sound_data_to_sound_data(XEN sd_in, XEN sd_out, XEN start, XEN frames, XEN cycle_length)
{
  #define H_sound_data_to_sound_data "(" S_sound_data_to_sound_data " sd-in sd-out start frames cycle-length): \
copies sound-data sd-in's data from 0 for 'frames' frames into 'sd-out' starting at 'start', wrapping around if sd-out's end (or cycle-length) is reached."
  sound_data *sdi, *sdo;
  int len, beg, i, j = 0, ilen, olen, cycle, chans;
  XEN_ASSERT_TYPE(SOUND_DATA_P(sd_in), sd_in, XEN_ARG_1, S_sound_data_to_sound_data, "a sound-data object");
  XEN_ASSERT_TYPE(SOUND_DATA_P(sd_out), sd_out, XEN_ARG_2, S_sound_data_to_sound_data, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(start), start, XEN_ARG_3, S_sound_data_to_sound_data, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(frames), frames, XEN_ARG_4, S_sound_data_to_sound_data, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(cycle_length), cycle_length, XEN_ARG_5, S_sound_data_to_sound_data, "an integer");
  sdi = (sound_data *)XEN_OBJECT_REF(sd_in);
  sdo = (sound_data *)XEN_OBJECT_REF(sd_out);
  ilen = sdi->length;
  olen = sdo->length;
  beg = XEN_TO_C_INT(start);
  if (beg < 0)
    XEN_ERROR(XEN_ERROR_TYPE("out-of-range"),
	      XEN_LIST_3(C_TO_XEN_STRING(S_sound_data_to_sound_data),
			 C_TO_XEN_STRING("start: ~A < 0"),
			 XEN_LIST_1(start)));
  if (beg >= olen) beg = 0;
  len = XEN_TO_C_INT(frames);
  if ((len < 0) || (len > ilen))
    XEN_ERROR(XEN_ERROR_TYPE("out-of-range"),
	      XEN_LIST_3(C_TO_XEN_STRING(S_sound_data_to_sound_data),
			 C_TO_XEN_STRING("frames: ~A?"),
			 XEN_LIST_1(frames)));
  cycle = XEN_TO_C_INT(cycle_length);
  if (beg >= cycle) beg = 0;
  if (cycle > olen) cycle = olen;
  chans = sdo->chans;
  if (chans > sdi->chans)
    chans = sdi->chans;
  if ((beg + len) < cycle)
    {
      for (i = 0; i < chans; i++)
	memmove((void *)(sdo->data[i] + beg), (void *)(sdi->data[i]), len * sizeof(Float));
      j = beg + len;
    }
  else
    {
      int k;
      for (i = 0; i < chans; i++)
	{
	  j = beg;
	  for (k = 0; k < len; k++)
	    {
	      sdo->data[i][j++] = sdi->data[i][k];
	      if (j == cycle) j = 0;
	    }
	}
    }
  return(C_TO_XEN_INT(j));
}


#if HAVE_FORTH
#define S_sound_data_to_vector          "sound-data->vector"
static XEN g_sound_data_to_vector(XEN sdata)
{
#define H_sound_data_to_vector "(" S_sound_data_to_vector " sd):  \
returns a vector of length sd->chans containing all channels of sound-data sd as vct."
  long chn;
  sound_data *sd;
  FTH vec;

  XEN_ASSERT_TYPE(SOUND_DATA_P(sdata), sdata, XEN_ONLY_ARG, S_sound_data_to_vector, "a sound-data object");
  sd = (sound_data *)XEN_OBJECT_REF(sdata);
  vec = XEN_MAKE_VECTOR(sd->chans, FTH_NIL);

  for (chn = 0; chn < sd->chans; chn++)
    XEN_VECTOR_SET(vec, chn, g_sound_data_to_vct(sdata, C_TO_XEN_INT(chn), XEN_UNDEFINED));
  return vec;
}
#endif

#if HAVE_RUBY
static XEN sound_data_each(XEN obj)
{
  int i, j;
  sound_data *sd;
  sd = (sound_data *)XEN_OBJECT_REF(obj);
  for (j = 0; j < sd->chans; j++)
    for (i = 0; i < sd->length; i++)
      rb_yield(C_TO_XEN_DOUBLE(sd->data[j][i]));
  return(obj);
}

static XEN sound_data_compare(XEN vr1, XEN vr2)
{
  int i, len, j;
  sound_data *v1, *v2;
  if ((SOUND_DATA_P(vr1)) && (SOUND_DATA_P(vr2)))
    {
      v1 = (sound_data *)XEN_OBJECT_REF(vr1);
      v2 = (sound_data *)XEN_OBJECT_REF(vr2);
      if (v1->chans > v2->chans) 
	return(C_TO_XEN_INT(1));
      if (v1->chans < v2->chans)
	return(C_TO_XEN_INT(-1));
      len = v1->length;
      if (len > v2->length) len = v2->length;
      for (j = 0; j < v1->chans; j++)
	for (i = 0; i < len; i++) 
	  if (v1->data[j][i] < v2->data[j][i])
	    return(C_TO_XEN_INT(-1));
	  else
	    if (v1->data[j][i] > v2->data[j][i])
	      return(C_TO_XEN_INT(1));
      len = v1->length - v2->length;
      if (len == 0) return(XEN_ZERO);
      if (len > 0) return(C_TO_XEN_INT(1));
    }
  return(C_TO_XEN_INT(-1));
}

static XEN sound_data_size(XEN obj)
{
  sound_data *sd;
  sd = (sound_data *)XEN_OBJECT_REF(obj);
  return(C_TO_XEN_INT(sd->length * sd->chans));
}

static XEN sound_data_chans(XEN obj)
{
  sound_data *sd;
  sd = (sound_data *)XEN_OBJECT_REF(obj);
  return(C_TO_XEN_INT(sd->chans));
}

static XEN g_rb_sound_data_fill(XEN obj, XEN val)
{
  sound_data_fill((sound_data *)XEN_OBJECT_REF(obj), XEN_TO_C_DOUBLE(val));
  return(val);
}

static XEN g_rb_make_sound_data(XEN self, XEN chans, XEN frames)
{
  return(g_make_sound_data(chans, frames));
}
#endif



#ifdef XEN_ARGIFY_1
XEN_NARGIFY_1(g_sound_data_length_w, g_sound_data_length)
XEN_NARGIFY_1(g_sound_data_chans_w, g_sound_data_chans)
XEN_NARGIFY_1(g_sound_data_copy_w, g_sound_data_copy)
XEN_NARGIFY_2(g_sound_data_addB_w, g_sound_data_addB)
XEN_NARGIFY_2(g_sound_data_add_w, g_sound_data_add)
XEN_NARGIFY_2(g_sound_data_offsetB_w, g_sound_data_offsetB)
XEN_NARGIFY_2(g_sound_data_multiplyB_w, g_sound_data_multiplyB)
XEN_NARGIFY_2(g_sound_data_multiply_w, g_sound_data_multiply)
XEN_NARGIFY_3(g_sound_data_ref_w, g_sound_data_ref)
XEN_NARGIFY_4(g_sound_data_set_w, g_sound_data_set)
XEN_NARGIFY_2(g_make_sound_data_w, g_make_sound_data)
XEN_NARGIFY_1(g_sound_data_p_w, g_sound_data_p)
XEN_NARGIFY_1(g_sound_data_maxamp_w, g_sound_data_maxamp)
XEN_NARGIFY_1(g_sound_data_peak_w, g_sound_data_peak)
XEN_NARGIFY_2(g_sound_data_scaleB_w, g_sound_data_scaleB)
XEN_NARGIFY_2(g_sound_data_fillB_w, g_sound_data_fillB)
XEN_NARGIFY_1(g_sound_data_reverseB_w, g_sound_data_reverseB)
XEN_ARGIFY_3(g_sound_data_to_vct_w, g_sound_data_to_vct)
XEN_NARGIFY_5(g_sound_data_to_sound_data_w, g_sound_data_to_sound_data)
XEN_ARGIFY_3(g_vct_to_sound_data_w, g_vct_to_sound_data)
XEN_NARGIFY_1(g_mus_sound_samples_w, g_mus_sound_samples)
XEN_NARGIFY_2(g_mus_sound_set_samples_w, g_mus_sound_set_samples)
XEN_NARGIFY_1(g_mus_sound_frames_w, g_mus_sound_frames)
XEN_NARGIFY_1(g_mus_sound_duration_w, g_mus_sound_duration)
XEN_NARGIFY_1(g_mus_sound_datum_size_w, g_mus_sound_datum_size)
XEN_NARGIFY_1(g_mus_sound_data_location_w, g_mus_sound_data_location)
XEN_NARGIFY_2(g_mus_sound_set_data_location_w, g_mus_sound_set_data_location)
XEN_NARGIFY_1(g_mus_sound_chans_w, g_mus_sound_chans)
XEN_NARGIFY_2(g_mus_sound_set_chans_w, g_mus_sound_set_chans)
XEN_NARGIFY_1(g_mus_sound_srate_w, g_mus_sound_srate)
XEN_NARGIFY_2(g_mus_sound_set_srate_w, g_mus_sound_set_srate)
XEN_NARGIFY_1(g_mus_sound_header_type_w, g_mus_sound_header_type)
XEN_NARGIFY_2(g_mus_sound_set_header_type_w, g_mus_sound_set_header_type)
XEN_NARGIFY_1(g_mus_sound_data_format_w, g_mus_sound_data_format)
XEN_NARGIFY_2(g_mus_sound_set_data_format_w, g_mus_sound_set_data_format)
XEN_NARGIFY_1(g_mus_sound_length_w, g_mus_sound_length)
XEN_NARGIFY_1(g_mus_sound_type_specifier_w, g_mus_sound_type_specifier)
XEN_NARGIFY_1(g_mus_header_type_name_w, g_mus_header_type_name)
XEN_NARGIFY_1(g_mus_header_type_to_string_w, g_mus_header_type_to_string)
XEN_NARGIFY_1(g_mus_data_format_name_w, g_mus_data_format_name)
XEN_NARGIFY_1(g_mus_data_format_to_string_w, g_mus_data_format_to_string)
XEN_NARGIFY_1(g_mus_sound_comment_w, g_mus_sound_comment)
XEN_NARGIFY_1(g_mus_sound_write_date_w, g_mus_sound_write_date)
XEN_NARGIFY_1(g_mus_bytes_per_sample_w, g_mus_bytes_per_sample)
XEN_NARGIFY_1(g_mus_sound_loop_info_w, g_mus_sound_loop_info)
XEN_NARGIFY_1(g_mus_sound_maxamp_w, g_mus_sound_maxamp)
XEN_NARGIFY_2(g_mus_sound_set_maxamp_w, g_mus_sound_set_maxamp)
XEN_NARGIFY_1(g_mus_sound_maxamp_exists_w, g_mus_sound_maxamp_exists)
XEN_NARGIFY_0(g_mus_audio_report_w, g_mus_audio_report)
XEN_NARGIFY_3(g_mus_sun_set_outputs_w, g_mus_sun_set_outputs)
XEN_NARGIFY_3(g_mus_netbsd_set_outputs_w, g_mus_netbsd_set_outputs)
XEN_NARGIFY_1(g_mus_sound_open_input_w, g_mus_sound_open_input)
XEN_NARGIFY_1(g_mus_sound_close_input_w, g_mus_sound_close_input)
XEN_NARGIFY_1(g_mus_audio_close_w, g_mus_audio_close)
XEN_NARGIFY_0(g_mus_audio_systems_w, g_mus_audio_systems)
XEN_NARGIFY_4(g_mus_audio_mixer_read_w, g_mus_audio_mixer_read)
XEN_NARGIFY_4(g_mus_audio_mixer_write_w, g_mus_audio_mixer_write)
XEN_NARGIFY_0(g_mus_clipping_w, g_mus_clipping)
XEN_NARGIFY_1(g_mus_set_clipping_w, g_mus_set_clipping)
XEN_NARGIFY_1(g_mus_file_clipping_w, g_mus_file_clipping)
XEN_NARGIFY_2(g_mus_file_set_clipping_w, g_mus_file_set_clipping)
XEN_NARGIFY_0(g_mus_prescaler_w, g_mus_prescaler)
XEN_NARGIFY_1(g_mus_set_prescaler_w, g_mus_set_prescaler)
XEN_NARGIFY_1(g_mus_file_prescaler_w, g_mus_file_prescaler)
XEN_NARGIFY_2(g_mus_file_set_prescaler_w, g_mus_file_set_prescaler)
XEN_NARGIFY_0(g_mus_header_raw_defaults_w, g_mus_header_raw_defaults)
XEN_NARGIFY_1(g_mus_header_set_raw_defaults_w, g_mus_header_set_raw_defaults)
XEN_NARGIFY_1(g_mus_expand_filename_w, g_mus_expand_filename)
XEN_NARGIFY_3(g_mus_audio_write_w, g_mus_audio_write)
XEN_NARGIFY_3(g_mus_audio_read_w, g_mus_audio_read)
XEN_ARGIFY_6(g_mus_sound_open_output_w, g_mus_sound_open_output)
XEN_ARGIFY_5(g_mus_sound_reopen_output_w, g_mus_sound_reopen_output)
XEN_NARGIFY_2(g_mus_sound_close_output_w, g_mus_sound_close_output)
XEN_NARGIFY_5(g_mus_sound_read_w, g_mus_sound_read)
XEN_NARGIFY_5(g_mus_sound_write_w, g_mus_sound_write)
XEN_NARGIFY_2(g_mus_sound_seek_frame_w, g_mus_sound_seek_frame)
XEN_NARGIFY_5(g_mus_audio_open_output_w, g_mus_audio_open_output)
XEN_NARGIFY_5(g_mus_audio_open_input_w, g_mus_audio_open_input)
XEN_ARGIFY_1(g_mus_sound_report_cache_w, g_mus_sound_report_cache)
XEN_NARGIFY_1(g_mus_sound_forget_w, g_mus_sound_forget)
XEN_NARGIFY_0(g_mus_sound_prune_w, g_mus_sound_prune)
XEN_NARGIFY_1(g_mus_error_type_to_string_w, g_mus_error_type_to_string)
XEN_NARGIFY_2(g_mus_oss_set_buffers_w, g_mus_oss_set_buffers)
XEN_NARGIFY_5(g_array_to_file_w, g_array_to_file)
XEN_NARGIFY_5(g_file_to_array_w, g_file_to_array)
XEN_NARGIFY_0(g_mus_alsa_buffers_w, g_mus_alsa_buffers)
XEN_NARGIFY_1(g_mus_alsa_set_buffers_w, g_mus_alsa_set_buffers)
XEN_NARGIFY_0(g_mus_alsa_buffer_size_w, g_mus_alsa_buffer_size)
XEN_NARGIFY_1(g_mus_alsa_set_buffer_size_w, g_mus_alsa_set_buffer_size)
XEN_NARGIFY_0(g_mus_alsa_device_w, g_mus_alsa_device)
XEN_NARGIFY_1(g_mus_alsa_set_device_w, g_mus_alsa_set_device)
XEN_NARGIFY_0(g_mus_alsa_playback_device_w, g_mus_alsa_playback_device)
XEN_NARGIFY_1(g_mus_alsa_set_playback_device_w, g_mus_alsa_set_playback_device)
XEN_NARGIFY_0(g_mus_alsa_capture_device_w, g_mus_alsa_capture_device)
XEN_NARGIFY_1(g_mus_alsa_set_capture_device_w, g_mus_alsa_set_capture_device)
XEN_NARGIFY_0(g_mus_alsa_squelch_warning_w, g_mus_alsa_squelch_warning)
XEN_NARGIFY_1(g_mus_alsa_set_squelch_warning_w, g_mus_alsa_set_squelch_warning)
#if HAVE_OSS
  XEN_NARGIFY_0(g_mus_audio_reinitialize_w, g_mus_audio_reinitialize)
#endif
#if MUS_DEBUGGING && HAVE_SCHEME
  XEN_NARGIFY_2(g_mus_header_original_format_name_w, g_mus_header_original_format_name)
#endif
#if HAVE_GAUCHE
XEN_NARGIFY_2(g_sound_data_equalp_w, equalp_sound_data)
#endif
#else
#define g_sound_data_length_w g_sound_data_length
#define g_sound_data_chans_w g_sound_data_chans
#define g_sound_data_copy_w g_sound_data_copy
#define g_sound_data_addB_w g_sound_data_addB
#define g_sound_data_add_w g_sound_data_add
#define g_sound_data_offsetB_w g_sound_data_offsetB
#define g_sound_data_multiplyB_w g_sound_data_multiplyB
#define g_sound_data_multiply_w g_sound_data_multiply
#define g_sound_data_ref_w g_sound_data_ref
#define g_sound_data_set_w g_sound_data_set
#define g_make_sound_data_w g_make_sound_data
#define g_sound_data_p_w g_sound_data_p
#define g_sound_data_maxamp_w g_sound_data_maxamp
#define g_sound_data_peak_w g_sound_data_peak
#define g_sound_data_scaleB_w g_sound_data_scaleB
#define g_sound_data_fillB_w g_sound_data_fillB
#define g_sound_data_reverseB_w g_sound_data_reverseB
#define g_sound_data_to_vct_w g_sound_data_to_vct
#define g_sound_data_to_sound_data_w g_sound_data_to_sound_data
#define g_vct_to_sound_data_w g_vct_to_sound_data
#define g_mus_sound_samples_w g_mus_sound_samples
#define g_mus_sound_set_samples_w g_mus_sound_set_samples
#define g_mus_sound_frames_w g_mus_sound_frames
#define g_mus_sound_duration_w g_mus_sound_duration
#define g_mus_sound_datum_size_w g_mus_sound_datum_size
#define g_mus_sound_data_location_w g_mus_sound_data_location
#define g_mus_sound_set_data_location_w g_mus_sound_set_data_location
#define g_mus_sound_chans_w g_mus_sound_chans
#define g_mus_sound_set_chans_w g_mus_sound_set_chans
#define g_mus_sound_srate_w g_mus_sound_srate
#define g_mus_sound_set_srate_w g_mus_sound_set_srate
#define g_mus_sound_header_type_w g_mus_sound_header_type
#define g_mus_sound_set_header_type_w g_mus_sound_set_header_type
#define g_mus_sound_data_format_w g_mus_sound_data_format
#define g_mus_sound_set_data_format_w g_mus_sound_set_data_format
#define g_mus_sound_length_w g_mus_sound_length
#define g_mus_sound_type_specifier_w g_mus_sound_type_specifier
#define g_mus_header_type_name_w g_mus_header_type_name
#define g_mus_data_format_name_w g_mus_data_format_name
#define g_mus_header_type_to_string_w g_mus_header_type_to_string
#define g_mus_data_format_to_string_w g_mus_data_format_to_string
#define g_mus_sound_comment_w g_mus_sound_comment
#define g_mus_sound_write_date_w g_mus_sound_write_date
#define g_mus_bytes_per_sample_w g_mus_bytes_per_sample
#define g_mus_sound_loop_info_w g_mus_sound_loop_info
#define g_mus_sound_maxamp_w g_mus_sound_maxamp
#define g_mus_sound_set_maxamp_w g_mus_sound_set_maxamp
#define g_mus_sound_maxamp_exists_w g_mus_sound_maxamp_exists
#define g_mus_audio_report_w g_mus_audio_report
#define g_mus_sun_set_outputs_w g_mus_sun_set_outputs
#define g_mus_netbsd_set_outputs_w g_mus_netbsd_set_outputs
#define g_mus_sound_open_input_w g_mus_sound_open_input
#define g_mus_sound_close_input_w g_mus_sound_close_input
#define g_mus_audio_close_w g_mus_audio_close
#define g_mus_audio_systems_w g_mus_audio_systems
#define g_mus_audio_mixer_read_w g_mus_audio_mixer_read
#define g_mus_audio_mixer_write_w g_mus_audio_mixer_write
#define g_mus_clipping_w g_mus_clipping
#define g_mus_set_clipping_w g_mus_set_clipping
#define g_mus_file_clipping_w g_mus_file_clipping
#define g_mus_file_set_clipping_w g_mus_file_set_clipping
#define g_mus_prescaler_w g_mus_prescaler
#define g_mus_set_prescaler_w g_mus_set_prescaler
#define g_mus_file_prescaler_w g_mus_file_prescaler
#define g_mus_file_set_prescaler_w g_mus_file_set_prescaler
#define g_mus_header_raw_defaults_w g_mus_header_raw_defaults
#define g_mus_header_set_raw_defaults_w g_mus_header_set_raw_defaults
#define g_mus_expand_filename_w g_mus_expand_filename
#define g_mus_audio_write_w g_mus_audio_write
#define g_mus_audio_read_w g_mus_audio_read
#define g_mus_sound_open_output_w g_mus_sound_open_output
#define g_mus_sound_reopen_output_w g_mus_sound_reopen_output
#define g_mus_sound_close_output_w g_mus_sound_close_output
#define g_mus_sound_read_w g_mus_sound_read
#define g_mus_sound_write_w g_mus_sound_write
#define g_mus_sound_seek_frame_w g_mus_sound_seek_frame
#define g_mus_audio_open_output_w g_mus_audio_open_output
#define g_mus_audio_open_input_w g_mus_audio_open_input
#define g_mus_sound_report_cache_w g_mus_sound_report_cache
#define g_mus_sound_forget_w g_mus_sound_forget
#define g_mus_sound_prune_w g_mus_sound_prune
#define g_mus_error_type_to_string_w g_mus_error_type_to_string
#define g_mus_oss_set_buffers_w g_mus_oss_set_buffers
#define g_array_to_file_w g_array_to_file
#define g_file_to_array_w g_file_to_array
#define g_mus_alsa_buffers_w g_mus_alsa_buffers
#define g_mus_alsa_set_buffers_w g_mus_alsa_set_buffers
#define g_mus_alsa_buffer_size_w g_mus_alsa_buffer_size
#define g_mus_alsa_set_buffer_size_w g_mus_alsa_set_buffer_size
#define g_mus_alsa_device_w g_mus_alsa_device
#define g_mus_alsa_set_device_w g_mus_alsa_set_device
#define g_mus_alsa_playback_device_w g_mus_alsa_playback_device
#define g_mus_alsa_set_playback_device_w g_mus_alsa_set_playback_device
#define g_mus_alsa_capture_device_w g_mus_alsa_capture_device
#define g_mus_alsa_set_capture_device_w g_mus_alsa_set_capture_device
#define g_mus_alsa_squelch_warning_w g_mus_alsa_squelch_warning
#define g_mus_alsa_set_squelch_warning_w g_mus_alsa_set_squelch_warning
#if HAVE_OSS
  #define g_mus_audio_reinitialize_w g_mus_audio_reinitialize
#endif
#if MUS_DEBUGGING && HAVE_SCHEME
#define g_mus_header_original_format_name_w g_mus_header_original_format_name
#endif
#endif

#if WITH_MODULES
static void sndlib2xen_init(void *ignore)
#else
void mus_sndlib_xen_initialize(void)
#endif
{
  mus_sound_initialize();

#if (!HAVE_GAUCHE)
  sound_data_tag = XEN_MAKE_OBJECT_TYPE("SoundData", sizeof(sound_data));
#else
  sound_data_tag = XEN_MAKE_OBJECT_TYPE("<sound-data>", sizeof(sound_data), print_sound_data, free_sound_data);
  XEN_EVAL_C_STRING("(define-method object-apply ((sd <sound-data>) (c <integer>) (i <integer>)) (sound-data-ref sd c i))");
  XEN_EVAL_C_STRING("(define-method (setter object-apply) ((sd <sound-data>) (c <integer>) (i <integer>) (val <number>)) (sound-data-set! sd c i val))");
  XEN_EVAL_C_STRING("(define-method equal? ((sd1 <sound-data>) (sd2 <sound-data>)) (sound-data-equal? sd1 sd2))");
  XEN_DEFINE_PROCEDURE("sound-data-equal?", g_sound_data_equalp_w, 2, 0, 0, "internal function for aound-data equal?");
#endif

#if HAVE_GUILE
  scm_set_smob_print(sound_data_tag, print_sound_data);
  scm_set_smob_free(sound_data_tag, free_sound_data);
  scm_set_smob_equalp(sound_data_tag, equalp_sound_data);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(sound_data_tag, XEN_PROCEDURE_CAST sound_data_apply, 2, 0, 0);
#endif
#endif

#if HAVE_FORTH
  fth_set_object_inspect(sound_data_tag, print_sound_data);
  fth_set_object_equal(sound_data_tag, equalp_sound_data);
  fth_set_object_to_array(sound_data_tag, g_sound_data_to_vector);
  fth_set_object_length(sound_data_tag, g_sound_data_length);
  fth_set_object_free(sound_data_tag, free_sound_data);
  fth_set_object_apply(sound_data_tag, XEN_PROCEDURE_CAST sound_data_apply, 2, 0, 0);
#endif

#if HAVE_RUBY
  Init_Hook();
  rb_include_module(sound_data_tag, rb_mComparable);
  rb_include_module(sound_data_tag, rb_mEnumerable);
  rb_define_method(sound_data_tag, "to_s",   XEN_PROCEDURE_CAST print_sound_data,     0);
  rb_define_method(sound_data_tag, "eql?",   XEN_PROCEDURE_CAST equalp_sound_data,    1);
  rb_define_method(sound_data_tag, "==",     XEN_PROCEDURE_CAST equalp_sound_data,    1);
  rb_define_method(sound_data_tag, "each",   XEN_PROCEDURE_CAST sound_data_each,      0);
  rb_define_method(sound_data_tag, "<=>",    XEN_PROCEDURE_CAST sound_data_compare,   1);
  rb_define_method(sound_data_tag, "[]",     XEN_PROCEDURE_CAST g_sound_data_ref,     2);
  rb_define_method(sound_data_tag, "[]=",    XEN_PROCEDURE_CAST g_sound_data_set,     3);
  rb_define_method(sound_data_tag, "length", XEN_PROCEDURE_CAST sound_data_size,      0);
  rb_define_method(sound_data_tag, "size",   XEN_PROCEDURE_CAST sound_data_size,      0);
  rb_define_method(sound_data_tag, "fill",   XEN_PROCEDURE_CAST g_rb_sound_data_fill, 1);
  rb_define_method(sound_data_tag, "dup",    XEN_PROCEDURE_CAST g_sound_data_copy,    0);
  rb_define_method(sound_data_tag, "chans",  XEN_PROCEDURE_CAST sound_data_chans,     0);
  rb_define_method(sound_data_tag, "peak",   XEN_PROCEDURE_CAST g_sound_data_peak,    0);
  rb_define_method(sound_data_tag, "offset!",   XEN_PROCEDURE_CAST g_sound_data_offsetB,   1);
  rb_define_method(sound_data_tag, "multiply!", XEN_PROCEDURE_CAST g_sound_data_multiplyB, 1);

  rb_define_method(sound_data_tag, "add!",      XEN_PROCEDURE_CAST g_sound_data_addB,      1);
  rb_define_method(sound_data_tag, "scale!",    XEN_PROCEDURE_CAST g_sound_data_scaleB,    1);
  rb_define_method(sound_data_tag, "reverse!",  XEN_PROCEDURE_CAST g_sound_data_reverseB,  0);

  rb_define_singleton_method(sound_data_tag, "new", XEN_PROCEDURE_CAST g_rb_make_sound_data, 2);
#endif

  XEN_DEFINE_CONSTANT(S_mus_out_format,           MUS_OUT_FORMAT,           "sample format for fastest IO");
  XEN_DEFINE_CONSTANT(S_mus_unsupported,          MUS_UNSUPPORTED,          "unsupported header id");
  XEN_DEFINE_CONSTANT(S_mus_next,                 MUS_NEXT,                 "NeXT (Sun) sound header id");
  XEN_DEFINE_CONSTANT(S_mus_aifc,                 MUS_AIFC,                 "AIFC sound header id");
  XEN_DEFINE_CONSTANT(S_mus_rf64,                 MUS_RF64,                 "RF64 sound header id");
  XEN_DEFINE_CONSTANT(S_mus_riff,                 MUS_RIFF,                 "RIFF (MS wave) sound header id");
  XEN_DEFINE_CONSTANT(S_mus_nist,                 MUS_NIST,                 "NIST (Sphere) sound header id");
  XEN_DEFINE_CONSTANT(S_mus_raw,                  MUS_RAW,                  "raw (headerless) sound header id");
  XEN_DEFINE_CONSTANT(S_mus_ircam,                MUS_IRCAM,                "IRCAM sound header id");
  XEN_DEFINE_CONSTANT(S_mus_aiff,                 MUS_AIFF,                 "AIFF (old-style) sound header id");
  XEN_DEFINE_CONSTANT(S_mus_bicsf,                MUS_BICSF,                "BICSF header id");
  XEN_DEFINE_CONSTANT(S_mus_voc,                  MUS_VOC,                  "VOC header id");
  XEN_DEFINE_CONSTANT(S_mus_svx,                  MUS_SVX,                  "SVX (IFF) header id");
  XEN_DEFINE_CONSTANT(S_mus_soundfont,            MUS_SOUNDFONT,            "soundfont header id");

  XEN_DEFINE_CONSTANT(S_mus_unknown,              MUS_UNKNOWN,              "unknown data format");
  XEN_DEFINE_CONSTANT(S_mus_bshort,               MUS_BSHORT,               "big-endian short data format id");
  XEN_DEFINE_CONSTANT(S_mus_lshort,               MUS_LSHORT,               "little-endian short data format id");
  XEN_DEFINE_CONSTANT(S_mus_mulaw,                MUS_MULAW,                "mulaw (8-bit) data format id");
  XEN_DEFINE_CONSTANT(S_mus_alaw,                 MUS_ALAW,                 "alaw (8-bit) data format id");
  XEN_DEFINE_CONSTANT(S_mus_byte,                 MUS_BYTE,                 "signed byte data format id");
  XEN_DEFINE_CONSTANT(S_mus_ubyte,                MUS_UBYTE,                "unsigned byte data format id");
  XEN_DEFINE_CONSTANT(S_mus_bfloat,               MUS_BFLOAT,               "big-endian float data format id");
  XEN_DEFINE_CONSTANT(S_mus_lfloat,               MUS_LFLOAT,               "little-endian float data format id");
  XEN_DEFINE_CONSTANT(S_mus_bint,                 MUS_BINT,                 "big-endian int data format id");
  XEN_DEFINE_CONSTANT(S_mus_lint,                 MUS_LINT,                 "little-endian int data format id");
  XEN_DEFINE_CONSTANT(S_mus_bintn,                MUS_BINTN,                "normalized big-endian int data format id");
  XEN_DEFINE_CONSTANT(S_mus_lintn,                MUS_LINTN,                "normalized little-endian int data format id");
  XEN_DEFINE_CONSTANT(S_mus_b24int,               MUS_B24INT,               "big-endian 24-bit data format id");
  XEN_DEFINE_CONSTANT(S_mus_l24int,               MUS_L24INT,               "little-endian 24-bit data format id");
  XEN_DEFINE_CONSTANT(S_mus_bdouble,              MUS_BDOUBLE,              "big-endian double data format id");
  XEN_DEFINE_CONSTANT(S_mus_ldouble,              MUS_LDOUBLE,              "little-endian double data format id");
  XEN_DEFINE_CONSTANT(S_mus_ubshort,              MUS_UBSHORT,              "unsigned big-endian short data format id");
  XEN_DEFINE_CONSTANT(S_mus_ulshort,              MUS_ULSHORT,              "unsigned little-endian short data format id");
  XEN_DEFINE_CONSTANT(S_mus_bdouble_unscaled,     MUS_BDOUBLE_UNSCALED,     "unscaled big-endian double data format id");
  XEN_DEFINE_CONSTANT(S_mus_ldouble_unscaled,     MUS_LDOUBLE_UNSCALED,     "unscaled little-endian double data format id");
  XEN_DEFINE_CONSTANT(S_mus_bfloat_unscaled,      MUS_BFLOAT_UNSCALED,      "unscaled big-endian float data format id");
  XEN_DEFINE_CONSTANT(S_mus_lfloat_unscaled,      MUS_LFLOAT_UNSCALED,      "unscaled little-endian float data format id");

  XEN_DEFINE_CONSTANT(S_mus_audio_default,        MUS_AUDIO_DEFAULT,        "default audio device");
  XEN_DEFINE_CONSTANT(S_mus_audio_duplex_default, MUS_AUDIO_DUPLEX_DEFAULT, "default duplex device");
  XEN_DEFINE_CONSTANT(S_mus_audio_line_out,       MUS_AUDIO_LINE_OUT,       "audio line-out device");
  XEN_DEFINE_CONSTANT(S_mus_audio_line_in,        MUS_AUDIO_LINE_IN,        "audio line-in device");
  XEN_DEFINE_CONSTANT(S_mus_audio_microphone,     MUS_AUDIO_MICROPHONE,     "microphone device");
  XEN_DEFINE_CONSTANT(S_mus_audio_speakers,       MUS_AUDIO_SPEAKERS,       "speakers device (a mixer kludge)");
  XEN_DEFINE_CONSTANT(S_mus_audio_dac_out,        MUS_AUDIO_DAC_OUT,        "DAC out device");
  XEN_DEFINE_CONSTANT(S_mus_audio_adat_in,        MUS_AUDIO_ADAT_IN,        "ADAT in device");
  XEN_DEFINE_CONSTANT(S_mus_audio_aes_in,         MUS_AUDIO_AES_IN,         "AES in device");
  XEN_DEFINE_CONSTANT(S_mus_audio_digital_in,     MUS_AUDIO_DIGITAL_IN,     "digital audio in device");
  XEN_DEFINE_CONSTANT(S_mus_audio_digital_out,    MUS_AUDIO_DIGITAL_OUT,    "digital audio out device");
  XEN_DEFINE_CONSTANT(S_mus_audio_adat_out,       MUS_AUDIO_ADAT_OUT,       "ADAT out device");
  XEN_DEFINE_CONSTANT(S_mus_audio_aes_out,        MUS_AUDIO_AES_OUT,        "AES out device");
  XEN_DEFINE_CONSTANT(S_mus_audio_dac_filter,     MUS_AUDIO_DAC_FILTER,     "DAC filter 'device' (a mixer kludge)");
  XEN_DEFINE_CONSTANT(S_mus_audio_mixer,          MUS_AUDIO_MIXER,          "the 'mixer' device");
  XEN_DEFINE_CONSTANT(S_mus_audio_line1,          MUS_AUDIO_LINE1,          "audio line 1 device");
  XEN_DEFINE_CONSTANT(S_mus_audio_line2,          MUS_AUDIO_LINE2,          "audio line 2 device");
  XEN_DEFINE_CONSTANT(S_mus_audio_line3,          MUS_AUDIO_LINE3,          "audio line 3 device");
  XEN_DEFINE_CONSTANT(S_mus_audio_aux_input,      MUS_AUDIO_AUX_INPUT,      "aux audio in device");
  XEN_DEFINE_CONSTANT(S_mus_audio_cd,             MUS_AUDIO_CD,             "CD in device");
  XEN_DEFINE_CONSTANT(S_mus_audio_aux_output,     MUS_AUDIO_AUX_OUTPUT,     "aux audio out device");
  XEN_DEFINE_CONSTANT(S_mus_audio_spdif_in,       MUS_AUDIO_SPDIF_IN,       "SPDIF in device");
  XEN_DEFINE_CONSTANT(S_mus_audio_spdif_out,      MUS_AUDIO_SPDIF_OUT,      "SPDIF out device");
  XEN_DEFINE_CONSTANT(S_mus_audio_direction,      MUS_AUDIO_DIRECTION,      "audio sample flow direction (" S_mus_audio_read ")");
  XEN_DEFINE_CONSTANT(S_mus_audio_samples_per_channel, MUS_AUDIO_SAMPLES_PER_CHANNEL, "samples per channel (" S_mus_audio_read ")");

  XEN_DEFINE_CONSTANT(S_mus_audio_amp,            MUS_AUDIO_AMP,            "mixer amp field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_srate,          MUS_AUDIO_SRATE,          "mixer srate field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_channel,        MUS_AUDIO_CHANNEL, "       mixer channel field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_format,         MUS_AUDIO_FORMAT,         "mixer data format field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_port,           MUS_AUDIO_PORT,           "mixer port");
  XEN_DEFINE_CONSTANT(S_mus_audio_imix,           MUS_AUDIO_IMIX,           "mixer 'imix' field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_igain,          MUS_AUDIO_IGAIN,          "mixer 'igain' field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_reclev,         MUS_AUDIO_RECLEV,         "mixer 'reclev' field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_pcm,            MUS_AUDIO_PCM,            "mixer 'pcm' field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_pcm2,           MUS_AUDIO_PCM2,           "mixer 'pcm2' field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_ogain,          MUS_AUDIO_OGAIN,          "mixer 'ogain' field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_line,           MUS_AUDIO_LINE,           "mixer 'line' field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_synth,          MUS_AUDIO_SYNTH,          "mixer 'synth' field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_bass,           MUS_AUDIO_BASS,           "mixer 'bass' field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_treble,         MUS_AUDIO_TREBLE,         "mixer 'treble' field id");

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_sound_samples, g_mus_sound_samples_w, H_mus_sound_samples, 
				   S_setB S_mus_sound_samples, g_mus_sound_set_samples_w, 1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_sound_data_location, g_mus_sound_data_location_w, H_mus_sound_data_location,
				   S_setB S_mus_sound_data_location, g_mus_sound_set_data_location_w, 1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_sound_chans, g_mus_sound_chans_w, H_mus_sound_chans,
				   S_setB S_mus_sound_chans, g_mus_sound_set_chans_w, 1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_sound_srate, g_mus_sound_srate_w, H_mus_sound_srate,
				   S_setB S_mus_sound_srate, g_mus_sound_set_srate_w, 1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_sound_header_type, g_mus_sound_header_type_w, H_mus_sound_header_type,
				   S_setB S_mus_sound_header_type, g_mus_sound_set_header_type_w, 1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_sound_data_format, g_mus_sound_data_format_w, H_mus_sound_data_format,
				   S_setB S_mus_sound_data_format, g_mus_sound_set_data_format_w, 1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE(S_sound_data_length,        g_sound_data_length_w,          1, 0, 0, H_sound_data_length);
  XEN_DEFINE_PROCEDURE(S_sound_data_chans,         g_sound_data_chans_w,           1, 0, 0, H_sound_data_chans);
  XEN_DEFINE_PROCEDURE(S_sound_data_copy,          g_sound_data_copy_w,            1, 0, 0, H_sound_data_copy);
  XEN_DEFINE_PROCEDURE(S_sound_data_addB,          g_sound_data_addB_w,            2, 0, 0, H_sound_data_addB);
  XEN_DEFINE_PROCEDURE(S_sound_data_add,           g_sound_data_add_w,             2, 0, 0, H_sound_data_add);
  XEN_DEFINE_PROCEDURE(S_sound_data_offsetB,       g_sound_data_offsetB_w,         2, 0, 0, H_sound_data_offsetB);
  XEN_DEFINE_PROCEDURE(S_sound_data_multiplyB,     g_sound_data_multiplyB_w,       2, 0, 0, H_sound_data_multiplyB);
  XEN_DEFINE_PROCEDURE(S_sound_data_multiply,      g_sound_data_multiply_w,        2, 0, 0, H_sound_data_multiply);
  XEN_DEFINE_PROCEDURE(S_make_sound_data,          g_make_sound_data_w,            2, 0, 0, H_make_sound_data);
  XEN_DEFINE_PROCEDURE(S_sound_data_p,             g_sound_data_p_w,               1, 0, 0, H_sound_data_p);
  XEN_DEFINE_PROCEDURE(S_sound_data_maxamp,        g_sound_data_maxamp_w,          1, 0, 0, H_sound_data_maxamp);
  XEN_DEFINE_PROCEDURE(S_sound_data_peak,          g_sound_data_peak_w,            1, 0, 0, H_sound_data_peak);
  XEN_DEFINE_PROCEDURE(S_sound_data_setB,          g_sound_data_set_w,             4, 0, 0, H_sound_data_setB);
  XEN_DEFINE_PROCEDURE(S_sound_data_scaleB,        g_sound_data_scaleB_w,          2, 0, 0, H_sound_data_scaleB);
  XEN_DEFINE_PROCEDURE(S_sound_data_fillB,         g_sound_data_fillB_w,           2, 0, 0, H_sound_data_fillB);
  XEN_DEFINE_PROCEDURE(S_sound_data_reverseB,      g_sound_data_reverseB_w,        1, 0, 0, H_sound_data_reverseB);
  XEN_DEFINE_PROCEDURE(S_sound_data_to_vct,        g_sound_data_to_vct_w,          1, 2, 0, H_sound_data_to_vct);
  XEN_DEFINE_PROCEDURE(S_sound_data_to_sound_data, g_sound_data_to_sound_data_w,   5, 0, 0, H_sound_data_to_sound_data);
  XEN_DEFINE_PROCEDURE(S_vct_to_sound_data,        g_vct_to_sound_data_w,          1, 2, 0, H_vct_to_sound_data);

  XEN_DEFINE_PROCEDURE(S_mus_sound_frames,         g_mus_sound_frames_w,           1, 0, 0, H_mus_sound_frames);
  XEN_DEFINE_PROCEDURE(S_mus_sound_duration,       g_mus_sound_duration_w,         1, 0, 0, H_mus_sound_duration);
  XEN_DEFINE_PROCEDURE(S_mus_sound_datum_size,     g_mus_sound_datum_size_w,       1, 0, 0, H_mus_sound_datum_size);
  XEN_DEFINE_PROCEDURE(S_mus_sound_length,         g_mus_sound_length_w,           1, 0, 0, H_mus_sound_length);
  XEN_DEFINE_PROCEDURE(S_mus_sound_type_specifier, g_mus_sound_type_specifier_w,   1, 0, 0, H_mus_sound_type_specifier);
  XEN_DEFINE_PROCEDURE(S_mus_header_type_name,     g_mus_header_type_name_w,       1, 0, 0, H_mus_header_type_name);
  XEN_DEFINE_PROCEDURE(S_mus_header_type_to_string,g_mus_header_type_to_string_w,  1, 0, 0, H_mus_header_type_to_string);
  XEN_DEFINE_PROCEDURE(S_mus_data_format_name,     g_mus_data_format_name_w,       1, 0, 0, H_mus_data_format_name);
  XEN_DEFINE_PROCEDURE(S_mus_data_format_to_string,g_mus_data_format_to_string_w,  1, 0, 0, H_mus_data_format_to_string);
  XEN_DEFINE_PROCEDURE(S_mus_sound_comment,        g_mus_sound_comment_w,          1, 0, 0, H_mus_sound_comment);
  XEN_DEFINE_PROCEDURE(S_mus_sound_write_date,     g_mus_sound_write_date_w,       1, 0, 0, H_mus_sound_write_date);
  XEN_DEFINE_PROCEDURE(S_mus_bytes_per_sample,     g_mus_bytes_per_sample_w,       1, 0, 0, H_mus_bytes_per_sample);
  XEN_DEFINE_PROCEDURE(S_mus_sound_loop_info,      g_mus_sound_loop_info_w,        1, 0, 0, H_mus_sound_loop_info);
  XEN_DEFINE_PROCEDURE(S_mus_sound_maxamp_exists,  g_mus_sound_maxamp_exists_w,    1, 0, 0, H_mus_sound_maxamp_exists);
  XEN_DEFINE_PROCEDURE(S_mus_sound_forget,         g_mus_sound_forget_w,           1, 0, 0, H_mus_sound_forget);
  XEN_DEFINE_PROCEDURE(S_mus_sound_prune,          g_mus_sound_prune_w,            0, 0, 0, H_mus_sound_prune);
  XEN_DEFINE_PROCEDURE(S_mus_audio_report,         g_mus_audio_report_w,           0, 0, 0, H_mus_audio_report);
  XEN_DEFINE_PROCEDURE(S_mus_sun_set_outputs,      g_mus_sun_set_outputs_w,        3, 0, 0, H_mus_sun_set_outputs);
  XEN_DEFINE_PROCEDURE(S_mus_netbsd_set_outputs,   g_mus_netbsd_set_outputs_w,     3, 0, 0, H_mus_netbsd_set_outputs);
  XEN_DEFINE_PROCEDURE(S_mus_sound_open_input,     g_mus_sound_open_input_w,       1, 0, 0, H_mus_sound_open_input);
  XEN_DEFINE_PROCEDURE(S_mus_sound_close_input,    g_mus_sound_close_input_w,      1, 0, 0, H_mus_sound_close_input);
  XEN_DEFINE_PROCEDURE(S_mus_audio_close,          g_mus_audio_close_w,            1, 0, 0, H_mus_audio_close);
  XEN_DEFINE_PROCEDURE(S_mus_audio_systems,        g_mus_audio_systems_w,          0, 0, 0, H_mus_audio_systems);
  XEN_DEFINE_PROCEDURE(S_mus_audio_mixer_read,     g_mus_audio_mixer_read_w,       4, 0, 0, H_mus_audio_mixer_read);
  XEN_DEFINE_PROCEDURE(S_mus_audio_mixer_write,    g_mus_audio_mixer_write_w,      4, 0, 0, H_mus_audio_mixer_write);
  XEN_DEFINE_PROCEDURE(S_mus_expand_filename,      g_mus_expand_filename_w,        1, 0, 0, H_mus_expand_filename);
  XEN_DEFINE_PROCEDURE(S_mus_audio_write,          g_mus_audio_write_w,            3, 0, 0, H_mus_audio_write);
  XEN_DEFINE_PROCEDURE(S_mus_audio_read,           g_mus_audio_read_w,             3, 0, 0, H_mus_audio_read);
  XEN_DEFINE_PROCEDURE(S_mus_sound_open_output,    g_mus_sound_open_output_w,      1, 5, 0, H_mus_sound_open_output);
  XEN_DEFINE_PROCEDURE(S_mus_sound_reopen_output,  g_mus_sound_reopen_output_w,    1, 4, 0, H_mus_sound_reopen_output);
  XEN_DEFINE_PROCEDURE(S_mus_sound_close_output,   g_mus_sound_close_output_w,     2, 0, 0, H_mus_sound_close_output);
  XEN_DEFINE_PROCEDURE(S_mus_sound_read,           g_mus_sound_read_w,             5, 0, 0, H_mus_sound_read);
  XEN_DEFINE_PROCEDURE(S_mus_sound_write,          g_mus_sound_write_w,            5, 0, 0, H_mus_sound_write);
  XEN_DEFINE_PROCEDURE(S_mus_sound_seek_frame,     g_mus_sound_seek_frame_w,       2, 0, 0, H_mus_sound_seek_frame);
  XEN_DEFINE_PROCEDURE(S_mus_audio_open_output,    g_mus_audio_open_output_w,      5, 0, 0, H_mus_audio_open_output);
  XEN_DEFINE_PROCEDURE(S_mus_audio_open_input,     g_mus_audio_open_input_w,       5, 0, 0, H_mus_audio_open_input);
  XEN_DEFINE_PROCEDURE(S_mus_sound_report_cache,   g_mus_sound_report_cache_w,     0, 1, 0, H_mus_sound_report_cache);
  XEN_DEFINE_PROCEDURE(S_mus_error_type_to_string, g_mus_error_type_to_string_w,   1, 0, 0, H_mus_error_type_to_string);
  XEN_DEFINE_PROCEDURE(S_mus_oss_set_buffers,      g_mus_oss_set_buffers_w,        2, 0, 0, H_mus_oss_set_buffers);
  XEN_DEFINE_PROCEDURE(S_array_to_file,            g_array_to_file_w,              5, 0, 0, H_array_to_file);
  XEN_DEFINE_PROCEDURE(S_file_to_array,            g_file_to_array_w,              5, 0, 0, H_file_to_array);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_header_raw_defaults, g_mus_header_raw_defaults_w, H_mus_header_raw_defaults,
				   S_setB S_mus_header_raw_defaults, g_mus_header_set_raw_defaults_w, 0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_prescaler, g_mus_prescaler_w, H_mus_prescaler,
				   S_setB S_mus_prescaler, g_mus_set_prescaler_w, 0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_file_prescaler, g_mus_file_prescaler_w, H_mus_file_prescaler,
				   S_setB S_mus_file_prescaler, g_mus_file_set_prescaler_w, 1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_clipping, g_mus_clipping_w, H_mus_clipping,
				   S_setB S_mus_clipping, g_mus_set_clipping_w, 0, 0, 1, 0);
#if (!SNDLIB_DISABLE_DEPRECATED)
  XEN_DEFINE_PROCEDURE_WITH_SETTER("mus-file-data-clipped", g_mus_clipping_w, H_mus_clipping,
				   S_setB "mus-file-data-clipped", g_mus_set_clipping_w, 0, 0, 1, 0);
#endif
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_file_clipping, g_mus_file_clipping_w, H_mus_file_clipping,
				   S_setB S_mus_file_clipping, g_mus_file_set_clipping_w, 1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_sound_data_ref, g_sound_data_ref_w, H_sound_data_ref,
				   S_setB S_sound_data_ref, g_sound_data_set_w,  3, 0, 4, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_sound_maxamp, g_mus_sound_maxamp_w, H_mus_sound_maxamp,
				   S_setB S_mus_sound_maxamp, g_mus_sound_set_maxamp_w, 1, 0, 2, 0);

  /* these are no-ops if not ALSA, but that makes it easier to maintain global initialization files */
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_alsa_buffers, g_mus_alsa_buffers_w, H_mus_alsa_buffers,
				   S_setB S_mus_alsa_buffers, g_mus_alsa_set_buffers_w, 0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_alsa_buffer_size, g_mus_alsa_buffer_size_w, H_mus_alsa_buffer_size,
				   S_setB S_mus_alsa_buffer_size, g_mus_alsa_set_buffer_size_w, 0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_alsa_device, g_mus_alsa_device_w, H_mus_alsa_device,
				   S_setB S_mus_alsa_device, g_mus_alsa_set_device_w, 0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_alsa_playback_device, g_mus_alsa_playback_device_w, H_mus_alsa_playback_device,
				   S_setB S_mus_alsa_playback_device, g_mus_alsa_set_playback_device_w, 0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_alsa_capture_device, g_mus_alsa_capture_device_w, H_mus_alsa_capture_device,
				   S_setB S_mus_alsa_capture_device, g_mus_alsa_set_capture_device_w, 0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_alsa_squelch_warning, g_mus_alsa_squelch_warning_w, H_mus_alsa_squelch_warning,
				   S_setB S_mus_alsa_squelch_warning, g_mus_alsa_set_squelch_warning_w, 0, 0, 1, 0);


#if HAVE_OSS
  XEN_DEFINE_PROCEDURE(S_mus_audio_reinitialize,   g_mus_audio_reinitialize_w, 0, 0, 0,  H_mus_audio_reinitialize);
#endif

#if MUS_DEBUGGING && HAVE_SCHEME
  XEN_DEFINE_PROCEDURE("mus-header-original-format-name", g_mus_header_original_format_name_w, 2, 0, 0, "internal testing function");
#endif

#if HAVE_FORTH
  XEN_DEFINE_PROCEDURE(S_sound_data_to_vector, g_sound_data_to_vector, 1, 0, 0, H_sound_data_to_vector);
#endif

  #define H_new_sound_hook S_new_sound_hook "(filename): called when a new sound file is being created"
  new_sound_hook = XEN_DEFINE_HOOK(S_new_sound_hook, 1, H_new_sound_hook);    /* arg = filename */
  mus_header_write_set_hook(g_new_sound_hook);

  XEN_YES_WE_HAVE("sndlib");

#if WITH_MODULES
  scm_c_export(S_array_to_file,
	       S_file_to_array,
	       S_make_sound_data,
	       S_mus_aifc,
	       S_mus_aiff,
	       S_mus_alaw,
	       S_mus_alsa_buffer_size,
	       S_mus_alsa_buffers,
	       S_mus_alsa_capture_device,
	       S_mus_alsa_device,
	       S_mus_alsa_playback_device,
	       S_mus_alsa_squelch_warning,
	       S_mus_audio_adat_in,
	       S_mus_audio_adat_out,
	       S_mus_audio_aes_in,
	       S_mus_audio_aes_out,
	       S_mus_audio_amp,
	       S_mus_audio_aux_input,
	       S_mus_audio_aux_output,
	       S_mus_audio_bass,
	       S_mus_audio_cd,
	       S_mus_audio_channel,
	       S_mus_audio_close,
	       S_mus_audio_dac_filter,
	       S_mus_audio_dac_out,
	       S_mus_audio_default,
	       S_mus_audio_digital_in,
	       S_mus_audio_digital_out,
	       S_mus_audio_direction,
	       S_mus_audio_duplex_default,
	       S_mus_audio_format,
	       S_mus_audio_igain,
	       S_mus_audio_imix,
	       S_mus_audio_line,
	       S_mus_audio_line1,
	       S_mus_audio_line2,
	       S_mus_audio_line3,
	       S_mus_audio_line_in,
	       S_mus_audio_line_out,
	       S_mus_audio_microphone,
	       S_mus_audio_mixer,
	       S_mus_audio_mixer_read,
	       S_mus_audio_mixer_write,
	       S_mus_audio_ogain,
	       S_mus_audio_open_input,
	       S_mus_audio_open_output,
	       S_mus_audio_pcm,
	       S_mus_audio_pcm2,
	       S_mus_audio_port,
	       S_mus_audio_read,
	       S_mus_audio_reclev,
#if HAVE_OSS
	       S_mus_audio_reinitialize,
#endif
	       S_mus_audio_report,
	       S_mus_audio_samples_per_channel,
	       S_mus_audio_spdif_in,
	       S_mus_audio_spdif_out,
	       S_mus_audio_speakers,
	       S_mus_audio_srate,
	       S_mus_audio_synth,
	       S_mus_audio_systems,
	       S_mus_audio_treble,
	       S_mus_audio_write,
	       S_mus_b24int,
	       S_mus_bdouble,
	       S_mus_bdouble_unscaled,
	       S_mus_bfloat,
	       S_mus_bfloat_unscaled,
	       S_mus_bicsf,
	       S_mus_bint,
	       S_mus_bintn,
	       S_mus_bshort,
	       S_mus_byte,
	       S_mus_bytes_per_sample,
	       S_mus_data_format_name,
	       S_mus_data_format_to_string,
	       S_mus_error_type_to_string,
	       S_mus_expand_filename,
	       S_mus_clipping,
	       S_mus_file_clipping,
	       S_mus_prescaler,
	       S_mus_file_prescaler,
	       S_mus_header_raw_defaults,
	       S_mus_header_type_name,
	       S_mus_header_type_to_string,
	       S_mus_ircam,
	       S_mus_l24int,
	       S_mus_ldouble,
	       S_mus_ldouble_unscaled,
	       S_mus_lfloat,
	       S_mus_lfloat_unscaled,
	       S_mus_lint,
	       S_mus_lintn,
	       S_mus_lshort,
	       S_mus_mulaw,
	       S_mus_netbsd_set_outputs,
	       S_mus_next,
	       S_mus_nist,
	       S_mus_oss_set_buffers,
	       S_mus_out_format,
	       S_mus_raw,
	       S_mus_rf64,
	       S_mus_riff,
	       S_mus_sound_chans,
	       S_mus_sound_close_input,
	       S_mus_sound_close_output,
	       S_mus_sound_comment,
	       S_mus_sound_data_format,
	       S_mus_sound_data_location,
	       S_mus_sound_datum_size,
	       S_mus_sound_duration,
	       S_mus_sound_forget,
	       S_mus_sound_frames,
	       S_mus_sound_header_type,
	       S_mus_sound_length,
	       S_mus_sound_loop_info,
	       S_mus_sound_maxamp,
	       S_mus_sound_maxamp_exists,
	       S_mus_sound_open_input,
	       S_mus_sound_open_output,
	       S_mus_sound_prune,
	       S_mus_sound_read,
	       S_mus_sound_reopen_output,
	       S_mus_sound_report_cache,
	       S_mus_sound_samples,
	       S_mus_sound_seek_frame,
	       S_mus_sound_srate,
	       S_mus_sound_type_specifier,
	       S_mus_sound_write,
	       S_mus_sound_write_date,
	       S_mus_soundfont,
	       S_mus_sun_set_outputs,
	       S_mus_svx,
	       S_mus_ubshort,
	       S_mus_ubyte,
	       S_mus_ulshort,
	       S_mus_unknown,
	       S_mus_unsupported,
	       S_mus_voc,
	       S_new_sound_hook,
	       S_sound_data_to_vct,
	       S_sound_data_chans,
	       S_sound_data_copy,
	       S_sound_data_addB,
	       S_sound_data_add,
	       S_sound_data_offsetB,
	       S_sound_data_multiplyB,
	       S_sound_data_multiply,
	       S_sound_data_length,
	       S_sound_data_maxamp,
	       S_sound_data_p,
	       S_sound_data_peak,
	       S_sound_data_ref,
	       S_sound_data_scaleB,
	       S_sound_data_fillB,
	       S_sound_data_reverseB,
	       S_sound_data_setB,
	       S_sound_data_to_sound_data,
	       S_vct_to_sound_data,
	       NULL);
#endif
}

#if WITH_MODULES
void mus_sndlib_xen_initialize(void)
{
  scm_c_define_module("snd sndlib", sndlib2xen_init, NULL);
}
#endif
