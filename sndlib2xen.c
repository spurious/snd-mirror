/* Tie sndlib into Xen */

#if defined(HAVE_CONFIG_H)
  #include <config.h>
#endif

#if USE_SND
  #include "snd.h"
#else
  #define PRINT_BUFFER_SIZE 512
  #define LABEL_BUFFER_SIZE 64
#endif

#include <stddef.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>

#include "sndlib.h"
#include "sndlib-strings.h"
#include "sndlib2xen.h"
#include "vct.h"

void mus_misc_error(const char *caller, char *msg, XEN val)
{
  if (msg)
    XEN_ERROR(MUS_MISC_ERROR,
	      XEN_LIST_3(C_TO_XEN_STRING(caller),
			 C_TO_XEN_STRING(msg),
			 val));
  else
    XEN_ERROR(MUS_MISC_ERROR,
	      XEN_LIST_2(C_TO_XEN_STRING(caller),
			 val));
}

static XEN g_sound_loop_info(XEN filename)
{
  #define H_mus_sound_loop_info "(" S_mus_sound_loop_info " filename) -> loop info for sound as a list (start1 end1 start2 end2 base-note base-detune mode1 mode2)"
  int *res;
  char *tmpstr;
  XEN sres = XEN_EMPTY_LIST;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ONLY_ARG, S_mus_sound_loop_info, "a string"); 
  tmpstr = mus_expand_filename(XEN_TO_C_STRING(filename));
  res = mus_sound_loop_info(tmpstr);
  if (tmpstr) FREE(tmpstr);
  if (res)
    {
      sres = XEN_LIST_8(C_TO_XEN_INT(res[0]), C_TO_XEN_INT(res[1]), C_TO_XEN_INT(res[2]),
			C_TO_XEN_INT(res[3]), C_TO_XEN_INT(res[4]), C_TO_XEN_INT(res[5]),
			C_TO_XEN_INT(res[6]), C_TO_XEN_INT(res[7]));
      FREE(res);
    }
  return(sres);
}

static XEN gmus_sound(const char *caller, int (*func)(const char *file), XEN filename)
{
  char *tmpstr = NULL;
  int res;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ONLY_ARG, caller, "a string"); 
  tmpstr = mus_expand_filename(XEN_TO_C_STRING(filename));
  res = (*func)(tmpstr);
  if (tmpstr) FREE(tmpstr);
  return(C_TO_XEN_INT(res));
}

static XEN gmus_sound_set(const char *caller, int (*func)(const char *file, int newval), XEN filename, XEN val)
{
  char *tmpstr = NULL;
  int res;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ARG_1, caller, "a string"); 
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ARG_2, caller, "an integer");
  tmpstr = mus_expand_filename(XEN_TO_C_STRING(filename));
  res = (*func)(tmpstr, XEN_TO_C_INT(val));
  if (tmpstr) FREE(tmpstr);
  return(C_TO_XEN_INT(res));
}

static XEN glmus_sound(const char *caller, off_t (*func)(const char *file), XEN filename)
{
  char *tmpstr = NULL;
  off_t res;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ONLY_ARG, caller, "a string"); 
  tmpstr = mus_expand_filename(XEN_TO_C_STRING(filename));
  res = (*func)(tmpstr);
  if (tmpstr) FREE(tmpstr);
  return(C_TO_XEN_OFF_T(res));
}

static XEN glmus_sound_set(const char *caller, int (*func)(const char *file, off_t newval), XEN filename, XEN val)
{
  char *tmpstr = NULL;
  off_t res;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ARG_1, caller, "a string"); 
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, caller, "a long");
  tmpstr = mus_expand_filename(XEN_TO_C_STRING(filename));
  res = (*func)(tmpstr, XEN_TO_C_OFF_T(val));
  if (tmpstr) FREE(tmpstr);
  return(C_TO_XEN_INT(res));
}

static XEN g_sound_samples(XEN filename) 
{
  #define H_mus_sound_samples "(" S_mus_sound_samples " filename) -> samples (frames*channels) in sound"
  return(glmus_sound(S_mus_sound_samples, mus_sound_samples, filename));
}

static XEN g_sound_set_samples(XEN filename, XEN val) 
{
  #define H_mus_sound_set_samples "(set! (" S_mus_sound_samples " filename) val) -> sets samples (frames*channels) in sound"
  return(glmus_sound_set("set-" S_mus_sound_samples, mus_sound_set_samples, filename, val));
}

static XEN g_sound_frames(XEN filename) 
{
  #define H_mus_sound_frames "(" S_mus_sound_frames " filename) -> frames (samples/channel) in sound"
  return(glmus_sound(S_mus_sound_frames, mus_sound_frames, filename));
}

static XEN g_sound_datum_size(XEN filename) 
{
  #define H_mus_sound_datum_size "(" S_mus_sound_datum_size " filename) -> bytes per sample of data in sound"
  return(gmus_sound(S_mus_sound_datum_size, mus_sound_datum_size, filename));
}

static XEN g_sound_data_location(XEN filename) 
{
  #define H_mus_sound_data_location "(" S_mus_sound_data_location " filename) -> location (bytes) of first sample of sound data"
  return(glmus_sound(S_mus_sound_data_location, mus_sound_data_location, filename));
}

static XEN g_sound_set_data_location(XEN filename, XEN val) 
{
  #define H_mus_sound_set_data_location "(set! (" S_mus_sound_data_location " filename) val) -> sets data_location in sound"
  return(glmus_sound_set("set-" S_mus_sound_data_location, mus_sound_set_data_location, filename, val));
}

static XEN g_sound_chans(XEN filename) 
{
  #define H_mus_sound_chans "(" S_mus_sound_chans " filename) -> channels of sound"
  return(gmus_sound(S_mus_sound_chans, mus_sound_chans, filename));
}

static XEN g_sound_set_chans(XEN filename, XEN val) 
{
  #define H_mus_sound_set_chans "(set! (" S_mus_sound_chans " filename) val) -> sets channels in sound"
  return(gmus_sound_set("set-" S_mus_sound_chans, mus_sound_set_chans, filename, val));
}

static XEN g_sound_srate(XEN filename) 
{
  #define H_mus_sound_srate "(" S_mus_sound_srate " filename) -> sampling rate of sound"
  return(gmus_sound(S_mus_sound_srate, mus_sound_srate, filename));
}

static XEN g_sound_set_srate(XEN filename, XEN val) 
{
  #define H_mus_sound_set_srate "(set! (" S_mus_sound_srate " filename) val) -> sets srate of sound"
  return(gmus_sound_set("set-" S_mus_sound_srate, mus_sound_set_srate, filename, val));
}

static XEN g_sound_header_type(XEN filename) 
{
  #define H_mus_sound_header_type "(" S_mus_sound_header_type " filename) -> header type (e.g. AIFF) of sound"
  return(gmus_sound(S_mus_sound_header_type, mus_sound_header_type, filename));
}

static XEN g_sound_set_header_type(XEN filename, XEN val) 
{
  #define H_mus_sound_set_header_type "(set! (" S_mus_sound_header_type " filename) val) -> sets header_type of sound"
  return(gmus_sound_set("set-" S_mus_sound_header_type, mus_sound_set_header_type, filename, val));
}

static XEN g_sound_data_format(XEN filename) 
{
  #define H_mus_sound_data_format "(" S_mus_sound_data_format " filename) -> data format (e.g. big endian short) of sound"
  return(gmus_sound(S_mus_sound_data_format, mus_sound_data_format, filename));
}

static XEN g_sound_set_data_format(XEN filename, XEN val) 
{
  #define H_mus_sound_set_data_format "(set! (" S_mus_sound_data_format " filename) val) -> sets data_format of sound"
  return(gmus_sound_set("set-" S_mus_sound_data_format, mus_sound_set_data_format, filename, val));
}

static XEN g_sound_length(XEN filename) 
{
  #define H_mus_sound_length "(" S_mus_sound_length " filename) -> file length in bytes"
  return(glmus_sound(S_mus_sound_length, mus_sound_length, filename));
}

static XEN g_sound_type_specifier(XEN filename) 
{
  #define H_mus_sound_type_specifier "(" S_mus_sound_type_specifier " filename) -> original file type identifier (e.g. 0x2e736e64)"
  return(gmus_sound(S_mus_sound_type_specifier, mus_sound_type_specifier, filename));
}

static XEN g_sound_forget(XEN filename) 
{
  #define H_mus_sound_forget "(" S_mus_sound_forget " filename) remove 'filename' from sound cache"
  return(gmus_sound(S_mus_sound_forget, mus_sound_forget, filename));
}

static XEN g_sound_prune(void) 
{
  #define H_mus_sound_prune "(" S_mus_sound_prune ") remove all defunct entries from sound cache"
  return(C_TO_XEN_INT(mus_sound_prune()));
}

static XEN g_sound_comment(XEN filename) 
{
  #define H_mus_sound_comment "(" S_mus_sound_comment " filename) -> comment (string) found in sound's header"
  char *tmpstr = NULL, *res = NULL; 
  XEN newstr;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ONLY_ARG, S_mus_sound_comment, "a string"); 
  res = mus_sound_comment(tmpstr = mus_expand_filename(XEN_TO_C_STRING(filename)));
  if (tmpstr) FREE(tmpstr);
  newstr = C_TO_XEN_STRING(res);
  if (res) FREE(res);
  return(newstr);
}

static XEN g_sound_write_date(XEN filename) 
{
  #define H_mus_sound_write_date "(" S_mus_sound_write_date " filename) -> write_date of sound"
  return(gmus_sound(S_mus_sound_write_date, mus_sound_write_date, filename));
}

static XEN g_sound_type_name(XEN type) 
{
  #define H_mus_header_type_name "(" S_mus_header_type_name " type) -> header type (e.g. mus-aiff) as a string"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(type), type, XEN_ONLY_ARG, S_mus_header_type_name, "an integer (header-type id)"); 
  return(C_TO_XEN_STRING(mus_header_type_name(XEN_TO_C_INT(type))));
}

static XEN g_sound_format_name(XEN format) 
{
  #define H_mus_data_format_name "(" S_mus_data_format_name " format) -> data format (e.g. mus-bshort) as a string"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(format), format, XEN_ONLY_ARG, S_mus_data_format_name, "an integer (data-format id)"); 
  return(C_TO_XEN_STRING(mus_data_format_name(XEN_TO_C_INT(format))));
}

static XEN g_sound_bytes_per_sample(XEN format) 
{
  #define H_mus_data_format_bytes_per_sample "(" S_mus_data_format_bytes_per_sample " format) -> number of bytes per sample in format (e.g. mus-short = 2)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(format), format, XEN_ONLY_ARG, S_mus_data_format_bytes_per_sample, "an integer (data-format id)"); 
  return(C_TO_XEN_INT(mus_data_format_to_bytes_per_sample(XEN_TO_C_INT(format))));
}

static XEN g_report_audio_state(void) 
{
  #define H_mus_audio_report "(" S_mus_audio_report ") -> string describing current audio hardware setup"
  return(C_TO_XEN_STRING(mus_audio_report()));
}

static XEN g_sound_duration(XEN filename) 
{
  #define H_mus_sound_duration "(" S_mus_sound_duration " filename) -> duration (seconds) of sound"
  char *tmpstr = NULL;
  float res;
  XEN_ASSERT_TYPE(XEN_STRING_P(filename), filename, XEN_ONLY_ARG, S_mus_sound_duration, "a string"); 
  res = mus_sound_duration(tmpstr = mus_expand_filename(XEN_TO_C_STRING(filename)));
  if (tmpstr) FREE(tmpstr);
  return(C_TO_XEN_DOUBLE(res));

}

static XEN g_audio_outputs(XEN speakers, XEN headphones, XEN line_out)
{
  #define H_mus_audio_sun_outputs "(" S_mus_audio_sun_outputs " speaker headphones line-out) sets the current Sun audio outputs"
#ifdef SUN
  XEN_ASSERT_TYPE(XEN_INTEGER_P(speakers), speakers, XEN_ARG_1, S_mus_audio_sun_outputs, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(headphones), headphones, XEN_ARG_2, S_mus_audio_sun_outputs, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(line_out), line_out, XEN_ARG_3, S_mus_audio_sun_outputs, "an integer");
  mus_audio_sun_outputs(XEN_TO_C_INT(speakers),
			XEN_TO_C_INT(headphones),
			XEN_TO_C_INT(line_out));
#endif
  return(XEN_FALSE);
}

static XEN g_sound_maxamp_exists(XEN file)
{
  #define H_mus_sound_maxamp_exists "(" S_mus_sound_maxamp_exists " filename) -> #t if max amps available for sound"
  int val;
  char *filename;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ONLY_ARG, S_mus_sound_maxamp, "a string");
  filename = mus_expand_filename(XEN_TO_C_STRING(file));
  val = mus_sound_maxamp_exists(filename);
  if (filename) FREE(filename);
  return(C_TO_XEN_BOOLEAN(val));
}

static XEN g_sound_maxamp(XEN file)
{
  #define H_mus_sound_maxamp "(" S_mus_sound_maxamp " filename) -> max amps in sound (a list of amps and locations)"
  int chans, i;
  off_t rtn;
  mus_sample_t *vals;
  off_t *times;
  char *filename;
  XEN res = XEN_EMPTY_LIST;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ONLY_ARG, S_mus_sound_maxamp, "a string");
  filename = mus_expand_filename(XEN_TO_C_STRING(file));
  chans = mus_sound_chans(filename);
  if (chans > 0)
    {
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
  if (filename) FREE(filename);
  return(res);
}

static XEN g_sound_set_maxamp(XEN file, XEN vals)
{
  #define H_mus_sound_set_maxamp "(" S_mus_sound_set_maxamp " filename vals) -> set max amps for sound (vals is a list of amps and locations)"
  int i, j, chans, len;
  mus_sample_t *mvals;
  off_t *times;
  char *filename;
  XEN lst;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_mus_sound_set_maxamp, "a string");
  XEN_ASSERT_TYPE(XEN_LIST_P(vals), vals, XEN_ARG_2, S_mus_sound_set_maxamp, "a list");
  filename = mus_expand_filename(XEN_TO_C_STRING(file));
  chans = mus_sound_chans(filename);
  if (chans > 0)
    {
      len = XEN_LIST_LENGTH(vals);
      if (len < (chans * 2))
	mus_misc_error(S_mus_sound_set_maxamp, "max amp list wrong length", vals);
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
  FREE(filename);
  return(vals);
}

/* to support the actual sound file/audio port stuff, we need an "smob" for the int** arrays */

static XEN_OBJECT_TYPE sound_data_tag = 0;
int sound_data_p(XEN obj) {return(XEN_OBJECT_TYPE_P(obj, sound_data_tag));}
#define SOUND_DATA_P(Obj) XEN_OBJECT_TYPE_P(Obj, sound_data_tag)

static XEN g_sound_data_p(XEN obj) 
{
  #define H_sound_data_p "(" S_sound_data_p " obj) -> #t if obj is a sound-data object, else #f"
  return(C_TO_XEN_BOOLEAN(sound_data_p(obj)));
}

static mus_sample_t **get_sound_data(XEN arg)
{
  sound_data *sd;
  if (SOUND_DATA_P(arg))
    {
      sd = (sound_data *)(XEN_OBJECT_REF(arg));
      return(sd->data);
    }
  return(NULL);
}

static void sound_data_free(sound_data *v)
{
  int i;
  if (v)
    {
      if ((v->data) && (v->wrapped == 0))
	{
	  for (i = 0; i < v->chans; i++) if (v->data[i]) FREE(v->data[i]);
	  FREE(v->data);
	}
      v->data = NULL;
      v->chans = 0;
      FREE(v);
    }
}

XEN_MAKE_OBJECT_FREE_PROCEDURE(sound_data, free_sound_data, sound_data_free)

static char *sound_data_to_string(sound_data *v)
{
  char *buf;
  buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  if ((v->data) && (v->chans > 0))
    mus_snprintf(buf, PRINT_BUFFER_SIZE, "#<sound-data: %d chan%s, %d frame%s>",
		 v->chans, (v->chans == 1) ? "" : "s",
		 v->length, (v->length == 1) ? "" : "s");
  else sprintf(buf, "#<sound-data: inactive>");
  return(buf);
}

XEN_MAKE_OBJECT_PRINT_PROCEDURE(sound_data, print_sound_data, sound_data_to_string)

static XEN equalp_sound_data(XEN obj1, XEN obj2)
{
  int i, chn;
  sound_data *v1, *v2;
  v1 = (sound_data *)XEN_OBJECT_REF(obj1);
  v2 = (sound_data *)XEN_OBJECT_REF(obj2);
  if (v1 == v2) return(XEN_TRUE);
  if ((v1) && (v2) &&
      (v1->chans == v2->chans) &&
      (v1->length == v2->length))
    {
      for (chn = 0; chn < v1->chans; chn++)
	for (i = 0; i < v1->length; i++)
	  if (v1->data[chn][i] != v2->data[chn][i])
	    return(XEN_FALSE);
      return(XEN_TRUE);
    }
  return(xen_return_first(XEN_FALSE, obj1, obj2));
}

static XEN sound_data_length(XEN obj)
{
  #define H_sound_data_length "(" S_sound_data_length " sd) -> length (samples) of each channel of sound-data object sd"
  sound_data *v;
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ONLY_ARG, S_sound_data_length, "a sound-data object");
  v = (sound_data *)XEN_OBJECT_REF(obj);
  return(C_TO_XEN_INT(v->length));
}

static XEN sound_data_chans(XEN obj)
{
  #define H_sound_data_chans "(" S_sound_data_chans " sd) -> number of channels in sound-data object sd"
  sound_data *v;
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ONLY_ARG, S_sound_data_chans, "a sound-data object");
  v = (sound_data *)XEN_OBJECT_REF(obj);
  return(C_TO_SMALL_XEN_INT(v->chans));
}

XEN make_sound_data(int chans, int frames)
{
  #define H_make_sound_data "(" S_make_sound_data " chans frames) -> new sound-data object with chans channels, each having frames samples"
  int i;
  sound_data *new_sound_data;
  new_sound_data = (sound_data *)MALLOC(sizeof(sound_data));
  new_sound_data->length = frames;
  new_sound_data->chans = chans;
  new_sound_data->wrapped = 0;
  new_sound_data->data = (mus_sample_t **)CALLOC(chans, sizeof(mus_sample_t *));
  for (i = 0; i < chans; i++)
    new_sound_data->data[i] = (mus_sample_t *)CALLOC(frames, sizeof(mus_sample_t));
  XEN_MAKE_AND_RETURN_OBJECT(sound_data_tag, new_sound_data, 0, free_sound_data);
}

XEN wrap_sound_data(int chans, int frames, mus_sample_t **data)
{
  sound_data *new_sound_data;
  new_sound_data = (sound_data *)MALLOC(sizeof(sound_data));
  new_sound_data->length = frames;
  new_sound_data->chans = chans;
  new_sound_data->wrapped = 1;
  new_sound_data->data = data;
  XEN_MAKE_AND_RETURN_OBJECT(sound_data_tag, new_sound_data, 0, free_sound_data);
}

static XEN g_make_sound_data(XEN chans, XEN frames)
{
  int chns, frms;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chans), chans, XEN_ARG_1, S_make_sound_data, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(frames), frames, XEN_ARG_2, S_make_sound_data, "an integer");
  chns = XEN_TO_C_INT(chans);
  frms = XEN_TO_C_INT(frames);
  if (chns <= 0)
    mus_misc_error(S_make_sound_data, "chans <= 0?", chans);
  if (frms <= 0)
    mus_misc_error(S_make_sound_data, "frames <= 0?", frames);
  return(make_sound_data(chns, frms));
			 
}

static XEN sound_data_ref(XEN obj, XEN chan, XEN frame_num)
{
  #define H_sound_data_ref "(" S_sound_data_ref " sd chan i) -> sample in channel chan at location i of sound-data object sd: sd[chan][i]"
  sound_data *v;
  int loc, chn;
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ARG_1, S_sound_data_ref, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_2, S_sound_data_ref, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(frame_num), frame_num, XEN_ARG_3, S_sound_data_ref, "an integer");
  v = (sound_data *)XEN_OBJECT_REF(obj);
  chn = XEN_TO_C_INT(chan);
  if ((chn < 0) || (chn >= v->chans))
    mus_misc_error(S_sound_data_ref, "invalid channel", XEN_LIST_2(obj, chan));
  loc = XEN_TO_C_INT(frame_num);
  if ((loc < 0) || (loc >= v->length))
    mus_misc_error(S_sound_data_ref, "invalid frame number", XEN_LIST_2(obj, frame_num));
  return(C_TO_XEN_DOUBLE(MUS_SAMPLE_TO_DOUBLE(v->data[chn][loc])));
}

static XEN g_sound_data_maxamp(XEN obj)
{
  #define H_sound_data_maxamp "(" S_sound_data_maxamp " sd) -> list of maxamps of data in sd"
  sound_data *v;
  int i, j, chans, len;
  mus_sample_t mx;
  mus_sample_t *buf;
  XEN lst = XEN_EMPTY_LIST;
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ARG_1, S_sound_data_maxamp, "a sound-data object");
  v = (sound_data *)XEN_OBJECT_REF(obj);
  chans = v->chans;
  len = v->length;
  for (i = chans - 1; i >= 0; i--)
    {
      mx = MUS_SAMPLE_MIN;
      buf = v->data[i];
      for (j = 0; j < len; j++)
	{
	  if (buf[j] > mx)
	    mx = buf[j];
	  else
	    if (-buf[j] > mx)
	      mx = -buf[j];
	}
      lst = XEN_CONS(C_TO_XEN_DOUBLE(MUS_SAMPLE_TO_DOUBLE(mx)), lst);
    }
  return(lst);
}


#if HAVE_APPLICABLE_SMOB
static XEN sound_data_apply(XEN obj, XEN chan, XEN i)
{
  return(sound_data_ref(obj, chan, i));
}
#endif

static XEN sound_data_set(XEN obj, XEN chan, XEN frame_num, XEN val)
{
  #define H_sound_data_setB "(" S_sound_data_setB " sd chan i val): set sound-data object sd's i-th element in channel chan to val: sd[chan][i] = val"
  sound_data *v;
  int loc, chn;
  XEN_ASSERT_TYPE(SOUND_DATA_P(obj), obj, XEN_ARG_1, S_sound_data_setB, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_2, S_sound_data_setB, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(frame_num), frame_num, XEN_ARG_3, S_sound_data_setB, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_4, S_sound_data_setB, "a number");
  v = (sound_data *)XEN_OBJECT_REF(obj);
  chn = XEN_TO_C_INT(chan);
  if ((chn < 0) || (chn >= v->chans))
    mus_misc_error(S_sound_data_setB, "invalid channel", XEN_LIST_3(obj, chan, frame_num));
  loc = XEN_TO_C_INT(frame_num);
  if ((loc < 0) || (loc >= v->length))
    mus_misc_error(S_sound_data_setB, "invalid frame number", XEN_LIST_3(obj, chan, frame_num));
  v->data[chn][loc] = MUS_DOUBLE_TO_SAMPLE(XEN_TO_C_DOUBLE(val));
  return(val);
}

static XEN sound_data2vct(XEN sdobj, XEN chan, XEN vobj)
{
  #define H_sound_data2vct "(" S_sound_data2vct " sd chan v) places sound-data object sd's channel chan data into vct object v"
  vct *v;
  sound_data *sd;
  int len, i, chn;
  XEN_ASSERT_TYPE(SOUND_DATA_P(sdobj), sdobj, XEN_ARG_1, S_sound_data2vct, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chan), chan, XEN_ARG_2, S_sound_data2vct, "an integer");
  XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(vobj) || VCT_P(vobj), vobj, XEN_ARG_3, S_sound_data2vct, "a vct");
  sd = (sound_data *)XEN_OBJECT_REF(sdobj);
  if (!(VCT_P(vobj))) vobj = make_vct(sd->length, (Float *)CALLOC(sd->length, sizeof(Float)));
  v = TO_VCT(vobj);
  chn = XEN_TO_C_INT_OR_ELSE(chan, 0);
  if (chn >= sd->chans)
    mus_misc_error(S_sound_data2vct, "invalid channel", XEN_LIST_3(sdobj, chan, vobj));
  if (sd->length < v->length) 
    len = sd->length; 
  else len = v->length;
  for (i = 0; i < len; i++) 
    v->data[i] = (Float)(MUS_SAMPLE_TO_FLOAT(sd->data[chn][i]));
  return(vobj);
}

static XEN vct2sound_data(XEN vobj, XEN sdobj, XEN chan)
{
  #define H_vct2sound_data "(" S_vct2sound_data " v sd chan) places vct v's data into sound-data sd's channel chan"
  vct *v;
  sound_data *sd;
  int len, i, chn;
  XEN_ASSERT_TYPE(VCT_P(vobj), vobj, XEN_ARG_1, S_vct2sound_data, "a vct");
  XEN_ASSERT_TYPE(XEN_NOT_BOUND_P(sdobj) || SOUND_DATA_P(sdobj), sdobj, XEN_ARG_2, S_vct2sound_data, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(chan), chan, XEN_ARG_3, S_vct2sound_data, "an integer");
  v = TO_VCT(vobj);
  if (!(SOUND_DATA_P(sdobj))) sdobj = make_sound_data(1, v->length);
  sd = (sound_data *)XEN_OBJECT_REF(sdobj);
  chn = XEN_TO_C_INT_OR_ELSE(chan, 0);
  if (chn >= sd->chans)
    mus_misc_error(S_vct2sound_data, "invalid channel", XEN_LIST_3(vobj, chan, sdobj));
  if (sd->length < v->length) 
    len = sd->length; 
  else len = v->length;
  for (i = 0; i < len; i++) 
    sd->data[chn][i] = MUS_FLOAT_TO_SAMPLE(v->data[i]);
  return(sdobj);
}


static XEN g_open_sound_input(XEN file)
{
  #define H_mus_sound_open_input "(" S_mus_sound_open_input " filename) -> fd (int), opens filename for sound input"
  int fd;
  char *tmpstr = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ONLY_ARG, S_mus_sound_open_input, "a string");
  fd = mus_sound_open_input(tmpstr = mus_expand_filename(XEN_TO_C_STRING(file)));
  if (tmpstr) FREE(tmpstr);
  return(C_TO_XEN_INT(fd));
}

static XEN g_open_sound_output(XEN file, XEN srate, XEN chans, XEN data_format, XEN header_type, XEN comment)
{

  #define H_mus_sound_open_output "(" S_mus_sound_open_output " filename srate chans data-format header-type &optional comment)\n\
opens filename for sound output with the given srate and so on, returns the file number (int). \
the file size is normally set later via mus-sound-close-output. srate is an integer, comment is a string, \
data-format is a sndlib format indicator such as mus-bshort, if #f if defaults to a format compatible with sndlib, \
header-type is a sndlib type indicator such as mus-aiff, sndlib currently only writes 5 or so header types."

  int fd = -1, df, ht, chns;
  char *com = NULL;
  char *tmpstr = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_mus_sound_open_output, "a string");
  XEN_ASSERT_TYPE(XEN_NUMBER_OR_BOOLEAN_P(srate), srate, XEN_ARG_2, S_mus_sound_open_output, "a number or #f");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_P(chans), chans, XEN_ARG_3, S_mus_sound_open_output, "an integer or #f");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_P(data_format), data_format, XEN_ARG_4, S_mus_sound_open_output, "an integer (data-format id) or #f");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_P(header_type), header_type, XEN_ARG_5, S_mus_sound_open_output, "an integer (header-type id) or #f");
  XEN_ASSERT_TYPE((XEN_STRING_P(comment) || (XEN_NOT_BOUND_P(comment))), comment, XEN_ARG_6, S_mus_sound_open_output, "a string");
  if (XEN_INTEGER_P(data_format))
    df = XEN_TO_C_INT(data_format);
  else df = MUS_OUT_FORMAT;
  if (MUS_DATA_FORMAT_OK(df))
    {
      ht = XEN_TO_C_INT_OR_ELSE(header_type, 0);
      if (MUS_HEADER_TYPE_OK(ht))
	{
	  chns = XEN_TO_C_INT_OR_ELSE(chans, 0);
	  if (chns > 0)
	    {
	      tmpstr = mus_expand_filename(XEN_TO_C_STRING(file));
	      if (XEN_STRING_P(comment)) com = XEN_TO_C_STRING(comment);
	      fd = mus_sound_open_output(tmpstr,
					 XEN_TO_C_INT_OR_ELSE(srate, 0),
					 chns,
					 df,
					 ht,
					 com);
	      if (tmpstr) FREE(tmpstr);
	    }
	  else mus_misc_error(S_mus_sound_open_output, "invalid chans", chans);
	}
      else mus_misc_error(S_mus_sound_open_output, "invalid header type", header_type);
    }
  else mus_misc_error(S_mus_sound_open_output, "invalid data format", data_format);
  return(C_TO_XEN_INT(fd));
}

static XEN g_reopen_sound_output(XEN file, XEN chans, XEN data_format, XEN header_type, XEN data_loc)
{

  #define H_mus_sound_reopen_output "(" S_mus_sound_reopen_output " filename chans data-format header-type data-location)\n\
reopen (without alteration) filename for output \
data-format and header-type are sndlib indicators such as mus-bshort or mus-aiff \
data-location should be retrieved from a previous call to mus-sound-data-location"

  int fd = -1, df, ht, chns;
  char *tmpstr = NULL;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ARG_1, S_mus_sound_reopen_output, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_P(chans), chans, XEN_ARG_2, S_mus_sound_reopen_output, "an integer or #f");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_P(data_format), data_format, XEN_ARG_3, S_mus_sound_reopen_output, "an integer (data-format id) or #f");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_P(header_type), header_type, XEN_ARG_4, S_mus_sound_reopen_output, "an integer (header-type id) or #f");
  XEN_ASSERT_TYPE(XEN_NUMBER_OR_BOOLEAN_P(data_loc), data_loc, XEN_ARG_5, S_mus_sound_reopen_output, "an integer or #f");
  df = XEN_TO_C_INT_OR_ELSE(data_format, MUS_OUT_FORMAT);
  if (MUS_DATA_FORMAT_OK(df))
    {
      ht = XEN_TO_C_INT_OR_ELSE(header_type, 0);
      if (MUS_HEADER_TYPE_OK(ht))
	{
	  chns = XEN_TO_C_INT_OR_ELSE(chans, 0);
	  if (chns > 0)
	    {
	      tmpstr = mus_expand_filename(XEN_TO_C_STRING(file));
	      fd = mus_sound_reopen_output(tmpstr,
					   chns,
					   df,
					   ht,
					   XEN_TO_C_OFF_T_OR_ELSE(data_loc, 0));
	      if (tmpstr) FREE(tmpstr);
	    }
	  else mus_misc_error(S_mus_sound_reopen_output, "invalid chans", chans);
	}
      else mus_misc_error(S_mus_sound_reopen_output, "invalid header type", header_type);
    }
  else mus_misc_error(S_mus_sound_reopen_output, "invalid data format", data_format);
  return(C_TO_XEN_INT(fd));
}

static XEN g_close_sound_input(XEN fd)
{
  #define H_mus_sound_close_input "(" S_mus_sound_close_input " fd) closes file number fd"
  int nfd;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ONLY_ARG, S_mus_sound_close_input, "an integer");
  nfd = XEN_TO_C_INT(fd);
  if ((nfd < 0) || (nfd == fileno(stdin)) || (nfd == fileno(stdout)) || (nfd == fileno(stderr)))
    mus_misc_error(S_mus_sound_close_input, "invalid file", fd);
  return(C_TO_XEN_INT(mus_sound_close_input(XEN_TO_C_INT(fd))));
}

static XEN g_close_sound_output(XEN fd, XEN bytes)
{
  #define H_mus_sound_close_output "(" S_mus_sound_close_output " fd bytes) closes file number fd \
after updating its header (if any) to reflect bytes, the new file data size"

  int nfd;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ARG_1, S_mus_sound_close_output, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(bytes), bytes, XEN_ARG_2, S_mus_sound_close_output, "a number");
  nfd = XEN_TO_C_INT(fd);
  if ((nfd < 0) || (nfd == fileno(stdin)) || (nfd == fileno(stdout)) || (nfd == fileno(stderr)))
    mus_misc_error(S_mus_sound_close_output, "invalid file", fd);
  return(C_TO_XEN_INT(mus_sound_close_output(XEN_TO_C_INT(fd),
					     XEN_TO_C_OFF_T_OR_ELSE(bytes, 0))));
}

static XEN g_read_sound(XEN fd, XEN beg, XEN end, XEN chans, XEN sv)
{
  #define H_mus_sound_read "(" S_mus_sound_read " fd beg end chans sdata) reads sound data from file number fd, \
filling the sound-data object sdata's buffers starting at (buffer location) beg, going to end"

  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ARG_1, S_mus_sound_read, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(beg), beg, XEN_ARG_2, S_mus_sound_read, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(end), end, XEN_ARG_3, S_mus_sound_read, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chans), chans, XEN_ARG_4, S_mus_sound_read, "an integer");
  XEN_ASSERT_TYPE(SOUND_DATA_P(sv), sv, XEN_ARG_5, S_mus_sound_read, "a sound-data object");
  return(C_TO_XEN_INT(mus_sound_read(XEN_TO_C_INT(fd),
				     XEN_TO_C_INT_OR_ELSE(beg, 0),
				     XEN_TO_C_INT_OR_ELSE(end, 0),
				     XEN_TO_C_INT(chans),
				     get_sound_data(sv))));
}

static XEN g_write_sound(XEN fd, XEN beg, XEN end, XEN chans, XEN sv)
{
  #define H_mus_sound_write "(" S_mus_sound_write " fd beg end chans sdata) writes sound-data object sdata's data \
starting at (buffer location) beg, going to end, writing to file number fd"

  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ARG_1, S_mus_sound_write, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(beg), beg, XEN_ARG_2, S_mus_sound_write, "a number");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(end), end, XEN_ARG_3, S_mus_sound_write, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chans), chans, XEN_ARG_4, S_mus_sound_write, "an integer");
  XEN_ASSERT_TYPE(SOUND_DATA_P(sv), sv, XEN_ARG_5, S_mus_sound_write, "a sound-data object");
  return(C_TO_XEN_INT(mus_sound_write(XEN_TO_C_INT(fd),
				      XEN_TO_C_INT_OR_ELSE(beg, 0),
				      XEN_TO_C_INT_OR_ELSE(end, 0),
				      XEN_TO_C_INT(chans),
				      get_sound_data(sv))));
}

static XEN g_seek_sound(XEN fd, XEN offset, XEN origin)
{
  #define H_mus_sound_seek "(" S_mus_sound_seek " fd offset origin) moves the current read/write location in file number fd \
to the short-wise sample offset given origin (both treated as in lseek)"

  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ARG_1, S_mus_sound_seek, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(offset), offset, XEN_ARG_2, S_mus_sound_seek, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(origin), origin, XEN_ARG_3, S_mus_sound_seek, "an integer");
  return(C_TO_XEN_OFF_T(mus_sound_seek(XEN_TO_C_INT(fd),
				       XEN_TO_C_OFF_T(offset),
				       XEN_TO_C_INT(origin))));
}

static XEN g_seek_sound_frame(XEN fd, XEN offset)
{
  #define H_mus_sound_seek_frame "(" S_mus_sound_seek_frame " fd frame) moves the current read/write location in file number fd \
to the indicated frame"

  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ARG_1, S_mus_sound_seek_frame, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(offset), offset, XEN_ARG_2, S_mus_sound_seek_frame, "an integer");
  return(C_TO_XEN_OFF_T(mus_sound_seek_frame(XEN_TO_C_INT(fd),
					     XEN_TO_C_OFF_T(offset))));
}

/* since mus-audio-read|write assume sound-data vectors communicating with Scheme,
 *   we can't assume that the user has had a chance to deal with type problems.
 *   So, we keep track of each audio line's current read|write format and
 *   translate in mus_audio_read|write if necessary (if mus_sample_t isn't
 *   compatible with the desired format).
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

static XEN g_open_audio_output(XEN dev, XEN srate, XEN chans, XEN format, XEN size)
{
  #define H_mus_audio_open_output "(" S_mus_audio_open_output " device srate chans format clipped)\n\
opens the audio device ready for output at the given srate and so on. \
returns the audio line number:\n\
  (set! line (mus-audio-open-output mus-audio-default 22050 1 mus-lshort 256)"

  int fmt, line;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(dev), dev, XEN_ARG_1, S_mus_audio_open_output, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(srate), srate, XEN_ARG_2, S_mus_audio_open_output, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chans), chans, XEN_ARG_3, S_mus_audio_open_output, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(format), format, XEN_ARG_4, S_mus_audio_open_output, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(size), size, XEN_ARG_5, S_mus_audio_open_output, "a number");
  if (!(MUS_AUDIO_DEVICE_OK(XEN_TO_C_INT(dev))))
    mus_misc_error(S_mus_audio_open_output, "invalid device", dev);
  fmt = XEN_TO_C_INT(format);
  if (!(MUS_DATA_FORMAT_OK(fmt)))
    mus_misc_error(S_mus_audio_open_output, "invalid data format", format);
  if (XEN_TO_C_INT_OR_ELSE(size, 0) < 0)
    mus_misc_error(S_mus_audio_open_output, "invalid size", size);
  if (XEN_TO_C_INT_OR_ELSE(srate, 0) <= 0)
    mus_misc_error(S_mus_audio_open_output, "invalid srate", srate);
  if ((XEN_TO_C_INT(chans) <= 0) || (XEN_TO_C_INT(chans) > 256))
    mus_misc_error(S_mus_audio_open_output, "invalid chans", chans);
  line = mus_audio_open_output(XEN_TO_C_INT(dev),
			       XEN_TO_C_INT_OR_ELSE(srate, 0),
			       XEN_TO_C_INT(chans),
			       fmt,
			       XEN_TO_C_INT_OR_ELSE(size, 0));
  audio_io_set_write_format(line, fmt);
  return(C_TO_XEN_INT(line));
}

static XEN g_open_audio_input(XEN dev, XEN srate, XEN chans, XEN format, XEN size)
{
  #define H_mus_audio_open_input "(" S_mus_audio_open_input " (device srate chans format bufsize)\n\
opens the audio device ready for input with the indicated attributes, returns the audio line number"

  int fmt, line;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(dev), dev, XEN_ARG_1, S_mus_audio_open_input, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(srate), srate, XEN_ARG_2, S_mus_audio_open_input, "a number");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chans), chans, XEN_ARG_3, S_mus_audio_open_input, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(format), format, XEN_ARG_4, S_mus_audio_open_input, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(size), size, XEN_ARG_5, S_mus_audio_open_input, "a number");
  if (!(MUS_AUDIO_DEVICE_OK(XEN_TO_C_INT(dev))))
    mus_misc_error(S_mus_audio_open_input, "invalid device", dev);
  fmt = XEN_TO_C_INT(format);
  if (!(MUS_DATA_FORMAT_OK(fmt)))
    mus_misc_error(S_mus_audio_open_input, "invalid data format", format);
  if (XEN_TO_C_INT_OR_ELSE(size, 0) < 0)
    mus_misc_error(S_mus_audio_open_input, "invalid size", size);
  if (XEN_TO_C_INT_OR_ELSE(srate, 0) <= 0)
    mus_misc_error(S_mus_audio_open_input, "invalid srate", srate);
  if (XEN_TO_C_INT(chans) <= 0)
    mus_misc_error(S_mus_audio_open_input, "invalid chans", chans);
  line = mus_audio_open_input(XEN_TO_C_INT(dev),
			      XEN_TO_C_INT_OR_ELSE(srate, 0),
			      XEN_TO_C_INT(chans),
			      fmt,
			      XEN_TO_C_INT_OR_ELSE(size, 0));
  audio_io_set_read_format(line, fmt);
  return(C_TO_XEN_INT(line));
}

static XEN g_close_audio(XEN line)
{
  #define H_mus_audio_close "(" S_mus_audio_close " line) closes the audio hardware port line"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(line), line, XEN_ONLY_ARG, S_mus_audio_close, "an integer");
  return(C_TO_XEN_INT(mus_audio_close(XEN_TO_C_INT(line))));
}

static XEN g_save_audio_state (void) 
{
  #define H_mus_audio_save "(" S_mus_audio_save ") tries to save the current audio hardware state for a subsequent mus-audio-restore"
  mus_audio_save(); 
  return(XEN_FALSE);
}

static XEN g_restore_audio_state (void) 
{
  #define H_mus_audio_restore "(" S_mus_audio_restore ") tries to restore a previously saved audio hardware state"
  mus_audio_restore(); 
  return(XEN_FALSE);
}

static XEN g_audio_systems (void) 
{
  #define H_mus_audio_systems "(" S_mus_audio_systems ") -> number of available audio systems (normally each sound card is a separate 'system')"
  return(C_TO_XEN_INT(mus_audio_systems()));
}


/* these take a sndlib buffer (sound_data) and handle the conversion to the interleaved char* internally */
/* so, they take "frames", not "bytes", and a sound_data object, not char* etc */

static XEN g_write_audio(XEN line, XEN sdata, XEN frames)
{
  #define H_mus_audio_write "(" S_mus_audio_write " line sdata frames) writes frames of data (channels*frames = samples) \
to the audio line from the sound-data object sdata."

  char *obuf;
  sound_data *sd;
  int outbytes, val, frms, fmt, fd;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(line), line, XEN_ARG_1, S_mus_audio_write, "an integer");
  XEN_ASSERT_TYPE(SOUND_DATA_P(sdata), sdata, XEN_ARG_2, S_mus_audio_write, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(frames), frames, XEN_ARG_3, S_mus_audio_write, "an integer");
  sd = (sound_data *)XEN_OBJECT_REF(sdata);
  frms = XEN_TO_C_INT(frames);
  if (frms > sd->length)
    mus_misc_error(S_mus_audio_write, 
		   "frames > sound-data buffer length: ", 
		   XEN_LIST_2(frames,
			      C_TO_XEN_INT(sd->length)));
  fd = XEN_TO_C_INT(line);
  fmt = audio_io_write_format(fd);
  outbytes = frms * sd->chans * mus_data_format_to_bytes_per_sample(fmt);
  obuf = (char *)CALLOC(outbytes, sizeof(char));
  mus_file_write_buffer(fmt, 0, frms - 1, sd->chans, sd->data, obuf, TRUE); /* TRUE -> clipped */
  val = mus_audio_write(fd, obuf, outbytes);
  FREE(obuf);
  return(xen_return_first(C_TO_XEN_INT(val), sdata));
}

static XEN g_read_audio(XEN line, XEN sdata, XEN frames)
{
  #define H_mus_audio_read "(" S_mus_audio_read " line sdata frames) reads frames of data (channels*frames = samples) \
from the audio line into the sound-data object sdata."

  char *inbuf;
  sound_data *sd;
  int val, inbytes, frms, fd, fmt;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(line), line, XEN_ARG_1, S_mus_audio_read, "an integer");
  XEN_ASSERT_TYPE(SOUND_DATA_P(sdata), sdata, XEN_ARG_2, S_mus_audio_read, "a sound-data object");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(frames), frames, XEN_ARG_3, S_mus_audio_read, "an integer");
  sd = (sound_data *)XEN_OBJECT_REF(sdata);
  frms = XEN_TO_C_INT(frames);
  fd = XEN_TO_C_INT(line);
  fmt = audio_io_read_format(fd);
  inbytes = frms * sd->chans * mus_data_format_to_bytes_per_sample(fmt);
  inbuf = (char *)CALLOC(inbytes, sizeof(char));
  val = mus_audio_read(fd, inbuf, inbytes);
  mus_file_read_buffer(fmt, 0, sd->chans, frms - 1, sd->data, inbuf);
  FREE(inbuf);
  return(xen_return_first(C_TO_XEN_INT(val), sdata));
}

static XEN g_mus_audio_mixer_read(XEN dev, XEN field, XEN chan, XEN vals)
{
  #define H_mus_audio_mixer_read "(" S_mus_audio_mixer_read " device field channel vals) reads sound card 'mixer' state"
  int val, i, len;
  float *fvals;
  XEN *vdata;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(dev), dev, XEN_ARG_1, S_mus_audio_mixer_read, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(field), field, XEN_ARG_2, S_mus_audio_mixer_read, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_3, S_mus_audio_mixer_read, "an integer");
  XEN_ASSERT_TYPE(XEN_VECTOR_P(vals), vals, XEN_ARG_4, S_mus_audio_mixer_read, "a vector");
  if (!(MUS_AUDIO_DEVICE_OK(XEN_TO_C_INT(dev)))) 
    mus_misc_error(S_mus_audio_mixer_read, "invalid device", dev);
  if (!(MUS_AUDIO_DEVICE_OK(XEN_TO_C_INT(field))))
    mus_misc_error(S_mus_audio_mixer_read, "invalid field", field);
  len = XEN_VECTOR_LENGTH(vals);
  if (len == 0)
    fvals = (float *)CALLOC(1, sizeof(float));
  else fvals = (float *)CALLOC(len, sizeof(float));
  val = mus_audio_mixer_read(XEN_TO_C_INT(dev),
			     XEN_TO_C_INT(field),
			     XEN_TO_C_INT(chan),
			     fvals);
  vdata = XEN_VECTOR_ELEMENTS(vals);
  for (i = 0; i < len; i++) 
    vdata[i] = C_TO_XEN_DOUBLE(fvals[i]);
  FREE(fvals);
  return(C_TO_XEN_INT(val));
}

static XEN g_mus_audio_mixer_write(XEN dev, XEN field, XEN chan, XEN vals)
{
  #define H_mus_audio_mixer_write "(" S_mus_audio_mixer_write " device field channel vals) changes the sound card's 'mixer' state"
  int i, len, res;
  float *fvals;
  XEN *vdata;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(dev), dev, XEN_ARG_1, S_mus_audio_mixer_write, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(field), field, XEN_ARG_2, S_mus_audio_mixer_write, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(chan), chan, XEN_ARG_3, S_mus_audio_mixer_write, "an integer");
  XEN_ASSERT_TYPE(XEN_VECTOR_P(vals), vals, XEN_ARG_4, S_mus_audio_mixer_write, "a vector");
  if (!(MUS_AUDIO_DEVICE_OK(XEN_TO_C_INT(dev)))) 
    mus_misc_error(S_mus_audio_mixer_write, "invalid device", dev);
  if (!(MUS_AUDIO_DEVICE_OK(XEN_TO_C_INT(field))))
    mus_misc_error(S_mus_audio_mixer_write, "invalid field", field);
  len = XEN_VECTOR_LENGTH(vals);
  if (len == 0)
    fvals = (float *)CALLOC(1, sizeof(float));
  else
    {
      fvals = (float *)CALLOC(len, sizeof(float));
      vdata = XEN_VECTOR_ELEMENTS(vals);
      for (i = 0; i < len; i++) 
	fvals[i] = XEN_TO_C_DOUBLE(vdata[i]);
    }
  res = mus_audio_mixer_write(XEN_TO_C_INT(dev),
			      XEN_TO_C_INT(field),
			      XEN_TO_C_INT(chan),
			      fvals);
  FREE(fvals);
  return(C_TO_XEN_INT(res));
}

static XEN g_mus_set_data_clipped(XEN fd, XEN clipped)
{
  #define H_mus_file_set_data_clipped "(" S_mus_file_set_data_clipped " fd val) sets whether data associated with file fd is clipped or wraps around"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ARG_1, S_mus_file_set_data_clipped, "an integer");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(clipped), fd, XEN_ARG_2, S_mus_file_set_data_clipped, "a boolean");
  return(C_TO_XEN_INT(mus_file_set_data_clipped(XEN_TO_C_INT(fd),
						(XEN_FALSE_P(clipped)) ? 0 : 1)));
}

static XEN g_mus_prescaler(XEN fd)
{
  #define H_mus_file_prescaler "(" S_mus_file_prescaler " fd) -> current prescaler (normally 1.0) associated with fd"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ONLY_ARG, S_mus_file_set_prescaler, "an integer");
  return(C_TO_XEN_DOUBLE(mus_file_prescaler(XEN_TO_C_INT(fd))));
}

static XEN g_mus_set_prescaler(XEN fd, XEN val)
{
  #define H_mus_file_set_prescaler "(" S_mus_file_set_prescaler " fd val) sets the current prescaler associated with fd"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(fd), fd, XEN_ARG_1, S_mus_file_set_prescaler, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ARG_2, S_mus_file_set_prescaler, "a number");
  return(C_TO_XEN_DOUBLE(mus_file_set_prescaler(XEN_TO_C_INT(fd),
						XEN_TO_C_DOUBLE(val))));
}

static XEN g_mus_expand_filename(XEN file)
{
  #define H_mus_expand_filename "(" S_mus_expand_filename " name) returns a 'canonical' or 'absolute' filename"
  XEN result;
  char *tmpstr;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ONLY_ARG, S_mus_expand_filename, "a string");
  tmpstr = mus_expand_filename(XEN_TO_C_STRING(file));
  result = C_TO_XEN_STRING(tmpstr);
  if (tmpstr) FREE(tmpstr);
  return(result);
}

static XEN g_mus_sound_report_cache(XEN file)
{
  #define H_mus_sound_report_cache "(" S_mus_sound_report_cache " name) prints the current sound \
header data table to the file given or stdout if none"

  XEN res = XEN_FALSE;
  if (XEN_NOT_BOUND_P(file))
    mus_sound_print_cache();
  else
    {
      FILE *fd;
      char *name = NULL;
      fd = FOPEN(name = mus_expand_filename(XEN_TO_C_STRING(file)), "w");
      mus_sound_report_cache(fd);
      FCLOSE(fd);
      if (name) FREE(name);
      return(file);
    }
  return(res);
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_1(sound_data_length_w, sound_data_length)
XEN_NARGIFY_1(sound_data_chans_w, sound_data_chans)
XEN_NARGIFY_3(sound_data_ref_w, sound_data_ref)
XEN_NARGIFY_4(sound_data_set_w, sound_data_set)
XEN_NARGIFY_2(g_make_sound_data_w, g_make_sound_data)
XEN_NARGIFY_1(g_sound_data_p_w, g_sound_data_p)
XEN_NARGIFY_1(g_sound_data_maxamp_w, g_sound_data_maxamp)
XEN_ARGIFY_3(sound_data2vct_w, sound_data2vct)
XEN_ARGIFY_3(vct2sound_data_w, vct2sound_data)
XEN_NARGIFY_1(g_sound_samples_w, g_sound_samples)
XEN_NARGIFY_2(g_sound_set_samples_w, g_sound_set_samples)
XEN_NARGIFY_1(g_sound_frames_w, g_sound_frames)
XEN_NARGIFY_1(g_sound_duration_w, g_sound_duration)
XEN_NARGIFY_1(g_sound_datum_size_w, g_sound_datum_size)
XEN_NARGIFY_1(g_sound_data_location_w, g_sound_data_location)
XEN_NARGIFY_2(g_sound_set_data_location_w, g_sound_set_data_location)
XEN_NARGIFY_1(g_sound_chans_w, g_sound_chans)
XEN_NARGIFY_2(g_sound_set_chans_w, g_sound_set_chans)
XEN_NARGIFY_1(g_sound_srate_w, g_sound_srate)
XEN_NARGIFY_2(g_sound_set_srate_w, g_sound_set_srate)
XEN_NARGIFY_1(g_sound_header_type_w, g_sound_header_type)
XEN_NARGIFY_2(g_sound_set_header_type_w, g_sound_set_header_type)
XEN_NARGIFY_1(g_sound_data_format_w, g_sound_data_format)
XEN_NARGIFY_2(g_sound_set_data_format_w, g_sound_set_data_format)
XEN_NARGIFY_1(g_sound_length_w, g_sound_length)
XEN_NARGIFY_1(g_sound_type_specifier_w, g_sound_type_specifier)
XEN_NARGIFY_1(g_sound_type_name_w, g_sound_type_name)
XEN_NARGIFY_1(g_sound_format_name_w, g_sound_format_name)
XEN_NARGIFY_1(g_sound_comment_w, g_sound_comment)
XEN_NARGIFY_1(g_sound_write_date_w, g_sound_write_date)
XEN_NARGIFY_1(g_sound_bytes_per_sample_w, g_sound_bytes_per_sample)
XEN_NARGIFY_1(g_sound_loop_info_w, g_sound_loop_info)
XEN_NARGIFY_1(g_sound_maxamp_w, g_sound_maxamp)
XEN_NARGIFY_2(g_sound_set_maxamp_w, g_sound_set_maxamp)
XEN_NARGIFY_1(g_sound_maxamp_exists_w, g_sound_maxamp_exists)
XEN_NARGIFY_0(g_report_audio_state_w, g_report_audio_state)
XEN_NARGIFY_3(g_audio_outputs_w, g_audio_outputs)
XEN_NARGIFY_1(g_open_sound_input_w, g_open_sound_input)
XEN_NARGIFY_1(g_close_sound_input_w, g_close_sound_input)
XEN_NARGIFY_1(g_close_audio_w, g_close_audio)
XEN_NARGIFY_0(g_save_audio_state_w, g_save_audio_state)
XEN_NARGIFY_0(g_restore_audio_state_w, g_restore_audio_state)
XEN_NARGIFY_0(g_audio_systems_w, g_audio_systems)
XEN_NARGIFY_4(g_mus_audio_mixer_read_w, g_mus_audio_mixer_read)
XEN_NARGIFY_4(g_mus_audio_mixer_write_w, g_mus_audio_mixer_write)
XEN_NARGIFY_2(g_mus_set_data_clipped_w, g_mus_set_data_clipped)
XEN_NARGIFY_1(g_mus_prescaler_w, g_mus_prescaler)
XEN_NARGIFY_2(g_mus_set_prescaler_w, g_mus_set_prescaler)
XEN_NARGIFY_1(g_mus_expand_filename_w, g_mus_expand_filename)
XEN_NARGIFY_3(g_write_audio_w, g_write_audio)
XEN_NARGIFY_3(g_read_audio_w, g_read_audio)
XEN_ARGIFY_6(g_open_sound_output_w, g_open_sound_output)
XEN_NARGIFY_5(g_reopen_sound_output_w, g_reopen_sound_output)
XEN_NARGIFY_2(g_close_sound_output_w, g_close_sound_output)
XEN_NARGIFY_5(g_read_sound_w, g_read_sound)
XEN_NARGIFY_5(g_write_sound_w, g_write_sound)
XEN_NARGIFY_3(g_seek_sound_w, g_seek_sound)
XEN_NARGIFY_2(g_seek_sound_frame_w, g_seek_sound_frame)
XEN_NARGIFY_5(g_open_audio_output_w, g_open_audio_output)
XEN_NARGIFY_5(g_open_audio_input_w, g_open_audio_input)
XEN_ARGIFY_1(g_mus_sound_report_cache_w, g_mus_sound_report_cache)
XEN_NARGIFY_1(g_sound_forget_w, g_sound_forget)
XEN_NARGIFY_0(g_sound_prune_w, g_sound_prune)
#else
#define sound_data_length_w sound_data_length
#define sound_data_chans_w sound_data_chans
#define sound_data_ref_w sound_data_ref
#define sound_data_set_w sound_data_set
#define g_make_sound_data_w g_make_sound_data
#define g_sound_data_p_w g_sound_data_p
#define g_sound_data_maxamp_w g_sound_data_maxamp
#define sound_data2vct_w sound_data2vct
#define vct2sound_data_w vct2sound_data
#define g_sound_samples_w g_sound_samples
#define g_sound_set_samples_w g_sound_set_samples
#define g_sound_frames_w g_sound_frames
#define g_sound_duration_w g_sound_duration
#define g_sound_datum_size_w g_sound_datum_size
#define g_sound_data_location_w g_sound_data_location
#define g_sound_set_data_location_w g_sound_set_data_location
#define g_sound_chans_w g_sound_chans
#define g_sound_set_chans_w g_sound_set_chans
#define g_sound_srate_w g_sound_srate
#define g_sound_set_srate_w g_sound_set_srate
#define g_sound_header_type_w g_sound_header_type
#define g_sound_set_header_type_w g_sound_set_header_type
#define g_sound_data_format_w g_sound_data_format
#define g_sound_set_data_format_w g_sound_set_data_format
#define g_sound_length_w g_sound_length
#define g_sound_type_specifier_w g_sound_type_specifier
#define g_sound_type_name_w g_sound_type_name
#define g_sound_format_name_w g_sound_format_name
#define g_sound_comment_w g_sound_comment
#define g_sound_write_date_w g_sound_write_date
#define g_sound_bytes_per_sample_w g_sound_bytes_per_sample
#define g_sound_loop_info_w g_sound_loop_info
#define g_sound_maxamp_w g_sound_maxamp
#define g_sound_set_maxamp_w g_sound_set_maxamp
#define g_sound_maxamp_exists_w g_sound_maxamp_exists
#define g_report_audio_state_w g_report_audio_state
#define g_audio_outputs_w g_audio_outputs
#define g_open_sound_input_w g_open_sound_input
#define g_close_sound_input_w g_close_sound_input
#define g_close_audio_w g_close_audio
#define g_save_audio_state_w g_save_audio_state
#define g_restore_audio_state_w g_restore_audio_state
#define g_audio_systems_w g_audio_systems
#define g_mus_audio_mixer_read_w g_mus_audio_mixer_read
#define g_mus_audio_mixer_write_w g_mus_audio_mixer_write
#define g_mus_set_data_clipped_w g_mus_set_data_clipped
#define g_mus_prescaler_w g_mus_prescaler
#define g_mus_set_prescaler_w g_mus_set_prescaler
#define g_mus_expand_filename_w g_mus_expand_filename
#define g_write_audio_w g_write_audio
#define g_read_audio_w g_read_audio
#define g_open_sound_output_w g_open_sound_output
#define g_reopen_sound_output_w g_reopen_sound_output
#define g_close_sound_output_w g_close_sound_output
#define g_read_sound_w g_read_sound
#define g_write_sound_w g_write_sound
#define g_seek_sound_w g_seek_sound
#define g_seek_sound_frame_w g_seek_sound_frame
#define g_open_audio_output_w g_open_audio_output
#define g_open_audio_input_w g_open_audio_input
#define g_mus_sound_report_cache_w g_mus_sound_report_cache
#define g_sound_forget_w g_sound_forget
#define g_sound_prune_w g_sound_prune
#endif

#if HAVE_RUBY
static XEN sound_data_each(XEN obj)
{
  int i, j;
  sound_data *v;
  v = (sound_data *)XEN_OBJECT_REF(obj);
  for (j = 0; j < v->chans; j++)
    for (i = 0; i < v->length; i++)
      rb_yield(C_TO_XEN_DOUBLE(MUS_SAMPLE_TO_DOUBLE(v->data[j][i])));
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
      if (len > 0) return(C_TO_SMALL_XEN_INT(1));
    }
  return(C_TO_XEN_INT(-1));
}

static XEN sound_data_size(XEN obj)
{
  sound_data *sd;
  sd = (sound_data *)XEN_OBJECT_REF(obj);
  return(C_TO_XEN_INT(sd->length * sd->chans));
}

static XEN sound_data_fill(XEN obj, XEN val)
{
  sound_data *sd;
  mus_sample_t filler;
  int i, j, chans, len;
  filler = MUS_DOUBLE_TO_SAMPLE(XEN_TO_C_DOUBLE(val));
  sd = (sound_data *)XEN_OBJECT_REF(obj);
  for (i = 0; i < sd->chans; i++)
    for (j = 0; j < sd->length; j++)
      sd->data[i][j] = filler;
  return(val);
}

static XEN sound_data_dup(XEN obj)
{
  sound_data *sd, *sdnew;
  int i, j, chans, len;
  XEN result;
  sd = (sound_data *)XEN_OBJECT_REF(obj);
  result = make_sound_data(sd->chans, sd->length);
  sdnew = (sound_data *)XEN_OBJECT_REF(result);
  for (i = 0; i < sd->chans; i++)
    for (j = 0; j < sd->length; j++)
      sdnew->data[i][j] = sd->data[i][j];
  return(result);
}

#endif

#if HAVE_OSS
#define S_mus_audio_reinitialize "mus-audio-reinitialize"
static XEN g_mus_audio_reinitialize(void)
{
  #define H_mus_audio_reinitialize "(" S_mus_audio_reinitialize ") force audio device re-initialization"
  return(C_TO_XEN_INT(mus_audio_reinitialize()));
}
#endif

#if WITH_MODULES
static void sndlib2xen_init(void *ignore)
#else
void mus_sndlib2xen_initialize(void)
#endif
{
  mus_sound_initialize();

  sound_data_tag = XEN_MAKE_OBJECT_TYPE("SoundData", sizeof(sound_data));
#if HAVE_GUILE
  scm_set_smob_print(sound_data_tag, print_sound_data);
  scm_set_smob_free(sound_data_tag, free_sound_data);
  scm_set_smob_equalp(sound_data_tag, equalp_sound_data);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(sound_data_tag, XEN_PROCEDURE_CAST sound_data_apply, 2, 0, 0);
#endif
#endif
#if HAVE_RUBY
  rb_include_module(sound_data_tag, rb_mComparable);
  rb_include_module(sound_data_tag, rb_mEnumerable);
  rb_define_method(sound_data_tag, "to_s", XEN_PROCEDURE_CAST print_sound_data, 0);
  rb_define_method(sound_data_tag, "eql?", XEN_PROCEDURE_CAST equalp_sound_data, 1);
  rb_define_method(sound_data_tag, "==", XEN_PROCEDURE_CAST equalp_sound_data, 1);
  rb_define_method(sound_data_tag, "each", XEN_PROCEDURE_CAST sound_data_each, 0);
  rb_define_method(sound_data_tag, "<=>", XEN_PROCEDURE_CAST sound_data_compare, 1);
  rb_define_method(sound_data_tag, "[]", XEN_PROCEDURE_CAST sound_data_ref, 2);
  rb_define_method(sound_data_tag, "[]=", XEN_PROCEDURE_CAST sound_data_set, 3);
  rb_define_method(sound_data_tag, "length", XEN_PROCEDURE_CAST sound_data_size, 0);
  rb_define_method(sound_data_tag, "size", XEN_PROCEDURE_CAST sound_data_size, 0);
  rb_define_method(sound_data_tag, "new", XEN_PROCEDURE_CAST make_sound_data, 2);
  rb_define_method(sound_data_tag, "fill", XEN_PROCEDURE_CAST sound_data_fill, 1);
  rb_define_method(sound_data_tag, "dup", XEN_PROCEDURE_CAST sound_data_dup, 0);
#endif

  XEN_DEFINE_CONSTANT(S_mus_out_format, MUS_OUT_FORMAT, "sample format for fastest IO");

  XEN_DEFINE_CONSTANT(S_mus_next,    MUS_NEXT,    "NeXT (Sun) sound header id");
  XEN_DEFINE_CONSTANT(S_mus_aifc,    MUS_AIFC,    "AIFC sound header id");
  XEN_DEFINE_CONSTANT(S_mus_riff,    MUS_RIFF,    "RIFF (MS wave) sound header id");
  XEN_DEFINE_CONSTANT(S_mus_nist,    MUS_NIST,    "NIST (Sphere) sound header id");
  XEN_DEFINE_CONSTANT(S_mus_raw,     MUS_RAW,     "raw (headerless) sound header id");
  XEN_DEFINE_CONSTANT(S_mus_ircam,   MUS_IRCAM,   "IRCAM sound header id");
  XEN_DEFINE_CONSTANT(S_mus_aiff,    MUS_AIFF,    "AIFF (old-style) sound header id");
  XEN_DEFINE_CONSTANT(S_mus_bicsf,   MUS_BICSF,   "BICSF header id");
  XEN_DEFINE_CONSTANT(S_mus_voc,     MUS_VOC,     "VOC header id");
  XEN_DEFINE_CONSTANT(S_mus_svx,     MUS_SVX,     "SVX (IFF) header id");
  XEN_DEFINE_CONSTANT(S_mus_soundfont, MUS_SOUNDFONT, "soundfont header id");

  XEN_DEFINE_CONSTANT(S_mus_bshort,  MUS_BSHORT,  "big-endian short data format id");
  XEN_DEFINE_CONSTANT(S_mus_lshort,  MUS_LSHORT,  "little-endian short data format id");
  XEN_DEFINE_CONSTANT(S_mus_mulaw,   MUS_MULAW,   "mulaw (8-bit) data format id");
  XEN_DEFINE_CONSTANT(S_mus_alaw,    MUS_ALAW,    "alaw (8-bit) data format id");
  XEN_DEFINE_CONSTANT(S_mus_byte,    MUS_BYTE,    "signed byte data format id");
  XEN_DEFINE_CONSTANT(S_mus_ubyte,   MUS_UBYTE,   "unsigned byte data format id");
  XEN_DEFINE_CONSTANT(S_mus_bfloat,  MUS_BFLOAT,  "big-endian float data format id");
  XEN_DEFINE_CONSTANT(S_mus_lfloat,  MUS_LFLOAT,  "little-endian float data format id");
  XEN_DEFINE_CONSTANT(S_mus_bint,    MUS_BINT,    "big-endian int data format id");
  XEN_DEFINE_CONSTANT(S_mus_lint,    MUS_LINT,    "little-endian int data format id");
  XEN_DEFINE_CONSTANT(S_mus_bintn,   MUS_BINTN,   "normalized big-endian int data format id");
  XEN_DEFINE_CONSTANT(S_mus_lintn,   MUS_LINTN,   "normalized little-endian int data format id");
  XEN_DEFINE_CONSTANT(S_mus_b24int,  MUS_B24INT,  "big-endian 24-bit data format id");
  XEN_DEFINE_CONSTANT(S_mus_l24int,  MUS_L24INT,  "little-endian 24-bit data format id");
  XEN_DEFINE_CONSTANT(S_mus_bdouble, MUS_BDOUBLE, "big-endian double data format id");
  XEN_DEFINE_CONSTANT(S_mus_ldouble, MUS_LDOUBLE, "little-endian double data format id");
  XEN_DEFINE_CONSTANT(S_mus_ubshort, MUS_UBSHORT, "unsigned big-endian short data format id");
  XEN_DEFINE_CONSTANT(S_mus_ulshort, MUS_ULSHORT, "unsigned little-endian short data format id");
  XEN_DEFINE_CONSTANT(S_mus_bdouble_unscaled, MUS_BDOUBLE_UNSCALED, "unscaled big-endian double data format id");
  XEN_DEFINE_CONSTANT(S_mus_ldouble_unscaled, MUS_LDOUBLE_UNSCALED, "unscaled little-endian double data format id");
  XEN_DEFINE_CONSTANT(S_mus_bfloat_unscaled,  MUS_BFLOAT_UNSCALED,  "unscaled big-endian float data format id");
  XEN_DEFINE_CONSTANT(S_mus_lfloat_unscaled,  MUS_LFLOAT_UNSCALED,  "unscaled little-endian float data format id");

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
  XEN_DEFINE_CONSTANT(S_mus_audio_direction,      MUS_AUDIO_DIRECTION,      "audio sample flow direction (mus-audio-read)");
  XEN_DEFINE_CONSTANT(S_mus_audio_samples_per_channel, MUS_AUDIO_SAMPLES_PER_CHANNEL, "samples per channel (mus-audio-read)");

  XEN_DEFINE_CONSTANT(S_mus_audio_amp,     MUS_AUDIO_AMP,     "mixer amp field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_srate,   MUS_AUDIO_SRATE,   "mixer srate field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_channel, MUS_AUDIO_CHANNEL, "mixer channel field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_format,  MUS_AUDIO_FORMAT,  "mixer data format field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_port,    MUS_AUDIO_PORT,    "mixer port");
  XEN_DEFINE_CONSTANT(S_mus_audio_imix,    MUS_AUDIO_IMIX,    "mixer 'imix' field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_igain,   MUS_AUDIO_IGAIN,   "mixer 'igain' field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_reclev,  MUS_AUDIO_RECLEV,  "mixer 'reclev' field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_pcm,     MUS_AUDIO_PCM,     "mixer 'pcm' field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_pcm2,    MUS_AUDIO_PCM2,    "mixer 'pcm2' field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_ogain,   MUS_AUDIO_OGAIN,   "mixer 'ogain' field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_line,    MUS_AUDIO_LINE,    "mixer 'line' field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_synth,   MUS_AUDIO_SYNTH,   "mixer 'synth' field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_bass,    MUS_AUDIO_BASS,    "mixer 'bass' field id");
  XEN_DEFINE_CONSTANT(S_mus_audio_treble,  MUS_AUDIO_TREBLE,  "mixer 'treble' field id");

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_sound_samples, g_sound_samples_w, H_mus_sound_samples, 
				   "set-" S_mus_sound_samples, g_sound_set_samples, 1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_sound_data_location, g_sound_data_location_w, H_mus_sound_data_location,
				   "set-" S_mus_sound_data_location, g_sound_set_data_location, 1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_sound_chans, g_sound_chans_w, H_mus_sound_chans,
				   "set-" S_mus_sound_chans, g_sound_set_chans, 1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_sound_srate, g_sound_srate_w, H_mus_sound_srate,
				   "set-" S_mus_sound_srate, g_sound_set_srate, 1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_sound_header_type, g_sound_header_type_w, H_mus_sound_header_type,
				   "set-" S_mus_sound_header_type, g_sound_set_header_type, 1, 0, 2, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mus_sound_data_format, g_sound_data_format_w, H_mus_sound_data_format,
				   "set-" S_mus_sound_data_format, g_sound_set_data_format, 1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE(S_sound_data_length,        sound_data_length_w, 1, 0, 0,       H_sound_data_length);
  XEN_DEFINE_PROCEDURE(S_sound_data_chans,         sound_data_chans_w, 1, 0, 0,        H_sound_data_chans);
  XEN_DEFINE_PROCEDURE(S_make_sound_data,          g_make_sound_data_w, 2, 0, 0,       H_make_sound_data);
  XEN_DEFINE_PROCEDURE(S_sound_data_p,             g_sound_data_p_w, 1, 0, 0,          H_sound_data_p);
  XEN_DEFINE_PROCEDURE(S_sound_data_maxamp,        g_sound_data_maxamp_w, 1, 0, 0,     H_sound_data_maxamp);
  XEN_DEFINE_PROCEDURE(S_sound_data2vct,           sound_data2vct_w, 1, 2, 0,          H_sound_data2vct);
  XEN_DEFINE_PROCEDURE(S_vct2sound_data,           vct2sound_data_w, 1, 2, 0,          H_vct2sound_data);
  XEN_DEFINE_PROCEDURE(S_mus_sound_frames,         g_sound_frames_w, 1, 0, 0,          H_mus_sound_frames);
  XEN_DEFINE_PROCEDURE(S_mus_sound_duration,       g_sound_duration_w, 1, 0, 0,        H_mus_sound_duration);
  XEN_DEFINE_PROCEDURE(S_mus_sound_datum_size,     g_sound_datum_size_w, 1, 0, 0,      H_mus_sound_datum_size);
  XEN_DEFINE_PROCEDURE(S_mus_sound_length,         g_sound_length_w, 1, 0, 0,          H_mus_sound_length);
  XEN_DEFINE_PROCEDURE(S_mus_sound_type_specifier, g_sound_type_specifier_w, 1, 0, 0,  H_mus_sound_type_specifier);
  XEN_DEFINE_PROCEDURE(S_mus_header_type_name,     g_sound_type_name_w, 1, 0, 0,       H_mus_header_type_name);
  XEN_DEFINE_PROCEDURE(S_mus_data_format_name,     g_sound_format_name_w, 1, 0, 0,     H_mus_data_format_name);
  XEN_DEFINE_PROCEDURE(S_mus_sound_comment,        g_sound_comment_w, 1, 0, 0,         H_mus_sound_comment);
  XEN_DEFINE_PROCEDURE(S_mus_sound_write_date,     g_sound_write_date_w, 1, 0, 0,      H_mus_sound_write_date);
  XEN_DEFINE_PROCEDURE(S_mus_data_format_bytes_per_sample, g_sound_bytes_per_sample_w, 1, 0, 0, H_mus_data_format_bytes_per_sample);
  XEN_DEFINE_PROCEDURE(S_mus_sound_loop_info,      g_sound_loop_info_w, 1, 0, 0,       H_mus_sound_loop_info);
  XEN_DEFINE_PROCEDURE(S_mus_sound_maxamp,         g_sound_maxamp_w, 1, 0, 0,          H_mus_sound_maxamp);
  XEN_DEFINE_PROCEDURE(S_mus_sound_set_maxamp,     g_sound_set_maxamp_w, 2, 0, 0,      H_mus_sound_set_maxamp);
  XEN_DEFINE_PROCEDURE(S_mus_sound_maxamp_exists,  g_sound_maxamp_exists_w, 1, 0, 0,   H_mus_sound_maxamp_exists);
  XEN_DEFINE_PROCEDURE(S_mus_sound_forget,         g_sound_forget, 1, 0, 0,            H_mus_sound_forget);
  XEN_DEFINE_PROCEDURE(S_mus_sound_prune,          g_sound_prune, 0, 0, 0,             H_mus_sound_prune);

  XEN_DEFINE_PROCEDURE(S_mus_audio_report,         g_report_audio_state_w, 0, 0, 0,    H_mus_audio_report);
  XEN_DEFINE_PROCEDURE(S_mus_audio_sun_outputs,    g_audio_outputs_w, 3, 0, 0,         H_mus_audio_sun_outputs);
  XEN_DEFINE_PROCEDURE(S_mus_sound_open_input,     g_open_sound_input_w, 1, 0, 0,      H_mus_sound_open_input);
  XEN_DEFINE_PROCEDURE(S_mus_sound_close_input,    g_close_sound_input_w, 1, 0, 0,     H_mus_sound_close_input);
  XEN_DEFINE_PROCEDURE(S_mus_audio_close,          g_close_audio_w, 1, 0, 0,           H_mus_audio_close);
  XEN_DEFINE_PROCEDURE(S_mus_audio_save,           g_save_audio_state_w, 0, 0, 0,      H_mus_audio_save);
  XEN_DEFINE_PROCEDURE(S_mus_audio_restore,        g_restore_audio_state_w, 0, 0, 0,   H_mus_audio_restore);
  XEN_DEFINE_PROCEDURE(S_mus_audio_systems,        g_audio_systems_w, 0, 0, 0,         H_mus_audio_systems);
  XEN_DEFINE_PROCEDURE(S_mus_audio_mixer_read,     g_mus_audio_mixer_read_w, 4, 0, 0,  H_mus_audio_mixer_read);
  XEN_DEFINE_PROCEDURE(S_mus_audio_mixer_write,    g_mus_audio_mixer_write_w, 4, 0, 0, H_mus_audio_mixer_write);
  XEN_DEFINE_PROCEDURE(S_mus_file_set_data_clipped, g_mus_set_data_clipped_w, 2, 0, 0, H_mus_file_set_data_clipped);
  XEN_DEFINE_PROCEDURE(S_mus_file_prescaler,       g_mus_prescaler_w, 1, 0, 0,         H_mus_file_prescaler);
  XEN_DEFINE_PROCEDURE(S_mus_file_set_prescaler,   g_mus_set_prescaler_w, 2, 0, 0,     H_mus_file_set_prescaler);
  XEN_DEFINE_PROCEDURE(S_mus_expand_filename,      g_mus_expand_filename_w, 1, 0, 0,   H_mus_expand_filename);
  XEN_DEFINE_PROCEDURE(S_mus_audio_write,          g_write_audio_w, 3, 0, 0,           H_mus_audio_write);
  XEN_DEFINE_PROCEDURE(S_mus_audio_read,           g_read_audio_w, 3, 0, 0,            H_mus_audio_read);
  XEN_DEFINE_PROCEDURE(S_mus_sound_open_output,    g_open_sound_output_w, 5, 1, 0,     H_mus_sound_open_output);
  XEN_DEFINE_PROCEDURE(S_mus_sound_reopen_output,  g_reopen_sound_output_w, 5, 0, 0,   H_mus_sound_reopen_output);
  XEN_DEFINE_PROCEDURE(S_mus_sound_close_output,   g_close_sound_output_w, 2, 0, 0,    H_mus_sound_close_output);
  XEN_DEFINE_PROCEDURE(S_mus_sound_read,           g_read_sound_w, 5, 0, 0,            H_mus_sound_read);
  XEN_DEFINE_PROCEDURE(S_mus_sound_write,          g_write_sound_w, 5, 0, 0,           H_mus_sound_write);
  XEN_DEFINE_PROCEDURE(S_mus_sound_seek,           g_seek_sound_w, 3, 0, 0,            H_mus_sound_seek);
  XEN_DEFINE_PROCEDURE(S_mus_sound_seek_frame,     g_seek_sound_frame_w, 2, 0, 0,      H_mus_sound_seek_frame);
  XEN_DEFINE_PROCEDURE(S_mus_audio_open_output,    g_open_audio_output_w, 5, 0, 0,     H_mus_audio_open_output);
  XEN_DEFINE_PROCEDURE(S_mus_audio_open_input,     g_open_audio_input_w, 5, 0, 0,      H_mus_audio_open_input);
  XEN_DEFINE_PROCEDURE(S_mus_sound_report_cache,   g_mus_sound_report_cache_w, 0, 1, 0,H_mus_sound_report_cache);

#if HAVE_GUILE
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_sound_data_ref, sound_data_ref_w, H_sound_data_ref,
				   "set-" S_sound_data_ref, sound_data_set_w,  3, 0, 4, 0);
#else
  XEN_DEFINE_PROCEDURE(S_sound_data_ref,           sound_data_ref_w, 3, 0, 0,          H_sound_data_ref);
#endif
  XEN_DEFINE_PROCEDURE(S_sound_data_setB,          sound_data_set_w, 4, 0, 0,          H_sound_data_setB);

#if HAVE_OSS
  XEN_DEFINE_PROCEDURE(S_mus_audio_reinitialize,   g_mus_audio_reinitialize, 0, 0, 0,  H_mus_audio_reinitialize);
#endif

  XEN_YES_WE_HAVE("sndlib");

#if WITH_MODULES
  scm_c_export(S_mus_out_format,
	       S_mus_next,
	       S_mus_aifc,
	       S_mus_riff,
	       S_mus_nist,
	       S_mus_raw,
	       S_mus_ircam,
	       S_mus_aiff,
	       S_mus_bicsf,
	       S_mus_voc,
	       S_mus_svx,
	       S_mus_soundfont,
	       S_mus_bshort,
	       S_mus_lshort,
	       S_mus_mulaw,
	       S_mus_alaw,
	       S_mus_byte,
	       S_mus_ubyte,
	       S_mus_bfloat,
	       S_mus_lfloat,
	       S_mus_bint,
	       S_mus_lint,
	       S_mus_bintn,
	       S_mus_lintn,
	       S_mus_b24int,
	       S_mus_l24int,
	       S_mus_bdouble,
	       S_mus_ldouble,
	       S_mus_ubshort,
	       S_mus_ulshort,
	       S_mus_bdouble_unscaled,
	       S_mus_ldouble_unscaled,
	       S_mus_bfloat_unscaled,
	       S_mus_lfloat_unscaled,
	       S_mus_audio_default,
	       S_mus_audio_duplex_default,
	       S_mus_audio_line_out,
	       S_mus_audio_line_in,
	       S_mus_audio_microphone,
	       S_mus_audio_speakers,
	       S_mus_audio_dac_out,
	       S_mus_audio_adat_in,
	       S_mus_audio_aes_in,
	       S_mus_audio_digital_in,
	       S_mus_audio_digital_out,
	       S_mus_audio_adat_out,
	       S_mus_audio_aes_out,
	       S_mus_audio_dac_filter,
	       S_mus_audio_mixer,
	       S_mus_audio_line1,
	       S_mus_audio_line2,
	       S_mus_audio_line3,
	       S_mus_audio_aux_input,
	       S_mus_audio_cd,
	       S_mus_audio_aux_output,
	       S_mus_audio_spdif_in,
	       S_mus_audio_spdif_out,
	       S_mus_audio_direction,
	       S_mus_audio_samples_per_channel,
	       S_mus_audio_amp,
	       S_mus_audio_srate,
	       S_mus_audio_channel,
	       S_mus_audio_format,
	       S_mus_audio_port,
	       S_mus_audio_imix,
	       S_mus_audio_igain,
	       S_mus_audio_reclev,
	       S_mus_audio_pcm,
	       S_mus_audio_pcm2,
	       S_mus_audio_ogain,
	       S_mus_audio_line,
	       S_mus_audio_synth,
	       S_mus_audio_bass,
	       S_mus_audio_treble,
	       S_mus_sound_samples,
	       S_mus_sound_data_location,
	       S_mus_sound_chans,
	       S_mus_sound_srate,
	       S_mus_sound_header_type,
	       S_mus_sound_data_format,
	       S_sound_data_length,
	       S_sound_data_chans,
	       S_make_sound_data,
	       S_sound_data_p,
	       S_sound_data_maxamp,
	       S_sound_data2vct,
	       S_vct2sound_data,
	       S_mus_sound_frames,
	       S_mus_sound_duration,
	       S_mus_sound_datum_size,
	       S_mus_sound_length,
	       S_mus_sound_type_specifier,
	       S_mus_header_type_name,
	       S_mus_data_format_name,
	       S_mus_sound_comment,
	       S_mus_sound_write_date,
	       S_mus_data_format_bytes_per_sample,
	       S_mus_sound_loop_info,
	       S_mus_sound_maxamp,
	       S_mus_sound_set_maxamp,
	       S_mus_sound_maxamp_exists,
	       S_mus_sound_forget,
	       S_mus_sound_prune,
	       S_mus_audio_report,
	       S_mus_audio_sun_outputs,
	       S_mus_sound_open_input,
	       S_mus_sound_close_input,
	       S_mus_audio_close,
	       S_mus_audio_save,
	       S_mus_audio_restore,
	       S_mus_audio_systems,
	       S_mus_audio_mixer_read,
	       S_mus_audio_mixer_write,
	       S_mus_file_set_data_clipped,
	       S_mus_file_prescaler,
	       S_mus_file_set_prescaler,
	       S_mus_expand_filename,
	       S_mus_audio_write,
	       S_mus_audio_read,
	       S_mus_sound_open_output,
	       S_mus_sound_reopen_output,
	       S_mus_sound_close_output,
	       S_mus_sound_read,
	       S_mus_sound_write,
	       S_mus_sound_seek,
	       S_mus_sound_seek_frame,
	       S_mus_audio_open_output,
	       S_mus_audio_open_input,
	       S_mus_sound_report_cache,
	       S_sound_data_ref,
	       S_sound_data_ref,
	       S_sound_data_setB,
#if HAVE_OSS
	       S_mus_audio_reinitialize,
#endif
	       NULL);
#endif
}

#if WITH_MODULES
void mus_sndlib2xen_initialize(void)
{
  scm_c_define_module("snd sndlib", sndlib2xen_init, NULL);
}
#endif
