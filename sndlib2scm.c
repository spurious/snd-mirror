/* Tie sndlib into guile (Scheme) */

#if defined(HAVE_CONFIG_H)
  #include "config.h"
#endif

#include <stddef.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#if USE_SND
  #include "snd.h"
#else
  #define PRINT_BUFFER_SIZE 512
  #define LABEL_BUFFER_SIZE 64
#endif

#include "sndlib.h"
#include "sndlib-strings.h"

#if (!USE_SND)
#if HAVE_GUILE
  #include <guile/gh.h>
  #include "sg.h"
#endif
#if HAVE_LIBREP 
  #include <rep.h>
  #include "sl.h"
#endif
#if HAVE_MZSCHEME
  #include <scheme.h>
  #include "sz.h"
#endif
#if (!HAVE_EXTENSION_LANGUAGE)
  #include "noguile.h"
#endif
#endif

#include "vct.h"
#include "sndlib2scm.h"


#if (!USE_SND)
int to_c_int_or_else(SCM obj, int fallback, char *origin)
{
  /* don't want errors here about floats with non-zero fractions etc */
  if (INTEGER_P(obj))
    return(TO_C_INT(obj));
  else
    if (NUMBER_P(obj))
      return((int)TO_C_DOUBLE_WITH_ORIGIN(obj, origin));
  return(fallback);
}

void define_procedure_with_setter(char *get_name, SCM (*get_func)(), char *get_help,
				  char *set_name, SCM (*set_func)(), 
				  SCM local_doc,
				  int get_req, int get_opt, int set_req, int set_opt)
{
#if HAVE_GUILE
#if HAVE_SCM_C_DEFINE
  SCM str;
  str = TO_SCM_STRING(get_help);
  scm_permanent_object(
    scm_c_define(get_name,
      scm_make_procedure_with_setter(
        gh_new_procedure("", SCM_FNC get_func, get_req, get_opt, 0),
	gh_new_procedure(set_name, SCM_FNC set_func, set_req, set_opt, 0))));
  scm_set_object_property_x(TO_SCM_SYMBOL(get_name), local_doc, str);
  scm_set_procedure_property_x(SND_LOOKUP(get_name), local_doc, str);
#else
  scm_set_object_property_x(
    SCM_CDR(
      gh_define(get_name,
	scm_make_procedure_with_setter(
          gh_new_procedure("", SCM_FNC get_func, get_req, get_opt, 0),
	  gh_new_procedure(set_name, SCM_FNC set_func, set_req, set_opt, 0)
	  ))),
    local_doc,
    TO_SCM_STRING(get_help));
  /* still need to trap help output and send it to the listener */
#endif
#endif
#if HAVE_LIBREP
  DEFINE_PROC(get_name, get_func, get_req, get_opt, 0, get_help);
  DEFINE_PROC(set_name, set_func, set_req, set_opt, 0, get_help);
#endif
}
#endif

void mus_misc_error(const char *caller, char *msg, SCM val)
{
  if (msg)
    ERROR(MUS_MISC_ERROR,
	  SCM_LIST3(TO_SCM_STRING(caller),
		    TO_SCM_STRING(msg),
		    val));
  else
    ERROR(MUS_MISC_ERROR,
	  SCM_LIST2(TO_SCM_STRING(caller),
		    val));
}

static SCM g_sound_loop_info(SCM filename)
{
  #define H_mus_sound_loop_info "(" S_mus_sound_loop_info " filename) -> loop info for sound as a list (start1 end1 start2 end2 base-note base-detune)"
  int *res;
  char *tmpstr;
  SCM sres = SCM_EOL;
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARGn, S_mus_sound_loop_info, "a string"); 
  tmpstr = mus_expand_filename(TO_C_STRING(filename));
  res = mus_sound_loop_info(tmpstr);
  if (tmpstr) FREE(tmpstr);
  if (res)
    {
      sres = SCM_LIST6(TO_SCM_INT(res[0]), TO_SCM_INT(res[1]), TO_SCM_INT(res[2]),
		       TO_SCM_INT(res[3]), TO_SCM_INT(res[4]), TO_SCM_INT(res[5]));
      FREE(res);
    }
  return(sres);
}

static SCM g_sound_samples(SCM filename) 
{
  #define H_mus_sound_samples "(" S_mus_sound_samples " filename) -> samples (frames*channels) in sound"
  char *tmpstr = NULL;
  int res;
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARGn, S_mus_sound_samples, "a string"); 
  res = mus_sound_samples(tmpstr = mus_expand_filename(TO_C_STRING(filename)));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(res));
}

static SCM g_sound_frames(SCM filename) 
{
  #define H_mus_sound_frames "(" S_mus_sound_frames " filename) -> frames (samples/channel) in sound"
  char *tmpstr = NULL;
  int res;
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARGn, S_mus_sound_frames, "a string"); 
  res = mus_sound_frames(tmpstr = mus_expand_filename(TO_C_STRING(filename)));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(res));
}

static SCM g_sound_datum_size(SCM filename) 
{
  #define H_mus_sound_datum_size "(" S_mus_sound_datum_size " filename) -> bytes per sample of data in sound"
  char *tmpstr = NULL;
  int res;
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARGn, S_mus_sound_datum_size, "a string"); 
  res = mus_sound_datum_size(tmpstr = mus_expand_filename(TO_C_STRING(filename)));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(res));
}

static SCM g_sound_data_location(SCM filename) 
{
  #define H_mus_sound_data_location "(" S_mus_sound_data_location " filename) -> location (bytes) of first sample of sound data"
  char *tmpstr = NULL;
  int res;
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARGn, S_mus_sound_data_location, "a string"); 
  res = mus_sound_data_location(tmpstr = mus_expand_filename(TO_C_STRING(filename)));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(res));
}

static SCM g_sound_chans(SCM filename) 
{
  #define H_mus_sound_chans "(" S_mus_sound_chans " filename) -> channels of sound"
  char *tmpstr = NULL;
  int res;
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARGn, S_mus_sound_chans, "a string"); 
  res = mus_sound_chans(tmpstr = mus_expand_filename(TO_C_STRING(filename)));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(res));
}

static SCM g_sound_srate(SCM filename) 
{
  #define H_mus_sound_srate "(" S_mus_sound_srate " filename) -> sampling rate of sound"
  char *tmpstr = NULL;
  int res;
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARGn, S_mus_sound_srate, "a string"); 
  res = mus_sound_srate(tmpstr = mus_expand_filename(TO_C_STRING(filename)));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(res));
}

static SCM g_sound_header_type(SCM filename) 
{
  #define H_mus_sound_header_type "(" S_mus_sound_header_type " filename) -> header type (e.g. AIFF) of sound"
  char *tmpstr = NULL;
  int res;
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARGn, S_mus_sound_header_type, "a string"); 
  res = mus_sound_header_type(tmpstr = mus_expand_filename(TO_C_STRING(filename)));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(res));
}

static SCM g_sound_data_format(SCM filename) 
{
  #define H_mus_sound_data_format "(" S_mus_sound_data_format " filename) -> data format (e.g. big endian short) of sound"
  char *tmpstr = NULL;
  int res;
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARGn, S_mus_sound_data_format, "a string"); 
  res = mus_sound_data_format(tmpstr = mus_expand_filename(TO_C_STRING(filename)));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(res));
}

static SCM g_sound_length(SCM filename) 
{
  #define H_mus_sound_length "(" S_mus_sound_length " filename) -> file length in bytes"
  char *tmpstr = NULL;
  int res;
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARGn, S_mus_sound_length, "a string"); 
  res = mus_sound_length(tmpstr = mus_expand_filename(TO_C_STRING(filename)));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(res));
}

static SCM g_sound_type_specifier(SCM filename) 
{
  #define H_mus_sound_type_specifier "(" S_mus_sound_type_specifier " filename) -> original file type identifier (e.g. 0x2e736e64)"
  char *tmpstr = NULL;
  int res;
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARGn, S_mus_sound_type_specifier, "a string"); 
  res = mus_sound_type_specifier(tmpstr = mus_expand_filename(TO_C_STRING(filename)));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(res));
}

static SCM g_sound_comment(SCM filename) 
{
  #define H_mus_sound_comment "(" S_mus_sound_comment " filename) -> comment (string) found in sound's header"
  char *tmpstr = NULL, *res = NULL; 
  SCM newstr;
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARGn, S_mus_sound_comment, "a string"); 
  res = mus_sound_comment(tmpstr = mus_expand_filename(TO_C_STRING(filename)));
  if (tmpstr) FREE(tmpstr);
  newstr = TO_SCM_STRING(res);
  if (res) FREE(res);
  return(newstr);
}

static SCM g_sound_write_date(SCM filename) 
{
  #define H_mus_sound_write_date "(" S_mus_sound_write_date " filename) -> write_date of sound"
  char *tmpstr = NULL;
  int date;
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARGn, S_mus_sound_write_date, "a string"); 
  date = mus_sound_write_date(tmpstr = mus_expand_filename(TO_C_STRING(filename)));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(date));
}

static SCM g_sound_type_name(SCM type) 
{
  #define H_mus_header_type_name "(" S_mus_header_type_name " type) -> header type (e.g. mus-aiff) as a string"
  ASSERT_TYPE(INTEGER_P(type), type, SCM_ARGn, S_mus_header_type_name, "an integer (header-type id)"); 
  return(TO_SCM_STRING(mus_header_type_name(TO_C_INT(type))));
}

static SCM g_sound_format_name(SCM format) 
{
  #define H_mus_data_format_name "(" S_mus_data_format_name " format) -> data format (e.g. mus-bshort) as a string"
  ASSERT_TYPE(INTEGER_P(format), format, SCM_ARGn, S_mus_data_format_name, "an integer (data-format id)"); 
  return(TO_SCM_STRING(mus_data_format_name(TO_C_INT(format))));
}

static SCM g_sound_bytes_per_sample(SCM format) 
{
  #define H_mus_data_format_bytes_per_sample "(" S_mus_data_format_bytes_per_sample " format) -> number of bytes per sample in format (e.g. mus-short = 2)"
  ASSERT_TYPE(INTEGER_P(format), format, SCM_ARGn, S_mus_data_format_bytes_per_sample, "an integer (data-format id)"); 
  return(TO_SCM_INT(mus_data_format_to_bytes_per_sample(TO_C_INT(format))));
}

static SCM g_report_audio_state(void) 
{
  #define H_mus_audio_report "(" S_mus_audio_report ") -> string describing current audio hardware setup"
  return(TO_SCM_STRING(mus_audio_report()));
}

static SCM g_sound_duration(SCM filename) 
{
  #define H_mus_sound_duration "(" S_mus_sound_duration " filename) -> duration (seconds) of sound"
  char *tmpstr = NULL;
  float res;
  ASSERT_TYPE(STRING_P(filename), filename, SCM_ARGn, S_mus_sound_duration, "a string"); 
  res = mus_sound_duration(tmpstr = mus_expand_filename(TO_C_STRING(filename)));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_DOUBLE(res));

}

static SCM g_audio_outputs(SCM speakers, SCM headphones, SCM line_out)
{
  #define H_mus_audio_sun_outputs "(" S_mus_audio_sun_outputs " speaker headphones line-out) sets the current Sun audio outputs"
#ifdef SUN
  ASSERT_TYPE(INTEGER_P(speakers), speakers, SCM_ARG1, S_mus_audio_sun_outputs, "an integer");
  ASSERT_TYPE(INTEGER_P(headphones), headphones, SCM_ARG2, S_mus_audio_sun_outputs, "an integer");
  ASSERT_TYPE(INTEGER_P(line_out), line_out, SCM_ARG3, S_mus_audio_sun_outputs, "an integer");
  mus_audio_sun_outputs(TO_C_INT(speakers),
			TO_C_INT(headphones),
			TO_C_INT(line_out));
#endif
  return(SCM_BOOL_F);
}

static SCM g_sound_max_amp_exists(SCM file)
{
  #define H_mus_sound_max_amp_exists "(" S_mus_sound_max_amp_exists " filename) -> max amps available for sound"
  int val;
  char *filename;
  ASSERT_TYPE(STRING_P(file), file, SCM_ARGn, S_mus_sound_max_amp, "a string");
  filename = mus_expand_filename(TO_C_STRING(file));
  val = mus_sound_max_amp_exists(filename);
  if (filename) FREE(filename);
  return(TO_SCM_BOOLEAN(val));
}

static SCM g_sound_max_amp(SCM file)
{
  #define H_mus_sound_max_amp "(" S_mus_sound_max_amp " filename) -> max amps in sound (a list of amps and locations)"
  int chans, rtn, i;
  MUS_SAMPLE_TYPE *vals;
  char *filename;
  SCM res = SCM_EOL;
  ASSERT_TYPE(STRING_P(file), file, SCM_ARGn, S_mus_sound_max_amp, "a string");
  filename = mus_expand_filename(TO_C_STRING(file));
  chans = mus_sound_chans(filename);
  if (chans > 0)
    {
      vals = (MUS_SAMPLE_TYPE *)CALLOC(chans * 2, sizeof(MUS_SAMPLE_TYPE));
      rtn = mus_sound_max_amp(filename, vals);
      if (rtn > 0)
	for (i = (chans * 2) - 2; i >= 0; i -= 2)
	  res = CONS(TO_SCM_INT((int)(vals[i])),
		  CONS(TO_SCM_DOUBLE(MUS_SAMPLE_TO_FLOAT(vals[i + 1])), res));
      FREE(vals);
    }
  if (filename) FREE(filename);
  return(res);
}

static SCM g_sound_set_max_amp(SCM file, SCM vals)
{
  #define H_mus_sound_set_max_amp "(" S_mus_sound_set_max_amp " filename vals) -> set max amps for sound (vals is a list of amps and locations)"
  int i, chans, len;
  MUS_SAMPLE_TYPE *mvals;
  char *filename;
  SCM lst;
  ASSERT_TYPE(STRING_P(file), file, SCM_ARG1, S_mus_sound_set_max_amp, "a string");
  ASSERT_TYPE(LIST_P(vals), vals, SCM_ARG2, S_mus_sound_set_max_amp, "a list");
  filename = mus_expand_filename(TO_C_STRING(file));
  chans = mus_sound_chans(filename);
  if (chans > 0)
    {
      len = LIST_LENGTH(vals);
      if (len < (chans * 2))
	mus_misc_error(S_mus_sound_set_max_amp, "max amp list wrong length", vals);
      if (len > chans * 2) len = chans * 2;
      mvals = (MUS_SAMPLE_TYPE *)CALLOC(chans * 2, sizeof(MUS_SAMPLE_TYPE));
      for (i = 0, lst = vals; i < len; i += 2, lst = SCM_CDDR(lst))
	{
	  mvals[i] = MUS_INT_TO_SAMPLE(TO_C_INT_OR_ELSE(SCM_CAR(lst), 0));
	  mvals[i + 1] = MUS_DOUBLE_TO_SAMPLE(TO_C_DOUBLE(SCM_CADR(lst)));
	}
      mus_sound_set_max_amp(filename, mvals);
      FREE(mvals);
    }
  FREE(filename);
  return(vals);
}

/* to support the actual sound file/audio port stuff, we need an "smob" for the int** arrays */

static SND_TAG_TYPE sound_data_tag = 0;
int sound_data_p(SCM obj) {return(SMOB_TYPE_P(obj, sound_data_tag));}
#define SOUND_DATA_P(Obj) SMOB_TYPE_P(Obj, sound_data_tag)

static SCM g_sound_data_p(SCM obj) 
{
  #define H_sound_data_p "(" S_sound_data_p " obj) -> #t if obj is a sound-data object, else #f"
  return(TO_SCM_BOOLEAN(sound_data_p(obj)));
}

static MUS_SAMPLE_TYPE **get_sound_data(SCM arg)
{
  sound_data *sd;
  if (SOUND_DATA_P(arg))
    {
      sd = (sound_data *)(SND_VALUE_OF(arg));
      return(sd->data);
    }
  return(NULL);
}

static scm_sizet free_sound_data(SCM obj)
{
  int i;
  sound_data *v = (sound_data *)SND_VALUE_OF(obj);
  if (v == NULL) return(0);
  if (v->data) 
    {
      for (i = 0; i < v->chans; i++) if (v->data[i]) FREE(v->data[i]);
      FREE(v->data);
    }
  v->data = NULL;
  v->chans = 0;
  free(v);
  return(sizeof(sound_data));
}

static int print_sound_data(SCM obj, SCM port, scm_print_state *pstate)
{
  char *buf;
  sound_data *v = (sound_data *)SND_VALUE_OF(obj);
  if (v == NULL)
    WRITE_STRING("#<sound-data: null>", port);
  else
    {
      if ((v->data) && (v->chans > 0))
	{
	  buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
	  mus_snprintf(buf, PRINT_BUFFER_SIZE, "#<sound-data: %d chan%s, %d frame%s>",
		       v->chans, (v->chans == 1) ? "" : "s",
		       v->length, (v->length == 1) ? "" : "s");
	  WRITE_STRING(buf, port);
	  FREE(buf);
	}
      else WRITE_STRING("#<sound-data: inactive>", port);
    }
#if HAVE_SCM_REMEMBER_UPTO_HERE
  scm_remember_upto_here(obj);
#endif
  return(1);
}

static SCM equalp_sound_data(SCM obj1, SCM obj2)
{
  int i, chn;
  sound_data *v1, *v2;
  v1 = (sound_data *)SND_VALUE_OF(obj1);
  v2 = (sound_data *)SND_VALUE_OF(obj2);
  if (v1 == v2) return(SCM_BOOL_T);
  if ((v1) && (v2) &&
      (v1->chans == v2->chans) &&
      (v1->length == v2->length))
    {
      for (chn = 0; chn < v1->chans; chn++)
	for (i = 0; i < v1->length; i++)
	  if (v1->data[chn][i] != v2->data[chn][i])
	    return(SCM_BOOL_F);
      return(SCM_BOOL_T);
    }
  return(scm_return_first(SCM_BOOL_F, obj1, obj2));
}

static SCM sound_data_length(SCM obj)
{
  #define H_sound_data_length "(" S_sound_data_length " sd) -> length (samples) of each channel of sound-data object sd"
  sound_data *v;
  ASSERT_TYPE(SOUND_DATA_P(obj), obj, SCM_ARGn, S_sound_data_length, "a sound-data object");
  v = (sound_data *)SND_VALUE_OF(obj);
  if (v == NULL) return(SCM_BOOL_F);
  return(TO_SCM_INT(v->length));
}

static SCM sound_data_chans(SCM obj)
{
  #define H_sound_data_chans "(" S_sound_data_chans " sd) -> number of channels in sound-data object sd"
  sound_data *v;
  ASSERT_TYPE(SOUND_DATA_P(obj), obj, SCM_ARGn, S_sound_data_chans, "a sound-data object");
  v = (sound_data *)SND_VALUE_OF(obj);
  if (v == NULL) return(SCM_BOOL_F);
  return(TO_SMALL_SCM_INT(v->chans));
}

SCM make_sound_data(int chans, int frames)
{
  #define H_make_sound_data "(" S_make_sound_data " chans frames) -> new sound-data object with chans channels, each having frames samples"
  int i;
  sound_data *new_sound_data;
  new_sound_data = (sound_data *)scm_must_malloc(sizeof(sound_data), S_make_sound_data);
  new_sound_data->length = frames;
  new_sound_data->chans = chans;
  new_sound_data->data = (MUS_SAMPLE_TYPE **)CALLOC(chans, sizeof(MUS_SAMPLE_TYPE *));
  for (i = 0; i < chans; i++)
    new_sound_data->data[i] = (MUS_SAMPLE_TYPE *)CALLOC(frames, sizeof(MUS_SAMPLE_TYPE));
  SND_RETURN_NEWSMOB(sound_data_tag, new_sound_data);
}

static SCM g_make_sound_data(SCM chans, SCM frames)
{
  int chns, frms;
  ASSERT_TYPE(INTEGER_P(chans), chans, SCM_ARG1, S_make_sound_data, "an integer");
  ASSERT_TYPE(INTEGER_P(frames), frames, SCM_ARG2, S_make_sound_data, "an integer");
  chns = TO_C_INT(chans);
  frms = TO_C_INT(frames);
  if (chns <= 0)
    mus_misc_error(S_make_sound_data, "chans <= 0?", chans);
  if (frms <= 0)
    mus_misc_error(S_make_sound_data, "frames <= 0?", frames);
  return(make_sound_data(chns, frms));
			 
}

static SCM sound_data_ref(SCM obj, SCM chan, SCM frame_num)
{
  #define H_sound_data_ref "(" S_sound_data_ref " sd chan i) -> sample in channel chan at location i of sound-data object sd: sd[chan][i]"
  sound_data *v;
  int loc, chn;
  ASSERT_TYPE(SOUND_DATA_P(obj), obj, SCM_ARG1, S_sound_data_ref, "a sound-data object");
  ASSERT_TYPE(INTEGER_P(chan), chan, SCM_ARG2, S_sound_data_ref, "an integer");
  ASSERT_TYPE(INTEGER_P(frame_num), frame_num, SCM_ARG3, S_sound_data_ref, "an integer");
  v = (sound_data *)SND_VALUE_OF(obj);
  if (v)
    {
      chn = TO_C_INT(chan);
      if ((chn < 0) || (chn >= v->chans))
	mus_misc_error(S_sound_data_ref, "invalid channel", SCM_LIST2(obj, chan));
      loc = TO_C_INT(frame_num);
      if ((loc < 0) || (loc >= v->length))
	mus_misc_error(S_sound_data_ref, "invalid frame number", SCM_LIST2(obj, frame_num));
      return(TO_SCM_DOUBLE(MUS_SAMPLE_TO_DOUBLE(v->data[chn][loc])));
    }
  else mus_misc_error(S_sound_data_ref, "nil sound-data?", SCM_EOL);
  return(TO_SCM_DOUBLE(0.0));
}

static SCM sound_data_apply(SCM obj, SCM chan, SCM i)
{
#if DEBUGGING
  /* if an error is signalled by sound_data_ref, our catch is ignored! */
  if ((INTEGER_P(chan)) && (INTEGER_P(i)))
    return(sound_data_ref(obj, chan, i));
  return(INTEGER_ZERO);
#else
  return(sound_data_ref(obj, chan, i));
#endif
}

static SCM sound_data_set(SCM obj, SCM chan, SCM frame_num, SCM val)
{
  #define H_sound_data_setB "(" S_sound_data_setB " sd chan i val): set sound-data object sd's i-th element in channel chan to val: sd[chan][i] = val"
  sound_data *v;
  int loc, chn;
  ASSERT_TYPE(SOUND_DATA_P(obj), obj, SCM_ARG1, S_sound_data_setB, "a sound-data object");
  ASSERT_TYPE(INTEGER_P(chan), chan, SCM_ARG2, S_sound_data_setB, "an integer");
  ASSERT_TYPE(INTEGER_P(frame_num), frame_num, SCM_ARG3, S_sound_data_setB, "an integer");
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARG4, S_sound_data_setB, "a number");
  v = (sound_data *)SND_VALUE_OF(obj);
  if (v)
    {
      chn = TO_C_INT(chan);
      if ((chn < 0) || (chn >= v->chans))
	mus_misc_error(S_sound_data_setB, "invalid channel", SCM_LIST3(obj, chan, frame_num));
      loc = TO_C_INT(frame_num);
      if ((loc < 0) || (loc >= v->length))
	mus_misc_error(S_sound_data_setB, "invalid frame number", SCM_LIST3(obj, chan, frame_num));
      v->data[chn][loc] = MUS_DOUBLE_TO_SAMPLE(TO_C_DOUBLE(val));
    }
  else mus_misc_error(S_sound_data_setB, "nil sound-data?", SCM_EOL);
  return(val);
}

static SCM sound_data2vct(SCM sdobj, SCM chan, SCM vobj)
{
  #define H_sound_data2vct "(" S_sound_data2vct " sd chan v) places sound-data object sd's channel chan data into vct object v"
  vct *v;
  sound_data *sd;
  int len, i, chn;
  ASSERT_TYPE(SOUND_DATA_P(sdobj), sdobj, SCM_ARG1, S_sound_data2vct, "a sound-data object");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(chan), chan, SCM_ARG2, S_sound_data2vct, "an integer");
  ASSERT_TYPE(NOT_BOUND_P(vobj) || VCT_P(vobj), vobj, SCM_ARG3, S_sound_data2vct, "a vct");
  sd = (sound_data *)SND_VALUE_OF(sdobj);
  if (!(VCT_P(vobj))) vobj = make_vct(sd->length, (Float *)CALLOC(sd->length, sizeof(Float)));
  v = TO_VCT(vobj);
  chn = TO_C_INT_OR_ELSE(chan, 0);
  if (chn >= sd->chans)
    mus_misc_error(S_sound_data2vct, "invalid channel", SCM_LIST3(sdobj, chan, vobj));
  if (sd->length < v->length) 
    len = sd->length; 
  else len = v->length;
  for (i = 0; i < len; i++) 
    v->data[i] = (Float)(MUS_SAMPLE_TO_FLOAT(sd->data[chn][i]));
  return(vobj);
}

static SCM vct2sound_data(SCM vobj, SCM sdobj, SCM chan)
{
  #define H_vct2sound_data "(" S_vct2sound_data " v sd chan) places vct v's data into sound-data sd's channel chan"
  vct *v;
  sound_data *sd;
  int len, i, chn;
  ASSERT_TYPE(VCT_P(vobj), vobj, SCM_ARG1, S_vct2sound_data, "a vct");
  ASSERT_TYPE(NOT_BOUND_P(sdobj) || SOUND_DATA_P(sdobj), sdobj, SCM_ARG2, S_vct2sound_data, "a sound-data object");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(chan), chan, SCM_ARG3, S_vct2sound_data, "an integer");
  v = TO_VCT(vobj);
  if (!(SOUND_DATA_P(sdobj))) sdobj = make_sound_data(1, v->length);
  sd = (sound_data *)SND_VALUE_OF(sdobj);
  chn = TO_C_INT_OR_ELSE(chan, 0);
  if (chn >= sd->chans)
    mus_misc_error(S_vct2sound_data, "invalid channel", SCM_LIST3(vobj, chan, sdobj));
  if (sd->length < v->length) 
    len = sd->length; 
  else len = v->length;
  for (i = 0; i < len; i++) 
    sd->data[chn][i] = MUS_FLOAT_TO_SAMPLE(v->data[i]);
  return(sdobj);
}


static SCM g_open_sound_input(SCM file)
{
  #define H_mus_sound_open_input "(" S_mus_sound_open_input " filename) -> fd (int), opens filename for sound input"
  int fd;
  char *tmpstr = NULL;
  ASSERT_TYPE(STRING_P(file), file, SCM_ARGn, S_mus_sound_open_input, "a string");
  fd = mus_sound_open_input(tmpstr = mus_expand_filename(TO_C_STRING(file)));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(fd));
}

static SCM g_open_sound_output(SCM file, SCM srate, SCM chans, SCM data_format, SCM header_type, SCM comment)
{

  #define H_mus_sound_open_output "(" S_mus_sound_open_output " filename srate chans data-format header-type &optional comment)\n\
opens filename for sound output with the given srate and so on, returns the file number (int). \
the file size is normally set later via mus-sound-close-output. srate is an integer, comment is a string, \
data-format is a sndlib format indicator such as mus-bshort, if #f if defaults to a format compatible with sndlib, \
header-type is a sndlib type indicator such as mus-aiff, sndlib currently only writes 5 or so header types."

  int fd = -1, df, ht, chns;
  char *com = NULL;
  char *tmpstr = NULL;
  ASSERT_TYPE(STRING_P(file), file, SCM_ARG1, S_mus_sound_open_output, "a string");
  ASSERT_TYPE(NUMBER_OR_BOOLEAN_P(srate), srate, SCM_ARG2, S_mus_sound_open_output, "a number or #f");
  ASSERT_TYPE(INTEGER_OR_BOOLEAN_P(chans), chans, SCM_ARG3, S_mus_sound_open_output, "an integer or #f");
  ASSERT_TYPE(INTEGER_OR_BOOLEAN_P(data_format), data_format, SCM_ARG4, S_mus_sound_open_output, "an integer (data-format id) or #f");
  ASSERT_TYPE(INTEGER_OR_BOOLEAN_P(header_type), header_type, SCM_ARG5, S_mus_sound_open_output, "an integer (header-type id) or #f");
  ASSERT_TYPE((STRING_P(comment) || (NOT_BOUND_P(comment))), comment, SCM_ARG6, S_mus_sound_open_output, "a string");
  if (INTEGER_P(data_format))
    df = TO_C_INT(data_format);
  else df = MUS_OUT_FORMAT;
  if (MUS_DATA_FORMAT_OK(df))
    {
      ht = TO_C_INT_OR_ELSE(header_type, 0);
      if (MUS_HEADER_TYPE_OK(ht))
	{
	  chns = TO_C_INT_OR_ELSE(chans, 0);
	  if (chns > 0)
	    {
	      tmpstr = mus_expand_filename(TO_C_STRING(file));
	      if (STRING_P(comment)) com = TO_C_STRING(comment);
	      fd = mus_sound_open_output(tmpstr,
					 TO_C_INT_OR_ELSE(srate, 0),
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
  return(TO_SCM_INT(fd));
}

static SCM g_reopen_sound_output(SCM file, SCM chans, SCM data_format, SCM header_type, SCM data_loc)
{

  #define H_mus_sound_reopen_output "(" S_mus_sound_reopen_output " filename chans data-format header-type data-location)\n\
reopen (without alteration) filename for output \
data-format and header-type are sndlib indicators such as mus-bshort or mus-aiff \
data-location should be retrieved from a previous call to mus-sound-data-location"

  int fd = -1, df, ht, chns;
  char *tmpstr = NULL;
  ASSERT_TYPE(STRING_P(file), file, SCM_ARG1, S_mus_sound_reopen_output, "a string");
  ASSERT_TYPE(INTEGER_OR_BOOLEAN_P(chans), chans, SCM_ARG2, S_mus_sound_reopen_output, "an integer or #f");
  ASSERT_TYPE(INTEGER_OR_BOOLEAN_P(data_format), data_format, SCM_ARG3, S_mus_sound_reopen_output, "an integer (data-format id) or #f");
  ASSERT_TYPE(INTEGER_OR_BOOLEAN_P(header_type), header_type, SCM_ARG4, S_mus_sound_reopen_output, "an integer (header-type id) or #f");
  ASSERT_TYPE(INTEGER_OR_BOOLEAN_P(data_loc), data_loc, SCM_ARG5, S_mus_sound_reopen_output, "an integer or #f");
  df = TO_C_INT_OR_ELSE(data_format, MUS_OUT_FORMAT);
  if (MUS_DATA_FORMAT_OK(df))
    {
      ht = TO_C_INT_OR_ELSE(header_type, 0);
      if (MUS_HEADER_TYPE_OK(ht))
	{
	  chns = TO_C_INT_OR_ELSE(chans, 0);
	  if (chns > 0)
	    {
	      tmpstr = mus_expand_filename(TO_C_STRING(file));
	      fd = mus_sound_reopen_output(tmpstr,
					   chns,
					   df,
					   ht,
					   TO_C_INT_OR_ELSE(data_loc, 0));
	      if (tmpstr) FREE(tmpstr);
	    }
	  else mus_misc_error(S_mus_sound_reopen_output, "invalid chans", chans);
	}
      else mus_misc_error(S_mus_sound_reopen_output, "invalid header type", header_type);
    }
  else mus_misc_error(S_mus_sound_reopen_output, "invalid data format", data_format);
  return(TO_SCM_INT(fd));
}

static SCM g_close_sound_input(SCM fd)
{
  #define H_mus_sound_close_input "(" S_mus_sound_close_input " fd) closes file number fd"
  int nfd;
  ASSERT_TYPE(INTEGER_P(fd), fd, SCM_ARGn, S_mus_sound_close_input, "an integer");
  nfd = TO_C_INT(fd);
  if ((nfd < 0) || (nfd == fileno(stdin)) || (nfd == fileno(stdout)) || (nfd == fileno(stderr)))
    mus_misc_error(S_mus_sound_close_input, "invalid file", fd);
  return(TO_SCM_INT(mus_sound_close_input(TO_C_INT(fd))));
}

static SCM g_close_sound_output(SCM fd, SCM bytes)
{
  #define H_mus_sound_close_output "(" S_mus_sound_close_output " fd bytes) closes file number fd \
after updating its header (if any) to reflect bytes, the new file data size"

  int nfd;
  ASSERT_TYPE(INTEGER_P(fd), fd, SCM_ARG1, S_mus_sound_close_output, "an integer");
  ASSERT_TYPE(NUMBER_P(bytes), bytes, SCM_ARG2, S_mus_sound_close_output, "a number");
  nfd = TO_C_INT(fd);
  if ((nfd < 0) || (nfd == fileno(stdin)) || (nfd == fileno(stdout)) || (nfd == fileno(stderr)))
    mus_misc_error(S_mus_sound_close_output, "invalid file", fd);
  return(TO_SCM_INT(mus_sound_close_output(TO_C_INT(fd),
					   TO_C_INT_OR_ELSE(bytes, 0))));
}

static SCM g_read_sound(SCM fd, SCM beg, SCM end, SCM chans, SCM sv)
{
  #define H_mus_sound_read "(" S_mus_sound_read " fd beg end chans sdata) reads sound data from file number fd, \
filling the sound-data object sdata's buffers starting at (buffer location) beg, going to end"

  ASSERT_TYPE(INTEGER_P(fd), fd, SCM_ARG1, S_mus_sound_read, "an integer");
  ASSERT_TYPE(NUMBER_P(beg), beg, SCM_ARG2, S_mus_sound_read, "a number");
  ASSERT_TYPE(NUMBER_P(end), end, SCM_ARG3, S_mus_sound_read, "a number");
  ASSERT_TYPE(INTEGER_P(chans), chans, SCM_ARG4, S_mus_sound_read, "an integer");
  ASSERT_TYPE(SOUND_DATA_P(sv), sv, SCM_ARG5, S_mus_sound_read, "a sound-data object");
  return(TO_SCM_INT(mus_sound_read(TO_C_INT(fd),
				   TO_C_INT_OR_ELSE(beg, 0),
				   TO_C_INT_OR_ELSE(end, 0),
				   TO_C_INT(chans),
				   get_sound_data(sv))));
}

static SCM g_write_sound(SCM fd, SCM beg, SCM end, SCM chans, SCM sv)
{
  #define H_mus_sound_write "(" S_mus_sound_write " fd beg end chans sdata) writes sound-data object sdata's data \
starting at (buffer location) beg, going to end, writing to file number fd"

  ASSERT_TYPE(INTEGER_P(fd), fd, SCM_ARG1, S_mus_sound_write, "an integer");
  ASSERT_TYPE(NUMBER_P(beg), beg, SCM_ARG2, S_mus_sound_write, "a number");
  ASSERT_TYPE(NUMBER_P(end), end, SCM_ARG3, S_mus_sound_write, "a number");
  ASSERT_TYPE(INTEGER_P(chans), chans, SCM_ARG4, S_mus_sound_write, "an integer");
  ASSERT_TYPE(SOUND_DATA_P(sv), sv, SCM_ARG5, S_mus_sound_write, "a sound-data object");
  return(TO_SCM_INT(mus_sound_write(TO_C_INT(fd),
				    TO_C_INT_OR_ELSE(beg, 0),
				    TO_C_INT_OR_ELSE(end, 0),
				    TO_C_INT(chans),
				    get_sound_data(sv))));
}

static SCM g_seek_sound(SCM fd, SCM offset, SCM origin)
{
  #define H_mus_sound_seek "(" S_mus_sound_seek " fd offset origin) moves the current read/write location in file number fd \
to the short-wise sample offset given origin (both treated as in lseek)"

  ASSERT_TYPE(INTEGER_P(fd), fd, SCM_ARG1, S_mus_sound_seek, "an integer");
  ASSERT_TYPE(INTEGER_P(offset), offset, SCM_ARG2, S_mus_sound_seek, "an integer");
  ASSERT_TYPE(INTEGER_P(origin), origin, SCM_ARG3, S_mus_sound_seek, "an integer");
  return(TO_SCM_INT(mus_sound_seek(TO_C_INT(fd),
				   TO_C_INT(offset),
				   TO_C_INT(origin))));
}

static SCM g_seek_sound_frame(SCM fd, SCM offset)
{
  #define H_mus_sound_seek_frame "(" S_mus_sound_seek_frame " fd frame) moves the current read/write location in file number fd \
to the indicated frame"

  ASSERT_TYPE(INTEGER_P(fd), fd, SCM_ARG1, S_mus_sound_seek_frame, "an integer");
  ASSERT_TYPE(INTEGER_P(offset), offset, SCM_ARG2, S_mus_sound_seek_frame, "an integer");
  return(TO_SCM_INT(mus_sound_seek_frame(TO_C_INT(fd),
					 TO_C_INT(offset))));
}

static SCM g_open_audio_output(SCM dev, SCM srate, SCM chans, SCM format, SCM size)
{
  #define H_mus_audio_open_output "(" S_mus_audio_open_output " device srate chans format clipped)\n\
opens the audio device ready for output at the given srate and so on. \
returns the audio line number:\n\
  (set! line (mus-audio-open-output mus-audio-default 22050 1 mus-lshort 256)"

  ASSERT_TYPE(INTEGER_P(dev), dev, SCM_ARG1, S_mus_audio_open_output, "an integer");
  ASSERT_TYPE(NUMBER_P(srate), srate, SCM_ARG2, S_mus_audio_open_output, "a number");
  ASSERT_TYPE(INTEGER_P(chans), chans, SCM_ARG3, S_mus_audio_open_output, "an integer");
  ASSERT_TYPE(INTEGER_P(format), format, SCM_ARG4, S_mus_audio_open_output, "an integer");
  ASSERT_TYPE(NUMBER_P(size), size, SCM_ARG5, S_mus_audio_open_output, "a number");
  if (!(MUS_AUDIO_DEVICE_OK(TO_C_INT(dev))))
    mus_misc_error(S_mus_audio_open_output, "invalid device", dev);
  if (!(MUS_DATA_FORMAT_OK(TO_C_INT(format))))
    mus_misc_error(S_mus_audio_open_output, "invalid data format", format);
  if (TO_C_INT_OR_ELSE(size, 0) < 0)
    mus_misc_error(S_mus_audio_open_output, "invalid size", size);
  if (TO_C_INT_OR_ELSE(srate, 0) <= 0)
    mus_misc_error(S_mus_audio_open_output, "invalid srate", srate);
  if ((TO_C_INT(chans) <= 0) || (TO_C_INT(chans) > 256))
    mus_misc_error(S_mus_audio_open_output, "invalid chans", chans);
  return(TO_SCM_INT(mus_audio_open_output(TO_C_INT(dev),
					  TO_C_INT_OR_ELSE(srate, 0),
					  TO_C_INT(chans),
					  TO_C_INT(format),
					  TO_C_INT_OR_ELSE(size, 0))));
}

static SCM g_open_audio_input(SCM dev, SCM srate, SCM chans, SCM format, SCM size)
{
  #define H_mus_audio_open_input "(" S_mus_audio_open_input " (device srate chans format bufsize)\n\
opens the audio device ready for input with the indicated attributes, returns the audio line number"

  ASSERT_TYPE(INTEGER_P(dev), dev, SCM_ARG1, S_mus_audio_open_input, "an integer");
  ASSERT_TYPE(NUMBER_P(srate), srate, SCM_ARG2, S_mus_audio_open_input, "a number");
  ASSERT_TYPE(INTEGER_P(chans), chans, SCM_ARG3, S_mus_audio_open_input, "an integer");
  ASSERT_TYPE(INTEGER_P(format), format, SCM_ARG4, S_mus_audio_open_input, "an integer");
  ASSERT_TYPE(NUMBER_P(size), size, SCM_ARG5, S_mus_audio_open_input, "a number");
  if (!(MUS_AUDIO_DEVICE_OK(TO_C_INT(dev))))
    mus_misc_error(S_mus_audio_open_input, "invalid device", dev);
  if (!(MUS_DATA_FORMAT_OK(TO_C_INT(format))))
    mus_misc_error(S_mus_audio_open_input, "invalid data format", format);
  if (TO_C_INT_OR_ELSE(size, 0) < 0)
    mus_misc_error(S_mus_audio_open_input, "invalid size", size);
  if (TO_C_INT_OR_ELSE(srate, 0) <= 0)
    mus_misc_error(S_mus_audio_open_input, "invalid srate", srate);
  if (TO_C_INT(chans) <= 0)
    mus_misc_error(S_mus_audio_open_input, "invalid chans", chans);
  return(TO_SCM_INT(mus_audio_open_input(TO_C_INT(dev),
					 TO_C_INT_OR_ELSE(srate, 0),
					 TO_C_INT(chans),
					 TO_C_INT(format),
					 TO_C_INT_OR_ELSE(size, 0))));
}

static SCM g_close_audio(SCM line)
{
  #define H_mus_audio_close "(" S_mus_audio_close " line) closes the audio hardware port line"
  ASSERT_TYPE(INTEGER_P(line), line, SCM_ARGn, S_mus_audio_close, "an integer");
  return(TO_SCM_INT(mus_audio_close(TO_C_INT(line))));
}

static SCM g_save_audio_state (void) 
{
  #define H_mus_audio_save "(" S_mus_audio_save ") tries to save the current audio hardware state for a subsequent mus-audio-restore"
  mus_audio_save(); 
  return(SCM_BOOL_F);
}

static SCM g_restore_audio_state (void) 
{
  #define H_mus_audio_restore "(" S_mus_audio_restore ") tries to restore a previously saved audio hardware state"
  mus_audio_restore(); 
  return(SCM_BOOL_F);
}

static SCM g_audio_systems (void) 
{
  #define H_mus_audio_systems "(" S_mus_audio_systems ") -> number of available audio systems (normally each sound card is a separate 'system')"
  return(TO_SCM_INT(mus_audio_systems()));
}


/* these take a sndlib buffer (sound_data) and handle the conversion to the interleaved char* internally */
/* so, they take "frames", not "bytes", and a sound_data object, not char* etc */

static SCM g_write_audio(SCM line, SCM sdata, SCM frames)
{
  #define H_mus_audio_write "(" S_mus_audio_write " line sdata frames) writes frames of data (channels*frames = samples) \
to the audio line from the sound-data object sdata."

  /* assuming 16-bit output here for now */
  short *obuf;
  MUS_SAMPLE_TYPE **bufs;
  sound_data *sd;
  int i, j, k, outbytes, val, frms;
  ASSERT_TYPE(INTEGER_P(line), line, SCM_ARG1, S_mus_audio_write, "an integer");
  ASSERT_TYPE(SOUND_DATA_P(sdata), sdata, SCM_ARG2, S_mus_audio_write, "a sound-data object");
  ASSERT_TYPE(INTEGER_P(frames), frames, SCM_ARG3, S_mus_audio_write, "an integer");
  sd = (sound_data *)SND_VALUE_OF(sdata);
  bufs = sd->data;
  frms = TO_C_INT(frames);
  outbytes = frms * sd->chans * 2;
  obuf = (short *)CALLOC(frms * sd->chans, sizeof(short));
  if (sd->chans == 1)
    {
      for (k = 0; k < frms; k++) 
	obuf[k] = (short)MUS_SAMPLE_TO_SHORT(bufs[0][k]);
    }
  else
    {
      for (k = 0, j = 0; k < frms; k++, j += sd->chans)
	for (i = 0; i < sd->chans; i++) 
	  obuf[j + i] = (short)MUS_SAMPLE_TO_SHORT(bufs[i][k]);
    }
  val = mus_audio_write(TO_C_INT(line), (char *)obuf, outbytes);
  FREE(obuf);
  return(scm_return_first(TO_SCM_INT(val), sdata));
}

static SCM g_read_audio(SCM line, SCM sdata, SCM frames)
{
  #define H_mus_audio_read "(" S_mus_audio_read " line sdata frames) reads frames of data (channels*frames = samples) \
from the audio line into the sound-data object sdata."

  short *inbuf;
  sound_data *sd;
  int val, inbytes, i, j, k, frms;
  MUS_SAMPLE_TYPE **bufs;
  ASSERT_TYPE(INTEGER_P(line), line, SCM_ARG1, S_mus_audio_read, "an integer");
  ASSERT_TYPE(SOUND_DATA_P(sdata), sdata, SCM_ARG2, S_mus_audio_read, "a sound-data object");
  ASSERT_TYPE(INTEGER_P(frames), frames, SCM_ARG3, S_mus_audio_read, "an integer");
  sd = (sound_data *)SND_VALUE_OF(sdata);
  bufs = sd->data;
  frms = TO_C_INT(frames);
  inbytes = frms * sd->chans * 2;
  inbuf = (short *)CALLOC(frms * sd->chans, sizeof(short));
  val = mus_audio_read(TO_C_INT(line), (char *)inbuf, inbytes);
  if (sd->chans == 1)
    {
      for (k = 0; k < frms; k++) 
	bufs[0][k] = (short)MUS_SHORT_TO_SAMPLE(inbuf[k]);
    }
  else
    {
      for (k = 0, j = 0; k < frms; k++, j += sd->chans)
	for (i = 0; i < sd->chans; i++) 
	  bufs[i][k] = (short)MUS_SHORT_TO_SAMPLE(inbuf[j + i]);
    }
  FREE(inbuf);
  return(scm_return_first(TO_SCM_INT(val), sdata));
}

static SCM g_read_audio_state(SCM dev, SCM field, SCM chan, SCM vals)
{
  #define H_mus_audio_mixer_read "(" S_mus_audio_mixer_read " device field channel vals) reads sound card 'mixer' state"
  int val, i, len;
  float *fvals;
  SCM *vdata;
  ASSERT_TYPE(INTEGER_P(dev), dev, SCM_ARG1, S_mus_audio_mixer_read, "an integer");
  ASSERT_TYPE(INTEGER_P(field), field, SCM_ARG2, S_mus_audio_mixer_read, "an integer");
  ASSERT_TYPE(INTEGER_P(chan), chan, SCM_ARG3, S_mus_audio_mixer_read, "an integer");
  ASSERT_TYPE(VECTOR_P(vals), vals, SCM_ARG4, S_mus_audio_mixer_read, "a vector");
  if (!(MUS_AUDIO_DEVICE_OK(TO_C_INT(dev)))) 
    mus_misc_error(S_mus_audio_mixer_read, "invalid device", dev);
  if (!(MUS_AUDIO_DEVICE_OK(TO_C_INT(field))))
    mus_misc_error(S_mus_audio_mixer_read, "invalid field", field);
  len = VECTOR_LENGTH(vals);
  if (len == 0)
    fvals = (float *)CALLOC(1, sizeof(float));
  else fvals = (float *)CALLOC(len, sizeof(float));
  val = mus_audio_mixer_read(TO_C_INT(dev),
			     TO_C_INT(field),
			     TO_C_INT(chan),
			     fvals);
  vdata = SCM_VELTS(vals);
  for (i = 0; i < len; i++) 
    vdata[i] = TO_SCM_DOUBLE(fvals[i]);
  FREE(fvals);
  return(TO_SCM_INT(val));
}

static SCM g_write_audio_state(SCM dev, SCM field, SCM chan, SCM vals)
{
  #define H_mus_audio_mixer_write "(" S_mus_audio_mixer_write " device field channel vals) changes the sound card's 'mixer' state"
  int i, len, res;
  float *fvals;
  SCM *vdata;
  ASSERT_TYPE(INTEGER_P(dev), dev, SCM_ARG1, S_mus_audio_mixer_write, "an integer");
  ASSERT_TYPE(INTEGER_P(field), field, SCM_ARG2, S_mus_audio_mixer_write, "an integer");
  ASSERT_TYPE(INTEGER_P(chan), chan, SCM_ARG3, S_mus_audio_mixer_write, "an integer");
  ASSERT_TYPE(VECTOR_P(vals), vals, SCM_ARG4, S_mus_audio_mixer_write, "a vector");
  if (!(MUS_AUDIO_DEVICE_OK(TO_C_INT(dev)))) 
    mus_misc_error(S_mus_audio_mixer_write, "invalid device", dev);
  if (!(MUS_AUDIO_DEVICE_OK(TO_C_INT(field))))
    mus_misc_error(S_mus_audio_mixer_write, "invalid field", field);
  len = VECTOR_LENGTH(vals);
  if (len == 0)
    fvals = (float *)CALLOC(1, sizeof(float));
  else
    {
      fvals = (float *)CALLOC(len, sizeof(float));
      vdata = SCM_VELTS(vals);
      for (i = 0; i < len; i++) 
	fvals[i] = TO_C_DOUBLE(vdata[i]);
    }
  res = mus_audio_mixer_write(TO_C_INT(dev),
			      TO_C_INT(field),
			      TO_C_INT(chan),
			      fvals);
  FREE(fvals);
  return(TO_SCM_INT(res));
}

static SCM g_mus_set_data_clipped(SCM fd, SCM clipped)
{
  #define H_mus_file_set_data_clipped "(" S_mus_file_set_data_clipped " fd val) sets whether data associated with file fd is clipped or wraps around"
  ASSERT_TYPE(INTEGER_P(fd), fd, SCM_ARG1, S_mus_file_set_data_clipped, "an integer");
  ASSERT_TYPE(BOOLEAN_P(clipped), fd, SCM_ARG2, S_mus_file_set_data_clipped, "a boolean");
  return(TO_SCM_INT(mus_file_set_data_clipped(TO_C_INT(fd),
					      (FALSE_P(clipped)) ? 0 : 1)));
}

static SCM g_mus_prescaler(SCM fd)
{
  #define H_mus_file_prescaler "(" S_mus_file_prescaler " fd) -> current prescaler (normally 1.0) associated with fd"
  ASSERT_TYPE(INTEGER_P(fd), fd, SCM_ARGn, S_mus_file_set_prescaler, "an integer");
  return(TO_SCM_DOUBLE(mus_file_prescaler(TO_C_INT(fd))));
}

static SCM g_mus_set_prescaler(SCM fd, SCM val)
{
  #define H_mus_file_set_prescaler "(" S_mus_file_set_prescaler " fd val) sets the current prescaler associated with fd"
  ASSERT_TYPE(INTEGER_P(fd), fd, SCM_ARG1, S_mus_file_set_prescaler, "an integer");
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARG2, S_mus_file_set_prescaler, "a number");
  return(TO_SCM_DOUBLE(mus_file_set_prescaler(TO_C_INT(fd),
					      TO_C_DOUBLE(val))));
}

static SCM g_mus_expand_filename(SCM file)
{
  #define H_mus_expand_filename "(" S_mus_expand_filename " name) returns a 'canonical' or 'absolute' filename"
  SCM result;
  char *tmpstr;
  ASSERT_TYPE(STRING_P(file), file, SCM_ARGn, S_mus_expand_filename, "a string");
  tmpstr = mus_expand_filename(TO_C_STRING(file));
  result = TO_SCM_STRING(tmpstr);
  if (tmpstr) FREE(tmpstr);
  return(result);
}

static SCM g_mus_sound_report_cache(SCM file)
{
  #define H_mus_sound_report_cache "(" S_mus_sound_report_cache " name) prints the current sound \
header data table to the file given or stdout if none"

  SCM res = SCM_BOOL_F;
  if (NOT_BOUND_P(file))
    mus_sound_print_cache();
  else
    {
      FILE *fd;
      char *name = NULL;
      fd = fopen(name = mus_expand_filename(TO_C_STRING(file)), "w");
      mus_sound_report_cache(fd);
      fclose(fd);
      if (name) FREE(name);
      return(file);
    }
  return(res);
}

void mus_sndlib2scm_initialize(void)
{
  SCM local_doc;
  mus_sound_initialize();
  local_doc = MAKE_PERMANENT(DOCUMENTATION);

#if HAVE_GUILE
  sound_data_tag = scm_make_smob_type("sound-data", sizeof(sound_data));
  scm_set_smob_print(sound_data_tag, print_sound_data);
  scm_set_smob_free(sound_data_tag, free_sound_data);
  scm_set_smob_equalp(sound_data_tag, equalp_sound_data);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(sound_data_tag, SCM_FNC sound_data_apply, 2, 0, 0);
#endif
#endif

  DEFINE_VAR(S_mus_out_format, MUS_OUT_FORMAT, "sample format for fastest IO");

  DEFINE_VAR(S_mus_next,    MUS_NEXT,    "NeXT (Sun) sound header id");
  DEFINE_VAR(S_mus_aifc,    MUS_AIFC,    "AIFC sound header id");
  DEFINE_VAR(S_mus_riff,    MUS_RIFF,    "RIFF (wave) sound header id");
  DEFINE_VAR(S_mus_nist,    MUS_NIST,    "NIST (Sphere) sound header id");
  DEFINE_VAR(S_mus_raw,     MUS_RAW,     "raw (headerless) sound header id");
  DEFINE_VAR(S_mus_ircam,   MUS_IRCAM,   "IRCAM sound header id");
  DEFINE_VAR(S_mus_aiff,    MUS_AIFF,    "AIFF (old-style) sound header id");

  DEFINE_VAR(S_mus_bshort,  MUS_BSHORT,  "big-endian short data format id");
  DEFINE_VAR(S_mus_lshort,  MUS_LSHORT,  "little-endian short data format id");
  DEFINE_VAR(S_mus_mulaw,   MUS_MULAW,   "mulaw (8-bit) data format id");
  DEFINE_VAR(S_mus_alaw,    MUS_ALAW,    "alaw (8-bit) data format id");
  DEFINE_VAR(S_mus_byte,    MUS_BYTE,    "signed byte data format id");
  DEFINE_VAR(S_mus_ubyte,   MUS_UBYTE,   "unsigned byte data format id");
  DEFINE_VAR(S_mus_bfloat,  MUS_BFLOAT,  "big-endian float data format id");
  DEFINE_VAR(S_mus_lfloat,  MUS_LFLOAT,  "little-endian float data format id");
  DEFINE_VAR(S_mus_bint,    MUS_BINT,    "big-endian int data format id");
  DEFINE_VAR(S_mus_lint,    MUS_LINT,    "little-endian int data format id");
  DEFINE_VAR(S_mus_bintn,   MUS_BINTN,   "normalized big-endian int data format id");
  DEFINE_VAR(S_mus_lintn,   MUS_LINTN,   "normalized little-endian int data format id");
  DEFINE_VAR(S_mus_b24int,  MUS_B24INT,  "big-endian 24-bit data format id");
  DEFINE_VAR(S_mus_l24int,  MUS_L24INT,  "little-endian 24-bit data format id");
  DEFINE_VAR(S_mus_bdouble, MUS_BDOUBLE, "big-endian double data format id");
  DEFINE_VAR(S_mus_ldouble, MUS_LDOUBLE, "little-endian double data format id");
  DEFINE_VAR(S_mus_ubshort, MUS_UBSHORT, "unsigned big-endian short data format id");
  DEFINE_VAR(S_mus_ulshort, MUS_ULSHORT, "unsigned little-endian short data format id");

  DEFINE_VAR(S_mus_audio_default,        MUS_AUDIO_DEFAULT,        "default audio device");
  DEFINE_VAR(S_mus_audio_duplex_default, MUS_AUDIO_DUPLEX_DEFAULT, "default duplex device");
  DEFINE_VAR(S_mus_audio_line_out,       MUS_AUDIO_LINE_OUT,       "audio line-out device");
  DEFINE_VAR(S_mus_audio_line_in,        MUS_AUDIO_LINE_IN,        "audio line-in device");
  DEFINE_VAR(S_mus_audio_microphone,     MUS_AUDIO_MICROPHONE,     "microphone device");
  DEFINE_VAR(S_mus_audio_speakers,       MUS_AUDIO_SPEAKERS,       "speakers device (a mixer kludge)");
  DEFINE_VAR(S_mus_audio_dac_out,        MUS_AUDIO_DAC_OUT,        "DAC out device");
  DEFINE_VAR(S_mus_audio_adat_in,        MUS_AUDIO_ADAT_IN,        "ADAT in device");
  DEFINE_VAR(S_mus_audio_aes_in,         MUS_AUDIO_AES_IN,         "AES in device");
  DEFINE_VAR(S_mus_audio_digital_in,     MUS_AUDIO_DIGITAL_IN,     "digital audio in device");
  DEFINE_VAR(S_mus_audio_digital_out,    MUS_AUDIO_DIGITAL_OUT,    "digital audio out device");
  DEFINE_VAR(S_mus_audio_adat_out,       MUS_AUDIO_ADAT_OUT,       "ADAT out device");
  DEFINE_VAR(S_mus_audio_aes_out,        MUS_AUDIO_AES_OUT,        "AES out device");
  DEFINE_VAR(S_mus_audio_dac_filter,     MUS_AUDIO_DAC_FILTER,     "DAC filter 'device' (a mixer kludge)");
  DEFINE_VAR(S_mus_audio_mixer,          MUS_AUDIO_MIXER,          "the 'mixer' device");
  DEFINE_VAR(S_mus_audio_line1,          MUS_AUDIO_LINE1,          "audio line 1 device");
  DEFINE_VAR(S_mus_audio_line2,          MUS_AUDIO_LINE2,          "audio line 2device");
  DEFINE_VAR(S_mus_audio_line3,          MUS_AUDIO_LINE3,          "audio line 3device");
  DEFINE_VAR(S_mus_audio_aux_input,      MUS_AUDIO_AUX_INPUT,      "aux audio in device");
  DEFINE_VAR(S_mus_audio_cd,             MUS_AUDIO_CD,             "CD in device");
  DEFINE_VAR(S_mus_audio_aux_output,     MUS_AUDIO_AUX_OUTPUT,     "aux audio out device");
  DEFINE_VAR(S_mus_audio_spdif_in,       MUS_AUDIO_SPDIF_IN,       "SPDIF in device");
  DEFINE_VAR(S_mus_audio_spdif_out,      MUS_AUDIO_SPDIF_OUT,      "SPDIF out device");
  DEFINE_VAR(S_mus_audio_direction,      MUS_AUDIO_DIRECTION,      "audio sample flow direction (mus-audio-read)");
  DEFINE_VAR(S_mus_audio_samples_per_channel, MUS_AUDIO_SAMPLES_PER_CHANNEL, "samples per channel (mus-audio-read)");

  DEFINE_VAR(S_mus_audio_amp,     MUS_AUDIO_AMP,     "mixer amp field id");
  DEFINE_VAR(S_mus_audio_srate,   MUS_AUDIO_SRATE,   "mixer srate field id");
  DEFINE_VAR(S_mus_audio_channel, MUS_AUDIO_CHANNEL, "mixer channel field id");
  DEFINE_VAR(S_mus_audio_format,  MUS_AUDIO_FORMAT,  "mixer data format field id");
  DEFINE_VAR(S_mus_audio_port,    MUS_AUDIO_PORT,    "mixer port");
  DEFINE_VAR(S_mus_audio_imix,    MUS_AUDIO_IMIX,    "mixer 'imix' field id");
  DEFINE_VAR(S_mus_audio_igain,   MUS_AUDIO_IGAIN,   "mixer 'igain' field id");
  DEFINE_VAR(S_mus_audio_reclev,  MUS_AUDIO_RECLEV,  "mixer 'reclev' field id");
  DEFINE_VAR(S_mus_audio_pcm,     MUS_AUDIO_PCM,     "mixer 'pcm' field id");
  DEFINE_VAR(S_mus_audio_pcm2,    MUS_AUDIO_PCM2,    "mixer 'pcm2' field id");
  DEFINE_VAR(S_mus_audio_ogain,   MUS_AUDIO_OGAIN,   "mixer 'ogain' field id");
  DEFINE_VAR(S_mus_audio_line,    MUS_AUDIO_LINE,    "mixer 'line' field id");
  DEFINE_VAR(S_mus_audio_synth,   MUS_AUDIO_SYNTH,   "mixer 'synth' field id");
  DEFINE_VAR(S_mus_audio_bass,    MUS_AUDIO_BASS,    "mixer 'bass' field id");
  DEFINE_VAR(S_mus_audio_treble,  MUS_AUDIO_TREBLE,  "mixer 'treble' field id");

  DEFINE_PROC(S_sound_data_length,        sound_data_length, 1, 0, 0,       H_sound_data_length);
  DEFINE_PROC(S_sound_data_chans,         sound_data_chans, 1, 0, 0,        H_sound_data_chans);
  DEFINE_PROC(S_sound_data_ref,           sound_data_ref, 3, 0, 0,          H_sound_data_ref);
  DEFINE_PROC(S_sound_data_setB,          sound_data_set, 4, 0, 0,          H_sound_data_setB);
  DEFINE_PROC(S_make_sound_data,          g_make_sound_data, 2, 0, 0,       H_make_sound_data);
  DEFINE_PROC(S_sound_data_p,             g_sound_data_p, 1, 0, 0,          H_sound_data_p);
  DEFINE_PROC(S_sound_data2vct,           sound_data2vct, 1, 2, 0,          H_sound_data2vct);
  DEFINE_PROC(S_vct2sound_data,           vct2sound_data, 1, 2, 0,          H_vct2sound_data);
  DEFINE_PROC(S_mus_sound_samples,        g_sound_samples, 1, 0, 0,         H_mus_sound_samples);
  DEFINE_PROC(S_mus_sound_frames,         g_sound_frames, 1, 0, 0,          H_mus_sound_frames);
  DEFINE_PROC(S_mus_sound_duration,       g_sound_duration, 1, 0, 0,        H_mus_sound_duration);
  DEFINE_PROC(S_mus_sound_datum_size,     g_sound_datum_size, 1, 0, 0,      H_mus_sound_datum_size);
  DEFINE_PROC(S_mus_sound_data_location,  g_sound_data_location, 1, 0, 0,   H_mus_sound_data_location);
  DEFINE_PROC(S_mus_sound_chans,          g_sound_chans, 1, 0, 0,           H_mus_sound_chans);
  DEFINE_PROC(S_mus_sound_srate,          g_sound_srate, 1, 0, 0,           H_mus_sound_srate);
  DEFINE_PROC(S_mus_sound_header_type,    g_sound_header_type, 1, 0, 0,     H_mus_sound_header_type);
  DEFINE_PROC(S_mus_sound_data_format,    g_sound_data_format, 1, 0, 0,     H_mus_sound_data_format);
  DEFINE_PROC(S_mus_sound_length,         g_sound_length, 1, 0, 0,          H_mus_sound_length	);
  DEFINE_PROC(S_mus_sound_type_specifier, g_sound_type_specifier, 1, 0, 0,  H_mus_sound_type_specifier);
  DEFINE_PROC(S_mus_header_type_name,     g_sound_type_name, 1, 0, 0,       H_mus_header_type_name);
  DEFINE_PROC(S_mus_data_format_name,     g_sound_format_name, 1, 0, 0,     H_mus_data_format_name);
  DEFINE_PROC(S_mus_sound_comment,        g_sound_comment, 1, 0, 0,         H_mus_sound_comment);
  DEFINE_PROC(S_mus_sound_write_date,     g_sound_write_date, 1, 0, 0,      H_mus_sound_write_date);
  DEFINE_PROC(S_mus_data_format_bytes_per_sample, g_sound_bytes_per_sample, 1, 0, 0, H_mus_data_format_bytes_per_sample);
  DEFINE_PROC(S_mus_sound_loop_info,      g_sound_loop_info, 1, 0, 0,       H_mus_sound_loop_info);
  DEFINE_PROC(S_mus_sound_max_amp,        g_sound_max_amp, 1, 0, 0,         H_mus_sound_max_amp);
  DEFINE_PROC(S_mus_sound_set_max_amp,    g_sound_set_max_amp, 2, 0, 0,     H_mus_sound_set_max_amp);
  DEFINE_PROC(S_mus_sound_max_amp_exists, g_sound_max_amp_exists, 1, 0, 0,  H_mus_sound_max_amp_exists);
  DEFINE_PROC(S_mus_audio_report,         g_report_audio_state, 0, 0, 0,    H_mus_audio_report);
  DEFINE_PROC(S_mus_audio_sun_outputs,    g_audio_outputs, 3, 0, 0,         H_mus_audio_sun_outputs);
  DEFINE_PROC(S_mus_sound_open_input,     g_open_sound_input, 1, 0, 0,      H_mus_sound_open_input);
  DEFINE_PROC(S_mus_sound_close_input,    g_close_sound_input, 1, 0, 0,     H_mus_sound_close_input);
  DEFINE_PROC(S_mus_audio_close,          g_close_audio, 1, 0, 0,           H_mus_audio_close);
  DEFINE_PROC(S_mus_audio_save,           g_save_audio_state, 0, 0, 0,      H_mus_audio_save);
  DEFINE_PROC(S_mus_audio_restore,        g_restore_audio_state, 0, 0, 0,   H_mus_audio_restore);
  DEFINE_PROC(S_mus_audio_systems,        g_audio_systems, 0, 0, 0,         H_mus_audio_systems);
  DEFINE_PROC(S_mus_audio_mixer_read,     g_read_audio_state, 4, 0, 0,      H_mus_audio_mixer_read);
  DEFINE_PROC(S_mus_audio_mixer_write,    g_write_audio_state, 4, 0, 0,     H_mus_audio_mixer_write);
  DEFINE_PROC(S_mus_file_set_data_clipped, g_mus_set_data_clipped, 2, 0, 0, H_mus_file_set_data_clipped);
  DEFINE_PROC(S_mus_file_prescaler,       g_mus_prescaler, 1, 0, 0,         H_mus_file_prescaler);
  DEFINE_PROC(S_mus_file_set_prescaler,   g_mus_set_prescaler, 2, 0, 0,     H_mus_file_set_prescaler);
  DEFINE_PROC(S_mus_expand_filename,      g_mus_expand_filename, 1, 0, 0,   H_mus_expand_filename);
  DEFINE_PROC(S_mus_audio_write,          g_write_audio, 3, 0, 0,           H_mus_audio_write);
  DEFINE_PROC(S_mus_audio_read,           g_read_audio, 3, 0, 0,            H_mus_audio_read);
  DEFINE_PROC(S_mus_sound_open_output,    g_open_sound_output, 5, 1, 0,     H_mus_sound_open_output);
  DEFINE_PROC(S_mus_sound_reopen_output,  g_reopen_sound_output, 5, 0, 0,   H_mus_sound_reopen_output);
  DEFINE_PROC(S_mus_sound_close_output,   g_close_sound_output, 2, 0, 0,    H_mus_sound_close_output);
  DEFINE_PROC(S_mus_sound_read,           g_read_sound, 5, 0, 0,            H_mus_sound_read);
  DEFINE_PROC(S_mus_sound_write,          g_write_sound, 5, 0, 0,           H_mus_sound_write);
  DEFINE_PROC(S_mus_sound_seek,           g_seek_sound, 3, 0, 0,            H_mus_sound_seek);
  DEFINE_PROC(S_mus_sound_seek_frame,     g_seek_sound_frame, 2, 0, 0,      H_mus_sound_seek_frame);
  DEFINE_PROC(S_mus_audio_open_output,    g_open_audio_output, 5, 0, 0,     H_mus_audio_open_output);
  DEFINE_PROC(S_mus_audio_open_input,     g_open_audio_input, 5, 0, 0,      H_mus_audio_open_input);
  DEFINE_PROC(S_mus_sound_report_cache,   g_mus_sound_report_cache, 0, 1, 0,H_mus_sound_report_cache);
#if USE_SND
  define_procedure_with_setter(S_sound_data_ref, SCM_FNC sound_data_ref, H_sound_data_ref,
			       "set-" S_sound_data_ref, SCM_FNC sound_data_set, local_doc, 3, 0, 4, 0);
#endif

  YES_WE_HAVE("sndlib");
}

/*
void scm_init_sndlib_sndlib_module ()
{
  scm_register_module_xxx("mus sndlib", mus_sndlib2scm_initialize);
}
*/
