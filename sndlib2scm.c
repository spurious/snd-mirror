/* tie sndlib into guile (Scheme) */

#if defined(HAVE_CONFIG_H)
  #include "config.h"
#endif

#ifndef HAVE_GUILE
  #define HAVE_GUILE 1
#endif

#if HAVE_GUILE

#include <stddef.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#if USE_SND
  #include "snd.h"
#endif

#include "sndlib.h"
#include "sndlib-strings.h"
#include "vct.h"
#include "sndlib2scm.h"
#include "sg.h"

#if (!USE_SND)
static int to_c_int_or_else(SCM obj, int fallback, char *origin)
{
  /* don't want errors here about floats with non-zero fractions etc */
  if (SCM_INUMP(obj))
    return(SCM_INUM(obj));
  else
    if (gh_number_p(obj))
      return((int)TO_C_DOUBLE_WITH_ORIGIN(obj, origin));
  return(fallback);
}

static char *full_filename(SCM file)
{
  return(mus_file_full_name(TO_C_STRING(file)));
}
#endif

static SCM g_sound_loop_info(SCM filename)
{
  #define H_mus_sound_loop_info "(" S_mus_sound_loop_info " filename) -> loop info for sound as a list (start1 end1 start2 end2 base-note base-detune)"
  int *res;
  char *tmpstr;
  SCM sres = SCM_EOL;
  SCM_ASSERT(gh_string_p(filename), filename, SCM_ARG1, S_mus_sound_loop_info); 
  tmpstr = full_filename(filename);
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
  SCM_ASSERT(gh_string_p(filename), filename, SCM_ARG1, S_mus_sound_samples); 
  res = mus_sound_samples(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(res));
}

static SCM g_sound_frames(SCM filename) 
{
  #define H_mus_sound_frames "(" S_mus_sound_frames " filename) -> frames (samples/channel) in sound"
  char *tmpstr = NULL;
  int res;
  SCM_ASSERT(gh_string_p(filename), filename, SCM_ARG1, S_mus_sound_frames); 
  res = mus_sound_frames(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(res));
}

static SCM g_sound_datum_size(SCM filename) 
{
  #define H_mus_sound_datum_size "(" S_mus_sound_datum_size " filename) -> bytes per sample of data in sound"
  char *tmpstr = NULL;
  int res;
  SCM_ASSERT(gh_string_p(filename), filename, SCM_ARG1, S_mus_sound_datum_size); 
  res = mus_sound_datum_size(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(res));
}

static SCM g_sound_data_location(SCM filename) 
{
  #define H_mus_sound_data_location "(" S_mus_sound_data_location " filename) -> location (bytes) of first sample of sound data"
  char *tmpstr = NULL;
  int res;
  SCM_ASSERT(gh_string_p(filename), filename, SCM_ARG1, S_mus_sound_data_location); 
  res = mus_sound_data_location(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(res));
}

static SCM g_sound_chans(SCM filename) 
{
  #define H_mus_sound_chans "(" S_mus_sound_chans " filename) -> channels of sound"
  char *tmpstr = NULL;
  int res;
  SCM_ASSERT(gh_string_p(filename), filename, SCM_ARG1, S_mus_sound_chans); 
  res = mus_sound_chans(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(res));
}

static SCM g_sound_srate(SCM filename) 
{
  #define H_mus_sound_srate "(" S_mus_sound_srate " filename) -> sampling rate of sound"
  char *tmpstr = NULL;
  int res;
  SCM_ASSERT(gh_string_p(filename), filename, SCM_ARG1, S_mus_sound_srate); 
  res = mus_sound_srate(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(res));
}

static SCM g_sound_header_type(SCM filename) 
{
  #define H_mus_sound_header_type "(" S_mus_sound_header_type " filename) -> header type (e.g. AIFF) of sound"
  char *tmpstr = NULL;
  int res;
  SCM_ASSERT(gh_string_p(filename), filename, SCM_ARG1, S_mus_sound_header_type); 
  res = mus_sound_header_type(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(res));
}

static SCM g_sound_data_format(SCM filename) 
{
  #define H_mus_sound_data_format "(" S_mus_sound_data_format " filename) -> data format (e.g. big endian short) of sound"
  char *tmpstr = NULL;
  int res;
  SCM_ASSERT(gh_string_p(filename), filename, SCM_ARG1, S_mus_sound_data_format); 
  res = mus_sound_data_format(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(res));
}

static SCM g_sound_length(SCM filename) 
{
  #define H_mus_sound_length "(" S_mus_sound_length " filename) -> file length in bytes"
  char *tmpstr = NULL;
  int res;
  SCM_ASSERT(gh_string_p(filename), filename, SCM_ARG1, S_mus_sound_length); 
  res = mus_sound_length(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(res));
}

static SCM g_sound_type_specifier(SCM filename) 
{
  #define H_mus_sound_type_specifier "(" S_mus_sound_type_specifier " filename) -> original file type identifier (e.g. 0x2e736e64)"
  char *tmpstr = NULL;
  int res;
  SCM_ASSERT(gh_string_p(filename), filename, SCM_ARG1, S_mus_sound_type_specifier); 
  res = mus_sound_type_specifier(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(res));
}

static SCM g_sound_comment(SCM filename) 
{
  #define H_mus_sound_comment "(" S_mus_sound_comment " filename) -> comment (string) found in sound's header"
  char *tmpstr = NULL, *res = NULL; 
  SCM newstr;
  SCM_ASSERT(gh_string_p(filename), filename, SCM_ARG1, S_mus_sound_comment); 
  res = mus_sound_comment(tmpstr = full_filename(filename));
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
  SCM_ASSERT(gh_string_p(filename), filename, SCM_ARG1, S_mus_sound_write_date); 
  date = mus_sound_write_date(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(date));
}

static SCM g_sound_type_name(SCM type) 
{
  #define H_mus_header_type_name "(" S_mus_header_type_name " type) -> header type (e.g. mus-aiff) as a string"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(type)), type, SCM_ARG1, S_mus_header_type_name); 
  return(TO_SCM_STRING(mus_header_type_name(TO_C_INT_OR_ELSE(type, 0))));
}

static SCM g_sound_format_name(SCM format) 
{
  #define H_mus_data_format_name "(" S_mus_data_format_name " format) -> data format (e.g. mus-bshort) as a string"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(format)), format, SCM_ARG1, S_mus_data_format_name); 
  return(TO_SCM_STRING(mus_data_format_name(TO_C_INT_OR_ELSE(format, 0))));
}

static SCM g_sound_bytes_per_sample(SCM format) 
{
  #define H_mus_data_format_bytes_per_sample "(" S_mus_data_format_bytes_per_sample " format) -> number of bytes per sample in format (e.g. mus-short = 2)"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(format)), format, SCM_ARG1, S_mus_data_format_bytes_per_sample); 
  return(TO_SCM_INT(mus_data_format_to_bytes_per_sample(TO_C_INT_OR_ELSE(format, 0))));
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
  SCM_ASSERT(gh_string_p(filename), filename, SCM_ARG1, S_mus_sound_duration); 
  res = mus_sound_duration(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_DOUBLE(res));

}

static SCM g_audio_outputs(SCM speakers, SCM headphones, SCM line_out)
{
  #define H_mus_audio_sun_outputs "(" S_mus_audio_sun_outputs " speaker headphones line-out) sets the current Sun audio outputs"
#ifdef SUN
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(speakers)), speakers, SCM_ARG1, S_mus_audio_sun_outputs);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(headphones)), headphones, SCM_ARG2, S_mus_audio_sun_outputs);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(line_out)), line_out, SCM_ARG3, S_mus_audio_sun_outputs);
  mus_audio_sun_outputs(TO_C_INT_OR_ELSE(speakers, 0),
			TO_C_INT_OR_ELSE(headphones, 0),
			TO_C_INT_OR_ELSE(line_out, 0));
#endif
  return(SCM_BOOL_F);
}

static SCM g_sound_max_amp_exists(SCM file)
{
  #define H_mus_sound_max_amp_exists "(" S_mus_sound_max_amp_exists " filename) -> max amps available for sound"
  int val;
  char *filename;
  SCM_ASSERT(gh_string_p(file), file, SCM_ARG1, S_mus_sound_max_amp);
  filename = full_filename(file);
  val = mus_sound_max_amp_exists(filename);
  if (filename) FREE(filename);
  return(TO_SCM_BOOLEAN(val));
}

/* TODO: bring out mus_sound_set_max_amp */

static SCM g_sound_max_amp(SCM file)
{
  #define H_mus_sound_max_amp "(" S_mus_sound_max_amp " filename) -> max amps in sound (a vector of amps and locations)"
  int chans, rtn, i;
  MUS_SAMPLE_TYPE *vals;
  char *filename;
  SCM *vdata;
  SCM vect = SCM_BOOL_F;
  SCM_ASSERT(gh_string_p(file), file, SCM_ARG1, S_mus_sound_max_amp);
  filename = full_filename(file);
  chans = mus_sound_chans(filename);
  if (chans > 0)
    {
      vals = (MUS_SAMPLE_TYPE *)CALLOC(chans*2, sizeof(MUS_SAMPLE_TYPE));
      rtn = mus_sound_max_amp(filename, vals);
      if (rtn > 0)
	{
	  vect = gh_make_vector(TO_SMALL_SCM_INT(chans*2), TO_SMALL_SCM_INT(0));
	  vdata = SCM_VELTS(vect);
	  for (i = 0; i < chans*2; i+=2)
	    {
	      vdata[i] = TO_SCM_INT((int)(vals[i]));
	      vdata[i+1] = TO_SCM_DOUBLE(MUS_SAMPLE_TO_FLOAT(vals[i+1]));
	    }
	}
      FREE(vals);
    }
  if (filename) FREE(filename);
  return(vect);
}

/* to support the actual sound file/audio port stuff, we need an "smob" for the int** arrays */

static SND_TAG_TYPE sound_data_tag = 0;

static SCM mark_sound_data(SCM obj)
{
  SND_SETGCMARK(obj);
  return(SCM_BOOL_F);
}

int sound_data_p(SCM obj)
{
  return((SCM_NIMP(obj)) && (SND_SMOB_TYPE(sound_data_tag, obj)));
}

static SCM g_sound_data_p(SCM obj) 
{
  #define H_sound_data_p "(" S_sound_data_p " obj) -> #t if obj is a sound-data object, else #f"
  return((sound_data_p(obj)) ? SCM_BOOL_T : SCM_BOOL_F);
}

static MUS_SAMPLE_TYPE **get_sound_data(SCM arg)
{
  sound_data *sd;
  if (sound_data_p(arg))
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
  FREE(v);
  return(0);
}

static int print_sound_data(SCM obj, SCM port, scm_print_state *pstate)
{
  char *buf;
  sound_data *v = (sound_data *)SND_VALUE_OF(obj);
  if (v == NULL)
    scm_puts("<null>", port);
  else
    {
      buf = (char *)CALLOC(64, sizeof(char));
      sprintf(buf, "#<sound-data: %d chan%s, %d frame%s>",
	      v->chans, (v->chans == 1) ? "" : "s",
	      v->length, (v->length == 1) ? "" : "s");
      scm_puts(buf, port);
      FREE(buf);
    }
  SND_REMEMBER(obj);
  return(1);
}

static SCM equalp_sound_data(SCM obj1, SCM obj2)
{
  sound_data *v1, *v2;
  v1 = (sound_data *)SND_VALUE_OF(obj1);
  v2 = (sound_data *)SND_VALUE_OF(obj2);
  if (v1 == v2) return(SCM_BOOL_T);
  return(SCM_BOOL_F);
}

static SCM sound_data_length(SCM obj)
{
  #define H_sound_data_length "(" S_sound_data_length " sd) -> length (samples) of each channel of sound-data object sd"
  sound_data *v = (sound_data *)SND_VALUE_OF(obj);
  if (v == NULL) return(0);
  return(TO_SCM_INT(v->length));
}

static SCM sound_data_chans(SCM obj)
{
  #define H_sound_data_chans "(" S_sound_data_chans " sd) -> number of channels in sound-data object sd"
  sound_data *v = (sound_data *)SND_VALUE_OF(obj);
  if (v == NULL) return(0);
  return(TO_SMALL_SCM_INT(v->chans));
}

SCM make_sound_data(int chans, int frames)
{
  #define H_make_sound_data "(" S_make_sound_data " chans frames) -> new sound-data object with chans channels, each having frames samples"
  int i;
  sound_data *new_sound_data;
  new_sound_data = (sound_data *)CALLOC(1, sizeof(sound_data));
  new_sound_data->length = frames;
  new_sound_data->chans = chans;
  new_sound_data->data = (MUS_SAMPLE_TYPE **)CALLOC(chans, sizeof(MUS_SAMPLE_TYPE *));
  for (i = 0; i < chans; i++)
    new_sound_data->data[i] = (MUS_SAMPLE_TYPE *)CALLOC(frames, sizeof(MUS_SAMPLE_TYPE));
  SND_RETURN_NEWSMOB(sound_data_tag, new_sound_data);
}

#if (!(HAVE_NEW_SMOB))
static scm_smobfuns sound_data_smobfuns = {
  &mark_sound_data,
  &free_sound_data,
  &print_sound_data,
  &equalp_sound_data};
#endif

static SCM g_make_sound_data(SCM chans, SCM frames)
{
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chans)), chans, SCM_ARG1, S_make_sound_data);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(frames)), frames, SCM_ARG1, S_make_sound_data);
  return(make_sound_data(TO_C_INT_OR_ELSE(chans, 0),
			 TO_C_INT_OR_ELSE(frames, 0)));
}

static SCM sound_data_ref(SCM obj, SCM chan, SCM frame)
{
  #define H_sound_data_ref "(" S_sound_data_ref " sd chan i) -> sample in channel chan at location i of sound-data object sd: sd[chan][i]"
  sound_data *v = (sound_data *)SND_VALUE_OF(obj);
  int loc, chn;
  SCM_ASSERT(sound_data_p(obj), obj, SCM_ARG1, S_sound_data_ref);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chan)), chan, SCM_ARG2, S_sound_data_ref);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(frame)), frame, SCM_ARG3, S_sound_data_ref);
  if (v)
    {
      chn = TO_C_INT_OR_ELSE(chan, 0);
      if ((chn >= 0) && (chn < v->chans))
	{
	  loc = TO_C_INT_OR_ELSE(frame, 0);
	  if ((loc >= 0) && (loc < v->length))
	    return(TO_SCM_DOUBLE(MUS_SAMPLE_TO_DOUBLE(v->data[chn][loc])));
	  else scm_misc_error(S_sound_data_ref, "invalid frame: ~S: ~S", SCM_LIST2(obj, frame));
	}
      else scm_misc_error(S_sound_data_ref, "invalid channel: ~S[~S]", SCM_LIST2(obj, chan));
    }
  else scm_misc_error(S_sound_data_ref, "nil sound-data?", SCM_EOL);
  return(TO_SCM_DOUBLE(0.0));
}

#if HAVE_APPLICABLE_SMOB
static SCM sound_data_apply(SCM obj, SCM chan, SCM i)
{
  return(sound_data_ref(obj, chan, i));
}
#endif

static SCM sound_data_set(SCM obj, SCM chan, SCM frame, SCM val)
{
  #define H_sound_data_setB "(" S_sound_data_setB " sd chan i val): set sound-data object sd's i-th element in channel chan to val: sd[chan][i] = val"
  sound_data *v = (sound_data *)SND_VALUE_OF(obj);
  int loc, chn;
  if (v)
    {
      chn = TO_C_INT_OR_ELSE(chan, 0);
      if ((chn >= 0) && (chn < v->chans))
	{
	  loc = TO_C_INT_OR_ELSE(frame, 0);
	  if ((loc >= 0) && 
	      (loc < v->length))
	    v->data[chn][loc] = MUS_DOUBLE_TO_SAMPLE(TO_C_DOUBLE(val));
	  else scm_misc_error(S_sound_data_setB,
			      "invalid frame: ~S[~S]: ~S",
			      SCM_LIST3(obj, chan, frame));
	}
      else scm_misc_error(S_sound_data_setB,
			  "invalid channel: ~S[~S]: ~S",
			  SCM_LIST3(obj, chan, frame));
    }
  else scm_misc_error(S_sound_data_setB, "nil sound-data?", SCM_EOL);
  return(val);
}

static SCM sound_data2vct(SCM sdobj, SCM chan, SCM vobj)
{
  #define H_sound_data2vct "(" S_sound_data2vct " sd chan v) places sound-data object sd's channel chan data into vct object v"
  vct *v;
  sound_data *sd;
  int len, i, chn;
  SCM_ASSERT(sound_data_p(sdobj), sdobj, SCM_ARG1, S_sound_data2vct);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chan)), chan, SCM_ARG2, S_sound_data2vct);
  SCM_ASSERT(vct_p(vobj), vobj, SCM_ARG3, S_sound_data2vct);
  v = (vct *)get_vct(vobj);
  sd = (sound_data *)SND_VALUE_OF(sdobj);
  chn = TO_C_INT_OR_ELSE(chan, 0);
  if (chn < sd->chans)
    {
      if (sd->length < v->length) 
	len = sd->length; 
      else len = v->length;
      for (i = 0; i < len; i++) 
	v->data[i] = (Float)(MUS_SAMPLE_TO_FLOAT(sd->data[chn][i]));
    }
  else scm_misc_error(S_sound_data2vct,
		      "invalid channel: ~S[~S]: ~S",
		      SCM_LIST3(sdobj, chan, vobj));
  return(vobj);
}

static SCM vct2sound_data(SCM vobj, SCM sdobj, SCM chan)
{
  #define H_vct2sound_data "(" S_vct2sound_data " v sd chan) places vct v's data into sound-data sd's channel chan"
  vct *v;
  sound_data *sd;
  int len, i, chn;
  SCM_ASSERT(vct_p(vobj), vobj, SCM_ARG1, S_vct2sound_data);
  SCM_ASSERT(sound_data_p(sdobj), sdobj, SCM_ARG2, S_vct2sound_data);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chan)), chan, SCM_ARG3, S_vct2sound_data);
  v = (vct *)get_vct(vobj);
  sd = (sound_data *)SND_VALUE_OF(sdobj);
  chn = TO_C_INT_OR_ELSE(chan, 0);
  if (chn < sd->chans)
    {
      if (sd->length < v->length) 
	len = sd->length; 
      else len = v->length;
      for (i = 0; i < len; i++) 
	sd->data[chn][i] = MUS_FLOAT_TO_SAMPLE(v->data[i]);
    }
  else scm_misc_error(S_vct2sound_data,
		      "invalid channel: ~S[~S]: ~S",
		      SCM_LIST3(vobj, chan, sdobj));
  return(vobj);
}


static SCM g_open_sound_input(SCM file)
{
  #define H_mus_sound_open_input "(" S_mus_sound_open_input " filename) -> fd (int), opens filename for sound input"
  int fd;
  char *tmpstr = NULL;
  SCM_ASSERT(gh_string_p(file), file, SCM_ARG1, S_mus_sound_open_input);
  fd = mus_sound_open_input(tmpstr = full_filename(file));
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

  int fd;
  char *com = NULL;
  char *tmpstr = NULL;
  SCM_ASSERT(gh_string_p(file), file, SCM_ARG1, S_mus_sound_open_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(srate)), srate, SCM_ARG2, S_mus_sound_open_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chans)), chans, SCM_ARG3, S_mus_sound_open_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(header_type)), header_type, SCM_ARG5, S_mus_sound_open_output);
  SCM_ASSERT((gh_string_p(comment) || (SCM_UNBNDP(comment))), comment, SCM_ARG6, S_mus_sound_open_output);
  if (gh_string_p(comment)) com = TO_NEW_C_STRING(comment);
  fd = mus_sound_open_output(tmpstr = full_filename(file),
			     TO_C_INT_OR_ELSE(srate, 0),
			     TO_C_INT_OR_ELSE(chans, 0),
			     TO_C_INT_OR_ELSE(data_format, MUS_OUT_FORMAT),
			     TO_C_INT_OR_ELSE(header_type, 0), com);
  if (tmpstr) FREE(tmpstr);
  if (com) free(com);
  return(TO_SCM_INT(fd));
}

static SCM g_reopen_sound_output(SCM file, SCM chans, SCM data_format, SCM header_type, SCM data_loc)
{

  #define H_mus_sound_reopen_output "(" S_mus_sound_reopen_output " filename chans data-format header-type data-location)\n\
reopen (without alteration) filename for output \
data-format and header-type are sndlib indicators such as mus-bshort or mus-aiff \
data-location should be retrieved from a previous call to mus-sound-data-location"

  int fd;
  char *tmpstr = NULL;
  SCM_ASSERT(gh_string_p(file), file, SCM_ARG1, S_mus_sound_reopen_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chans)), chans, SCM_ARG2, S_mus_sound_reopen_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(header_type)), header_type, SCM_ARG4, S_mus_sound_reopen_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(data_loc)), data_loc, SCM_ARG5, S_mus_sound_reopen_output);
  fd = mus_sound_reopen_output(tmpstr = full_filename(file),
			       TO_C_INT_OR_ELSE(chans, 0),
			       TO_C_INT_OR_ELSE(data_format, MUS_OUT_FORMAT),
			       TO_C_INT_OR_ELSE(header_type, 0),
			       TO_C_INT_OR_ELSE(data_loc, 0));
  if (tmpstr) FREE(tmpstr);
  return(TO_SCM_INT(fd));
}

static SCM g_close_sound_input(SCM fd)
{
  #define H_mus_sound_close_input "(" S_mus_sound_close_input " fd) closes file number fd"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(fd)), fd, SCM_ARG1, S_mus_sound_close_input);
  return(TO_SCM_INT(mus_sound_close_input(TO_C_INT_OR_ELSE(fd, 0))));
}

static SCM g_close_sound_output(SCM fd, SCM bytes)
{
  #define H_mus_sound_close_output "(" S_mus_sound_close_output " fd bytes) closes file number fd \
after updating its header (if any) to reflect bytes, the new file data size"

  SCM_ASSERT(SCM_NFALSEP(scm_real_p(fd)), fd, SCM_ARG1, S_mus_sound_close_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(bytes)), bytes, SCM_ARG2, S_mus_sound_close_output);
  return(TO_SCM_INT(mus_sound_close_output(TO_C_INT_OR_ELSE(fd, 0),
					   TO_C_INT_OR_ELSE(bytes, 0))));
}

static SCM g_read_sound(SCM fd, SCM beg, SCM end, SCM chans, SCM sv)
{
  #define H_mus_sound_read "(" S_mus_sound_read " fd beg end chans sdata) reads sound data from file number fd, \
filling the sound-data object sdata's buffers starting at (buffer location) beg, going to end"

  SCM_ASSERT(SCM_NFALSEP(scm_real_p(fd)), fd, SCM_ARG1, S_mus_sound_read);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(beg)), beg, SCM_ARG2, S_mus_sound_read);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(end)), end, SCM_ARG3, S_mus_sound_read);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chans)), chans, SCM_ARG4, S_mus_sound_read);
  SCM_ASSERT(sound_data_p(sv), sv, SCM_ARG5, S_mus_sound_read);
  return(TO_SCM_INT(mus_sound_read(TO_C_INT_OR_ELSE(fd, 0),
				   TO_C_INT_OR_ELSE(beg, 0),
				   TO_C_INT_OR_ELSE(end, 0),
				   TO_C_INT_OR_ELSE(chans, 0),
				   get_sound_data(sv))));
}

static SCM g_write_sound(SCM fd, SCM beg, SCM end, SCM chans, SCM sv)
{
  #define H_mus_sound_write "(" S_mus_sound_write " fd beg end chans sdata) writes sound-data object sdata's data \
starting at (buffer location) beg, going to end, writing to file number fd"

  SCM_ASSERT(SCM_NFALSEP(scm_real_p(fd)), fd, SCM_ARG1, S_mus_sound_write);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(beg)), beg, SCM_ARG2, S_mus_sound_write);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(end)), end, SCM_ARG3, S_mus_sound_write);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chans)), chans, SCM_ARG4, S_mus_sound_write);
  SCM_ASSERT(sound_data_p(sv), sv, SCM_ARG5, S_mus_sound_write);
  return(TO_SCM_INT(mus_sound_write(TO_C_INT_OR_ELSE(fd, 0),
				    TO_C_INT_OR_ELSE(beg, 0),
				    TO_C_INT_OR_ELSE(end, 0),
				    TO_C_INT_OR_ELSE(chans, 0),
				    get_sound_data(sv))));
}

static SCM g_seek_sound(SCM fd, SCM offset, SCM origin)
{
  #define H_mus_sound_seek "(" S_mus_sound_seek " fd offset origin) moves the current read/write location in file number fd \
to the short-wise sample offset given origin (both treated as in lseek)"

  SCM_ASSERT(SCM_NFALSEP(scm_real_p(fd)), fd, SCM_ARG1, S_mus_sound_seek);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(offset)), offset, SCM_ARG2, S_mus_sound_seek);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(origin)), origin, SCM_ARG3, S_mus_sound_seek);
  return(TO_SCM_INT(mus_sound_seek(TO_C_INT_OR_ELSE(fd, 0),
				   TO_C_INT_OR_ELSE(offset, 0),
				   TO_C_INT_OR_ELSE(origin, 0))));
}

static SCM g_seek_sound_frame(SCM fd, SCM offset)
{
  #define H_mus_sound_seek_frame "(" S_mus_sound_seek_frame " fd frame) moves the current read/write location in file number fd \
to the indicated frame"

  SCM_ASSERT(SCM_NFALSEP(scm_real_p(fd)), fd, SCM_ARG1, S_mus_sound_seek_frame);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(offset)), offset, SCM_ARG2, S_mus_sound_seek_frame);
  return(TO_SCM_INT(mus_sound_seek_frame(TO_C_INT_OR_ELSE(fd, 0),
					 TO_C_INT_OR_ELSE(offset, 0))));
}

static SCM g_open_audio_output(SCM dev, SCM srate, SCM chans, SCM format, SCM size)
{
  #define H_mus_audio_open_output "(" S_mus_audio_open_output " device srate chans format clipped)\n\
opens the audio device ready for output at the given srate and so on. \
returns the audio line number:\n\
  (set! line (mus-audio-open-output mus-audio-default 22050 1 mus-lshort 256)"

  SCM_ASSERT(SCM_NFALSEP(scm_real_p(dev)), dev, SCM_ARG1, S_mus_audio_open_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(srate)), srate, SCM_ARG2, S_mus_audio_open_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chans)), chans, SCM_ARG3, S_mus_audio_open_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(format)), format, SCM_ARG4, S_mus_audio_open_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(size)), size, SCM_ARG5, S_mus_audio_open_output);
  return(TO_SCM_INT(mus_audio_open_output(TO_C_INT_OR_ELSE(dev, 0),
					  TO_C_INT_OR_ELSE(srate, 0),
					  TO_C_INT_OR_ELSE(chans, 0),
					  TO_C_INT_OR_ELSE(format, 0),
					  TO_C_INT_OR_ELSE(size, 0))));
}

static SCM g_open_audio_input(SCM dev, SCM srate, SCM chans, SCM format, SCM size)
{
  #define H_mus_audio_open_input "(" S_mus_audio_open_input " (device srate chans format bufsize)\n\
opens the audio device ready for input with the indicated attributes, returns the audio line number"

  SCM_ASSERT(SCM_NFALSEP(scm_real_p(dev)), dev, SCM_ARG1, S_mus_audio_open_input);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(srate)), srate, SCM_ARG2, S_mus_audio_open_input);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chans)), chans, SCM_ARG3, S_mus_audio_open_input);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(format)), format, SCM_ARG4, S_mus_audio_open_input);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(size)), size, SCM_ARG5, S_mus_audio_open_input);
  return(TO_SCM_INT(mus_audio_open_input(TO_C_INT_OR_ELSE(dev, 0),
					 TO_C_INT_OR_ELSE(srate, 0),
					 TO_C_INT_OR_ELSE(chans, 0),
					 TO_C_INT_OR_ELSE(format, 0),
					 TO_C_INT_OR_ELSE(size, 0))));
}

static SCM g_close_audio(SCM line)
{
  #define H_mus_audio_close "(" S_mus_audio_close " line) closes the audio hardware port line"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(line)), line, SCM_ARG1, S_mus_audio_close);
  return(TO_SCM_INT(mus_audio_close(TO_C_INT_OR_ELSE(line, 0))));
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
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(line)), line, SCM_ARG1, S_mus_audio_write);
  SCM_ASSERT(sound_data_p(sdata), sdata, SCM_ARG2, S_mus_audio_write);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(frames)), frames, SCM_ARG3, S_mus_audio_write);
  sd = (sound_data *)SND_VALUE_OF(sdata);
  bufs = sd->data;
  frms = TO_C_INT_OR_ELSE(frames, 0);
  outbytes = frms * sd->chans * 2;
  obuf = (short *)CALLOC(frms * sd->chans, sizeof(short));
  if (sd->chans == 1)
    {
      for (k = 0; k < frms; k++) 
	obuf[k] = MUS_SAMPLE_TO_SHORT(bufs[0][k]);
    }
  else
    {
      for (k = 0, j = 0; k < frms; k++, j+=sd->chans)
	for (i = 0; i < sd->chans; i++) 
	  obuf[j+i] = MUS_SAMPLE_TO_SHORT(bufs[i][k]);
    }
  val = mus_audio_write(TO_C_INT_OR_ELSE(line, 0), (char *)obuf, outbytes);
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
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(line)), line, SCM_ARG1, S_mus_audio_read);
  SCM_ASSERT(sound_data_p(sdata), sdata, SCM_ARG2, S_mus_audio_read);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(frames)), frames, SCM_ARG3, S_mus_audio_read);
  sd = (sound_data *)SND_VALUE_OF(sdata);
  bufs = sd->data;
  frms = TO_C_INT_OR_ELSE(frames, 0);
  inbytes = frms * sd->chans * 2;
  inbuf = (short *)CALLOC(frms * sd->chans, sizeof(short));
  val = mus_audio_read(TO_C_INT_OR_ELSE(line, 0), (char *)inbuf, inbytes);
  if (sd->chans == 1)
    {
      for (k = 0; k < frms; k++) 
	bufs[0][k] = MUS_SHORT_TO_SAMPLE(inbuf[k]);
    }
  else
    {
      for (k = 0, j = 0; k < frms; k++, j+=sd->chans)
	for (i = 0; i < sd->chans; i++) 
	  bufs[i][k] = MUS_SHORT_TO_SAMPLE(inbuf[j+i]);
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
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(dev)), dev, SCM_ARG1, S_mus_audio_mixer_read);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(field)), field, SCM_ARG2, S_mus_audio_mixer_read);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chan)), chan, SCM_ARG3, S_mus_audio_mixer_read);
  SCM_ASSERT(gh_vector_p(vals), vals, SCM_ARG4, S_mus_audio_mixer_read);
  len = gh_vector_length(vals);
  if (len == 0)
    fvals = (float *)CALLOC(1, sizeof(float));
  else fvals = (float *)CALLOC(len, sizeof(float));
  val = mus_audio_mixer_read(TO_C_INT_OR_ELSE(dev, 0),
			     TO_C_INT_OR_ELSE(field, 0),
			     TO_C_INT_OR_ELSE(chan, 0),
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
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(dev)), dev, SCM_ARG1, S_mus_audio_mixer_write);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(field)), field, SCM_ARG2, S_mus_audio_mixer_write);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chan)), chan, SCM_ARG3, S_mus_audio_mixer_write);
  SCM_ASSERT(gh_vector_p(vals), vals, SCM_ARG4, S_mus_audio_mixer_write);
  len = gh_vector_length(vals);
  if (len == 0)
    fvals = (float *)CALLOC(1, sizeof(float));
  else
    {
      fvals = (float *)CALLOC(len, sizeof(float));
      vdata = SCM_VELTS(vals);
      for (i = 0; i < len; i++) 
	fvals[i] = TO_C_DOUBLE(vdata[i]);
    }
  res = mus_audio_mixer_write(TO_C_INT_OR_ELSE(dev, 0),
			      TO_C_INT_OR_ELSE(field, 0),
			      TO_C_INT_OR_ELSE(chan, 0),
			      fvals);
  FREE(fvals);
  return(TO_SCM_INT(res));
}

static SCM g_mus_set_data_clipped(SCM fd, SCM clipped)
{
  #define H_mus_file_set_data_clipped "(" S_mus_file_set_data_clipped " fd val) sets whether data associated with file fd is clipped or wraps around"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(fd)), fd, SCM_ARG1, S_mus_file_set_data_clipped);
  SCM_ASSERT(gh_boolean_p(clipped), fd, SCM_ARG1, S_mus_file_set_data_clipped);
  return(TO_SCM_INT(mus_file_set_data_clipped(TO_C_INT_OR_ELSE(fd, 0),
					      (SCM_FALSEP(clipped)) ? 0 : 1)));
}

static SCM g_mus_prescaler(SCM fd)
{
  #define H_mus_file_prescaler "(" S_mus_file_prescaler " fd) -> current prescaler (normally 1.0) associated with fd"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(fd)), fd, SCM_ARG1, S_mus_file_set_prescaler);
  return(TO_SCM_DOUBLE(mus_file_prescaler(TO_C_INT_OR_ELSE(fd, 0))));
}

static SCM g_mus_set_prescaler(SCM fd, SCM val)
{
  #define H_mus_file_set_prescaler "(" S_mus_file_set_prescaler " fd val) sets the current prescaler associated with fd"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(fd)), fd, SCM_ARG1, S_mus_file_set_prescaler);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)), val, SCM_ARG2, S_mus_file_set_prescaler);
  return(TO_SCM_DOUBLE(mus_file_set_prescaler(TO_C_INT_OR_ELSE(fd, 0),
					      TO_C_DOUBLE(val))));
}

#if DEBUGGING
static SCM g_sound_print_cache(void)
{
  mus_sound_print_cache();
  return(SCM_BOOL_F);
}
#endif

#ifdef __cplusplus
  static SCM gh_new_procedure5_1 (char *proc_name, SCM (*fn)(SCM, SCM, SCM, SCM, SCM, SCM)) {return(gh_new_procedure(proc_name, SCM_FNC fn, 5, 1, 0));}
#else
  static SCM gh_new_procedure5_1 (char *proc_name, SCM (*fn)()) {return(gh_new_procedure(proc_name, fn, 5, 1, 0));}
#endif

void mus_sndlib2scm_initialize(void)
{
  SCM local_doc;
  mus_sound_initialize();
  local_doc = scm_permanent_object(scm_string_to_symbol(TO_SCM_STRING("documentation")));

#if HAVE_NEW_SMOB
  sound_data_tag = scm_make_smob_type("sound-data", sizeof(sound_data));
  scm_set_smob_mark(sound_data_tag, mark_sound_data);
  scm_set_smob_print(sound_data_tag, print_sound_data);
  scm_set_smob_free(sound_data_tag, free_sound_data);
  scm_set_smob_equalp(sound_data_tag, equalp_sound_data);
#if HAVE_APPLICABLE_SMOB
  scm_set_smob_apply(sound_data_tag, SCM_FNC sound_data_apply, 2, 0, 0);
#endif
#else
  sound_data_tag = scm_newsmob(&sound_data_smobfuns);
#endif

  DEFINE_VAR(S_mus_next,  TO_SMALL_SCM_INT(MUS_NEXT),  "NeXT (Sun) sound header id");
  DEFINE_VAR(S_mus_aifc,  TO_SMALL_SCM_INT(MUS_AIFC),  "AIFC sound header id");
  DEFINE_VAR(S_mus_riff,  TO_SMALL_SCM_INT(MUS_RIFF),  "RIFF (wave) sound header id");
  DEFINE_VAR(S_mus_nist,  TO_SMALL_SCM_INT(MUS_NIST),  "NIST (Sphere) sound header id");
  DEFINE_VAR(S_mus_raw,   TO_SMALL_SCM_INT(MUS_RAW),   "raw (headerless) sound header id");
  DEFINE_VAR(S_mus_ircam, TO_SMALL_SCM_INT(MUS_IRCAM), "IRCAM sound header id");
  DEFINE_VAR(S_mus_aiff,  TO_SMALL_SCM_INT(MUS_AIFF),  "AIFF (old-style) sound header id");

  DEFINE_VAR(S_mus_bshort,  TO_SMALL_SCM_INT(MUS_BSHORT),  "big-endian short data format id");
  DEFINE_VAR(S_mus_lshort,  TO_SMALL_SCM_INT(MUS_LSHORT),  "little-endian short data format id");
  DEFINE_VAR(S_mus_mulaw,   TO_SMALL_SCM_INT(MUS_MULAW),   "mulaw (8-bit) data format id");
  DEFINE_VAR(S_mus_alaw,    TO_SMALL_SCM_INT(MUS_ALAW),    "alaw (8-bit) data format id");
  DEFINE_VAR(S_mus_byte,    TO_SMALL_SCM_INT(MUS_BYTE),    "signed byte data format id");
  DEFINE_VAR(S_mus_ubyte,   TO_SMALL_SCM_INT(MUS_UBYTE),   "unsigned byte data format id");
  DEFINE_VAR(S_mus_bfloat,  TO_SMALL_SCM_INT(MUS_BFLOAT),  "big-endian float data format id");
  DEFINE_VAR(S_mus_lfloat,  TO_SMALL_SCM_INT(MUS_LFLOAT),  "little-endian float data format id");
  DEFINE_VAR(S_mus_bint,    TO_SMALL_SCM_INT(MUS_BINT),    "big-endian int data format id");
  DEFINE_VAR(S_mus_lint,    TO_SMALL_SCM_INT(MUS_LINT),    "little-endian int data format id");
  DEFINE_VAR(S_mus_bintn,   TO_SMALL_SCM_INT(MUS_BINTN),   "normalized big-endian int data format id");
  DEFINE_VAR(S_mus_lintn,   TO_SMALL_SCM_INT(MUS_LINTN),   "normalized little-endian int data format id");
  DEFINE_VAR(S_mus_b24int,  TO_SMALL_SCM_INT(MUS_B24INT),  "big-endian 24-bit data format id");
  DEFINE_VAR(S_mus_l24int,  TO_SMALL_SCM_INT(MUS_L24INT),  "little-endian 24-bit data format id");
  DEFINE_VAR(S_mus_bdouble, TO_SMALL_SCM_INT(MUS_BDOUBLE), "big-endian double data format id");
  DEFINE_VAR(S_mus_ldouble, TO_SMALL_SCM_INT(MUS_LDOUBLE), "little-endian double data format id");
  DEFINE_VAR(S_mus_ubshort, TO_SMALL_SCM_INT(MUS_UBSHORT), "unsigned big-endian short data format id");
  DEFINE_VAR(S_mus_ulshort, TO_SMALL_SCM_INT(MUS_ULSHORT), "unsigned little-endian short data format id");

  DEFINE_VAR(S_mus_audio_default,        TO_SMALL_SCM_INT(MUS_AUDIO_DEFAULT),        "default audio device");
  DEFINE_VAR(S_mus_audio_duplex_default, TO_SMALL_SCM_INT(MUS_AUDIO_DUPLEX_DEFAULT), "default duplex device");
  DEFINE_VAR(S_mus_audio_line_out,       TO_SMALL_SCM_INT(MUS_AUDIO_LINE_OUT),       "audio line-out device");
  DEFINE_VAR(S_mus_audio_line_in,        TO_SMALL_SCM_INT(MUS_AUDIO_LINE_IN),        "audio line-in device");
  DEFINE_VAR(S_mus_audio_microphone,     TO_SMALL_SCM_INT(MUS_AUDIO_MICROPHONE),     "microphone device");
  DEFINE_VAR(S_mus_audio_speakers,       TO_SMALL_SCM_INT(MUS_AUDIO_SPEAKERS),       "speakers device (a mixer kludge)");
  DEFINE_VAR(S_mus_audio_dac_out,        TO_SMALL_SCM_INT(MUS_AUDIO_DAC_OUT),        "DAC out device");
  DEFINE_VAR(S_mus_audio_adat_in,        TO_SMALL_SCM_INT(MUS_AUDIO_ADAT_IN),        "ADAT in device");
  DEFINE_VAR(S_mus_audio_aes_in,         TO_SMALL_SCM_INT(MUS_AUDIO_AES_IN),         "AES in device");
  DEFINE_VAR(S_mus_audio_digital_in,     TO_SMALL_SCM_INT(MUS_AUDIO_DIGITAL_IN),     "digital audio in device");
  DEFINE_VAR(S_mus_audio_digital_out,    TO_SMALL_SCM_INT(MUS_AUDIO_DIGITAL_OUT),    "digital audio out device");
  DEFINE_VAR(S_mus_audio_adat_out,       TO_SMALL_SCM_INT(MUS_AUDIO_ADAT_OUT),       "ADAT out device");
  DEFINE_VAR(S_mus_audio_aes_out,        TO_SMALL_SCM_INT(MUS_AUDIO_AES_OUT),        "AES out device");
  DEFINE_VAR(S_mus_audio_dac_filter,     TO_SMALL_SCM_INT(MUS_AUDIO_DAC_FILTER),     "DAC filter 'device' (a mixer kludge)");
  DEFINE_VAR(S_mus_audio_mixer,          TO_SMALL_SCM_INT(MUS_AUDIO_MIXER),          "the 'mixer' device");
  DEFINE_VAR(S_mus_audio_line1,          TO_SMALL_SCM_INT(MUS_AUDIO_LINE1),          "audio line 1 device");
  DEFINE_VAR(S_mus_audio_line2,          TO_SMALL_SCM_INT(MUS_AUDIO_LINE2),          "audio line 2device");
  DEFINE_VAR(S_mus_audio_line3,          TO_SMALL_SCM_INT(MUS_AUDIO_LINE3),          "audio line 3device");
  DEFINE_VAR(S_mus_audio_aux_input,      TO_SMALL_SCM_INT(MUS_AUDIO_AUX_INPUT),      "aux audio in device");
  DEFINE_VAR(S_mus_audio_cd,             TO_SMALL_SCM_INT(MUS_AUDIO_CD),             "CD in device");
  DEFINE_VAR(S_mus_audio_aux_output,     TO_SMALL_SCM_INT(MUS_AUDIO_AUX_OUTPUT),     "aux audio out device");
  DEFINE_VAR(S_mus_audio_spdif_in,       TO_SMALL_SCM_INT(MUS_AUDIO_SPDIF_IN),       "SPDIF in device");
  DEFINE_VAR(S_mus_audio_spdif_out,      TO_SMALL_SCM_INT(MUS_AUDIO_SPDIF_OUT),      "SPDIF out device");
  DEFINE_VAR(S_mus_audio_direction,      TO_SMALL_SCM_INT(MUS_AUDIO_DIRECTION),      "audio sample flow direction (mus-audio-read)");
  DEFINE_VAR(S_mus_audio_samples_per_channel, TO_SMALL_SCM_INT(MUS_AUDIO_SAMPLES_PER_CHANNEL), "samples per channel (mus-audio-read)");

  DEFINE_VAR(S_mus_audio_amp,     TO_SMALL_SCM_INT(MUS_AUDIO_AMP),     "mixer amp field id");
  DEFINE_VAR(S_mus_audio_srate,   TO_SMALL_SCM_INT(MUS_AUDIO_SRATE),   "mixer srate field id");
  DEFINE_VAR(S_mus_audio_channel, TO_SMALL_SCM_INT(MUS_AUDIO_CHANNEL), "mixer channel field id");
  DEFINE_VAR(S_mus_audio_format,  TO_SMALL_SCM_INT(MUS_AUDIO_FORMAT),  "mixer data format field id");
  DEFINE_VAR(S_mus_audio_port,    TO_SMALL_SCM_INT(MUS_AUDIO_PORT),    "mixer port");
  DEFINE_VAR(S_mus_audio_imix,    TO_SMALL_SCM_INT(MUS_AUDIO_IMIX),    "mixer 'imix' field id");
  DEFINE_VAR(S_mus_audio_igain,   TO_SMALL_SCM_INT(MUS_AUDIO_IGAIN),   "mixer 'igain' field id");
  DEFINE_VAR(S_mus_audio_reclev,  TO_SMALL_SCM_INT(MUS_AUDIO_RECLEV),  "mixer 'reclev' field id");
  DEFINE_VAR(S_mus_audio_pcm,     TO_SMALL_SCM_INT(MUS_AUDIO_PCM),     "mixer 'pcm' field id");
  DEFINE_VAR(S_mus_audio_pcm2,    TO_SMALL_SCM_INT(MUS_AUDIO_PCM2),    "mixer 'pcm2' field id");
  DEFINE_VAR(S_mus_audio_ogain,   TO_SMALL_SCM_INT(MUS_AUDIO_OGAIN),   "mixer 'ogain' field id");
  DEFINE_VAR(S_mus_audio_line,    TO_SMALL_SCM_INT(MUS_AUDIO_LINE),    "mixer 'line' field id");
  DEFINE_VAR(S_mus_audio_synth,   TO_SMALL_SCM_INT(MUS_AUDIO_SYNTH),   "mixer 'synth' field id");
  DEFINE_VAR(S_mus_audio_bass,    TO_SMALL_SCM_INT(MUS_AUDIO_BASS),    "mixer 'bass' field id");
  DEFINE_VAR(S_mus_audio_treble,  TO_SMALL_SCM_INT(MUS_AUDIO_TREBLE),  "mixer 'treble' field id");

  DEFINE_PROC(gh_new_procedure1_0(S_sound_data_length,        sound_data_length),       H_sound_data_length);
  DEFINE_PROC(gh_new_procedure1_0(S_sound_data_chans,         sound_data_chans),        H_sound_data_chans);
  DEFINE_PROC(gh_new_procedure3_0(S_sound_data_ref,           sound_data_ref),          H_sound_data_ref);
  DEFINE_PROC(gh_new_procedure4_0(S_sound_data_setB,          sound_data_set),          H_sound_data_setB);
  DEFINE_PROC(gh_new_procedure2_0(S_make_sound_data,          g_make_sound_data),       H_make_sound_data);
  DEFINE_PROC(gh_new_procedure1_0(S_sound_data_p,             g_sound_data_p),          H_sound_data_p);
  DEFINE_PROC(gh_new_procedure3_0(S_sound_data2vct,           sound_data2vct),          H_sound_data2vct);
  DEFINE_PROC(gh_new_procedure3_0(S_vct2sound_data,           vct2sound_data),          H_vct2sound_data);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_samples,        g_sound_samples),         H_mus_sound_samples);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_frames,         g_sound_frames),          H_mus_sound_frames);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_duration,       g_sound_duration),        H_mus_sound_duration);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_datum_size,     g_sound_datum_size),      H_mus_sound_datum_size);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_data_location,  g_sound_data_location),   H_mus_sound_data_location);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_chans,          g_sound_chans),           H_mus_sound_chans);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_srate,          g_sound_srate),           H_mus_sound_srate);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_header_type,    g_sound_header_type),     H_mus_sound_header_type);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_data_format,    g_sound_data_format),     H_mus_sound_data_format);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_length,         g_sound_length),          H_mus_sound_length	);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_type_specifier, g_sound_type_specifier),  H_mus_sound_type_specifier);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_header_type_name,     g_sound_type_name),       H_mus_header_type_name);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_data_format_name,     g_sound_format_name),     H_mus_data_format_name);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_comment,        g_sound_comment),         H_mus_sound_comment);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_write_date,     g_sound_write_date),      H_mus_sound_write_date);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_data_format_bytes_per_sample, g_sound_bytes_per_sample), H_mus_data_format_bytes_per_sample);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_loop_info,      g_sound_loop_info),       H_mus_sound_loop_info);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_max_amp,        g_sound_max_amp),         H_mus_sound_max_amp);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_max_amp_exists, g_sound_max_amp_exists),  H_mus_sound_max_amp_exists);
  DEFINE_PROC(gh_new_procedure0_0(S_mus_audio_report,         g_report_audio_state),    H_mus_audio_report);
  DEFINE_PROC(gh_new_procedure3_0(S_mus_audio_sun_outputs,    g_audio_outputs),         H_mus_audio_sun_outputs);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_open_input,     g_open_sound_input),      H_mus_sound_open_input);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_close_input,    g_close_sound_input),     H_mus_sound_close_input);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_audio_close,          g_close_audio),           H_mus_audio_close);
  DEFINE_PROC(gh_new_procedure0_0(S_mus_audio_save,           g_save_audio_state),      H_mus_audio_save);
  DEFINE_PROC(gh_new_procedure0_0(S_mus_audio_restore,        g_restore_audio_state),   H_mus_audio_restore);
  DEFINE_PROC(gh_new_procedure0_0(S_mus_audio_systems,        g_audio_systems),         H_mus_audio_systems);
  DEFINE_PROC(gh_new_procedure4_0(S_mus_audio_mixer_read,     g_read_audio_state),      H_mus_audio_mixer_read);
  DEFINE_PROC(gh_new_procedure4_0(S_mus_audio_mixer_write,    g_write_audio_state),     H_mus_audio_mixer_write);
  DEFINE_PROC(gh_new_procedure2_0(S_mus_file_set_data_clipped, g_mus_set_data_clipped), H_mus_file_set_data_clipped);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_file_prescaler,       g_mus_prescaler),         H_mus_file_prescaler);
  DEFINE_PROC(gh_new_procedure2_0(S_mus_file_set_prescaler,   g_mus_set_prescaler),     H_mus_file_set_prescaler);
  DEFINE_PROC(gh_new_procedure3_0(S_mus_audio_write,          g_write_audio),           H_mus_audio_write);
  DEFINE_PROC(gh_new_procedure3_0(S_mus_audio_read,           g_read_audio),            H_mus_audio_read);
  DEFINE_PROC(gh_new_procedure5_1(S_mus_sound_open_output,    g_open_sound_output),     H_mus_sound_open_output);
  DEFINE_PROC(gh_new_procedure5_0(S_mus_sound_reopen_output,  g_reopen_sound_output),   H_mus_sound_reopen_output);
  DEFINE_PROC(gh_new_procedure2_0(S_mus_sound_close_output,   g_close_sound_output),    H_mus_sound_close_output);
  DEFINE_PROC(gh_new_procedure5_0(S_mus_sound_read,           g_read_sound),            H_mus_sound_read);
  DEFINE_PROC(gh_new_procedure5_0(S_mus_sound_write,          g_write_sound),           H_mus_sound_write);
  DEFINE_PROC(gh_new_procedure3_0(S_mus_sound_seek,           g_seek_sound),            H_mus_sound_seek);
  DEFINE_PROC(gh_new_procedure2_0(S_mus_sound_seek_frame,     g_seek_sound_frame),      H_mus_sound_seek_frame);
  DEFINE_PROC(gh_new_procedure5_0(S_mus_audio_open_output,    g_open_audio_output),     H_mus_audio_open_output);
  DEFINE_PROC(gh_new_procedure5_0(S_mus_audio_open_input,     g_open_audio_input),      H_mus_audio_open_input);
#if USE_SND
  define_procedure_with_setter(S_sound_data_ref, SCM_FNC sound_data_ref, H_sound_data_ref,
			       "set-" S_sound_data_ref, SCM_FNC sound_data_set, local_doc, 3, 0, 4, 0);
#endif
#if DEBUGGING
  gh_new_procedure0_0("mus-sound-print-cache", g_sound_print_cache);
#endif

  scm_add_feature("sndlib");
}

#endif

/*
void scm_init_sndlib_sndlib_module ()
{
  scm_register_module_xxx("sndlib sndlib", mus_sndlib2scm_initialize);
}
*/
