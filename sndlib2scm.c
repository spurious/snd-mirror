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

#include "sndlib.h"
#include "sndlib-strings.h"
#include "vct.h"
#include "sndlib2scm.h"
#include "sg.h"

static int g_scm2int(SCM obj)
{
  /* don't want errors here about floats with non-zero fractions etc */
  if (SCM_INUMP(obj))
    return(SCM_INUM(obj));
  else
    if (gh_number_p(obj))
      return((int)scm_num2dbl(obj,"g_scm2int"));
  return(0);
}

static char *full_filename(SCM file)
{
  char *urn,*filename;
  urn = gh_scm2newstr(file,NULL);
  filename = mus_file_full_name(urn);
  free(urn);
  return(filename);
}

static SCM g_sound_loop_info(SCM filename)
{
  #define H_mus_sound_loop_info "(" S_mus_sound_loop_info " filename) -> loop info for sound as a list\n   (start1 end1 start2 end2 base-note base-detune)"
  int *res;
  char *tmpstr;
  SCM sres = SCM_EOL;
  ERRS1(filename,S_mus_sound_loop_info); 
  tmpstr = full_filename(filename);
  res = mus_sound_loop_info(tmpstr);
  if (tmpstr) FREE(tmpstr);
  if (res)
    {
      sres = SCM_LIST6(gh_int2scm(res[0]),gh_int2scm(res[1]),gh_int2scm(res[2]),gh_int2scm(res[3]),gh_int2scm(res[4]),gh_int2scm(res[5]));
      FREE(res);
    }
  return(sres);
}

static SCM g_sound_samples(SCM filename) 
{
  #define H_mus_sound_samples "(" S_mus_sound_samples " filename) -> samples (frames*channels) in sound"
  char *tmpstr = NULL;
  int res;
  ERRS1(filename,S_mus_sound_samples); 
  res = mus_sound_samples(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  RTNINT(res);
}

static SCM g_sound_frames(SCM filename) 
{
  #define H_mus_sound_frames "(" S_mus_sound_frames " filename) -> frames (samples/channel) in sound"
  char *tmpstr = NULL;
  int res;
  ERRS1(filename,S_mus_sound_frames); 
  res = mus_sound_frames(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  RTNINT(res);
}

static SCM g_sound_datum_size(SCM filename) 
{
  #define H_mus_sound_datum_size "(" S_mus_sound_datum_size " filename) -> bytes per sample of data in sound"
  char *tmpstr = NULL;
  int res;
  ERRS1(filename,S_mus_sound_datum_size); 
  res = mus_sound_datum_size(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  RTNINT(res);
}

static SCM g_sound_data_location(SCM filename) 
{
  #define H_mus_sound_data_location "(" S_mus_sound_data_location " filename) -> location (bytes) of first sample of sound data"
  char *tmpstr = NULL;
  int res;
  ERRS1(filename,S_mus_sound_data_location); 
  res = mus_sound_data_location(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  RTNINT(res);
}

static SCM g_sound_chans(SCM filename) 
{
  #define H_mus_sound_chans "(" S_mus_sound_chans " filename) -> channels of sound"
  char *tmpstr = NULL;
  int res;
  ERRS1(filename,S_mus_sound_chans); 
  res = mus_sound_chans(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  RTNINT(res);
}

static SCM g_sound_srate(SCM filename) 
{
  #define H_mus_sound_srate "(" S_mus_sound_srate " filename) -> sampling rate of sound"
  char *tmpstr = NULL;
  int res;
  ERRS1(filename,S_mus_sound_srate); 
  res = mus_sound_srate(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  RTNINT(res);
}

static SCM g_sound_header_type(SCM filename) 
{
  #define H_mus_sound_header_type "(" S_mus_sound_header_type " filename) -> header type (e.g. AIFF) of sound"
  char *tmpstr = NULL;
  int res;
  ERRS1(filename,S_mus_sound_header_type); 
  res = mus_sound_header_type(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  RTNINT(res);
}

static SCM g_sound_data_format(SCM filename) 
{
  #define H_mus_sound_data_format "(" S_mus_sound_data_format " filename) -> data format (e.g. big endian short) of sound"
  char *tmpstr = NULL;
  int res;
  ERRS1(filename,S_mus_sound_data_format); 
  res = mus_sound_data_format(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  RTNINT(res);
}

static SCM g_sound_length(SCM filename) 
{
  #define H_mus_sound_length "(" S_mus_sound_length " filename) -> file length in bytes"
  char *tmpstr = NULL;
  int res;
  ERRS1(filename,S_mus_sound_length); 
  res = mus_sound_length(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  RTNINT(res);
}

static SCM g_sound_type_specifier(SCM filename) 
{
  #define H_mus_sound_type_specifier "(" S_mus_sound_type_specifier " filename) -> original file type identifier (e.g. 0x2e736e64)"
  char *tmpstr = NULL;
  int res;
  ERRS1(filename,S_mus_sound_type_specifier); 
  res = mus_sound_type_specifier(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  RTNINT(res);
}

static SCM g_sound_comment(SCM filename) 
{
  #define H_mus_sound_comment "(" S_mus_sound_comment " filename) -> comment (string) found in sound's header"
  char *tmpstr = NULL,*res; 
  ERRS1(filename,S_mus_sound_comment); 
  res = mus_sound_comment(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  RTNSTR(res);
}

static SCM g_sound_type_name(SCM type) 
{
  #define H_mus_header_type_name "(" S_mus_header_type_name " type) -> header type (e.g. mus-aiff) as a string"
  ERRN1(type,S_mus_header_type_name); 
  RTNSTR(mus_header_type_name(g_scm2int(type)));
}

static SCM g_sound_format_name(SCM format) 
{
  #define H_mus_data_format_name "(" S_mus_data_format_name " format) -> data format (e.g. mus-bshort) as a string"
  ERRN1(format,S_mus_data_format_name); 
  RTNSTR(mus_data_format_name(g_scm2int(format)));
}

static SCM g_sound_bytes_per_sample(SCM format) 
{
  #define H_mus_data_format_bytes_per_sample "(" S_mus_data_format_bytes_per_sample " format) -> number of bytes per sample in format (e.g. mus-short = 2)"
  ERRN1(format,S_mus_data_format_bytes_per_sample); 
  RTNINT(mus_data_format_to_bytes_per_sample(g_scm2int(format)));
}

static SCM g_audio_error(void) 
{
  #define H_mus_audio_error "(" S_mus_audio_error ") -> audio error indication, if any (treated like C's errno)"
  RTNINT(mus_audio_error());
}

static SCM g_audio_error_name(SCM err) 
{
  #define H_mus_audio_error_name "(" S_mus_audio_error_name " err) -> audio error indication as a string"
  ERRN1(err,S_mus_audio_error_name); 
  RTNSTR(mus_audio_error_name(g_scm2int(err)));
}

static SCM g_report_audio_state(void) 
{
  #define H_mus_audio_report "(" S_mus_audio_report ") -> string describing current audio hardware setup"
  RTNSTR(mus_audio_report());
}

static SCM g_sound_duration(SCM filename) 
{
  #define H_mus_sound_duration "(" S_mus_sound_duration " filename) -> duration (seconds) of sound"
  char *tmpstr = NULL;
  float res;
  ERRS1(filename,S_mus_sound_duration); 
  res = mus_sound_duration(tmpstr = full_filename(filename));
  if (tmpstr) FREE(tmpstr);
  return(gh_double2scm(res));

}

static SCM g_audio_outputs(SCM speakers, SCM headphones, SCM line_out)
{
  #define H_mus_audio_sun_outputs "(" S_mus_audio_sun_outputs " speaker headphones line-out) sets the current Sun audio outputs"
#ifdef SUN
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(speakers)),speakers,SCM_ARG1,S_mus_audio_sun_outputs);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(headphones)),headphones,SCM_ARG2,S_mus_audio_sun_outputs);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(line_out)),line_out,SCM_ARG3,S_mus_audio_sun_outputs);
  mus_audio_sun_outputs(g_scm2int(speakers),g_scm2int(headphones),g_scm2int(line_out));
#endif
  return(SCM_BOOL_F);
}

static SCM g_sound_max_amp(SCM file)
{
  #define H_mus_sound_max_amp "(" S_mus_sound_max_amp " filename) -> max amps in sound (a vector of amps and locations)"
  int chans,rtn,i;
  MUS_SAMPLE_TYPE *vals;
  char *filename;
  SCM vect = SCM_BOOL_F;
  ERRS1(file,S_mus_sound_max_amp);
  filename = full_filename(file);
  chans = mus_sound_chans(filename);
  if (chans > 0)
    {
      vals = (MUS_SAMPLE_TYPE *)CALLOC(chans*2,sizeof(MUS_SAMPLE_TYPE));
      rtn = mus_sound_max_amp(filename,vals);
      if (rtn > 0)
	{
	  vect = gh_make_vector(gh_int2scm(chans*2),gh_int2scm(0));
	  for (i=0;i<chans*2;i+=2)
	    {
	      gh_vector_set_x(vect,gh_int2scm(i),gh_int2scm((int)(vals[i])));
	      gh_vector_set_x(vect,gh_int2scm(i+1),gh_double2scm(MUS_SAMPLE_TO_FLOAT(vals[i+1])));
	    }
	}
    }
  if (filename) FREE(filename);
  return(vect);
}

/* to support the actual sound file/audio port stuff, we need an "smob" for the int** arrays */

static int sound_data_tag = 0;

static SCM mark_sound_data(SCM obj)
{
  SCM_SETGC8MARK(obj);
  return(SCM_BOOL_F);
}

int sound_data_p(SCM obj)
{
  return((SCM_NIMP(obj)) && (SCM_TYP16(obj) == (SCM)sound_data_tag));
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
      sd = (sound_data *)(GH_VALUE_OF(arg));
      return(sd->data);
    }
  return(NULL);
}

static scm_sizet free_sound_data(SCM obj)
{
  int i;
  sound_data *v = (sound_data *)GH_VALUE_OF(obj);
  if (v == NULL) return(0);
  if (v->data) 
    {
      for (i=0;i<v->chans;i++) if (v->data[i]) FREE(v->data[i]);
      FREE(v->data);
    }
  v->data = NULL;
  FREE(v);
  return(0);
}

static int print_sound_data(SCM obj, SCM port, scm_print_state *pstate)
{
  char *buf;
  sound_data *v = (sound_data *)GH_VALUE_OF(obj);
  if (v == NULL)
    scm_puts("<null>",port);
  else
    {
      buf = (char *)CALLOC(64,sizeof(char));
      sprintf(buf,"#<sound_data: %d chans, %d frames>",v->chans,v->length);
      scm_puts(buf,port);
      FREE(buf);
    }
  scm_remember(&obj);
  return(1);
}

static SCM equalp_sound_data(SCM obj1, SCM obj2)
{
  sound_data *v1,*v2;
  v1 = (sound_data *)GH_VALUE_OF(obj1);
  v2 = (sound_data *)GH_VALUE_OF(obj2);
  if (v1 == v2) return(SCM_BOOL_T);
  return(SCM_BOOL_F);
}

static SCM sound_data_length(SCM obj)
{
  #define H_sound_data_length "(" S_sound_data_length " sd) -> length (samples) of each channel of sound-data object sd"
  sound_data *v = (sound_data *)GH_VALUE_OF(obj);
  if (v == NULL) return(0);
  return(gh_int2scm(v->length));
}

static SCM sound_data_chans(SCM obj)
{
  #define H_sound_data_chans "(" S_sound_data_chans " sd) -> number of channels in sound-data object sd"
  sound_data *v = (sound_data *)GH_VALUE_OF(obj);
  if (v == NULL) return(0);
  return(gh_int2scm(v->chans));
}

SCM make_sound_data(int chans, int frames)
{
  #define H_make_sound_data "(" S_make_sound_data " chans frames) -> new sound-data object with chans channels, each having frames samples"
#if HAVE_GUILE_1_3_0
  SCM ans;
#endif
  int i;
  sound_data *new_sound_data;
  new_sound_data = (sound_data *)CALLOC(1,sizeof(sound_data));
  new_sound_data->length = frames;
  new_sound_data->chans = chans;
  new_sound_data->data = (MUS_SAMPLE_TYPE **)CALLOC(chans,sizeof(MUS_SAMPLE_TYPE *));
  for (i=0;i<chans;i++) new_sound_data->data[i] = (MUS_SAMPLE_TYPE *)CALLOC(frames,sizeof(MUS_SAMPLE_TYPE));
#if (!HAVE_GUILE_1_3_0)
  SCM_RETURN_NEWSMOB(sound_data_tag,new_sound_data);
#else
  SCM_NEWCELL(ans);
  SCM_SETCDR(ans,(SCM)new_sound_data);
  SCM_SETCAR(ans,sound_data_tag);
  return(ans);
#endif
}

#if HAVE_GUILE_1_3_0
static scm_smobfuns sound_data_smobfuns = {
  &mark_sound_data,
  &free_sound_data,
  &print_sound_data,
  &equalp_sound_data};
#endif

static SCM g_make_sound_data(SCM chans, SCM frames)
{
  ERRN1(chans,S_make_sound_data);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(frames)),frames,SCM_ARG1,S_make_sound_data);
  return(make_sound_data(g_scm2int(chans),g_scm2int(frames)));
}

static SCM sound_data_ref(SCM obj, SCM chan, SCM frame)
{
  #define H_sound_data_ref "(" S_sound_data_ref " sd chan i) -> sample in channel chan at location i of sound-data object sd: sd[chan][i]"
  sound_data *v = (sound_data *)GH_VALUE_OF(obj);
  int loc,chn;
  SCM_ASSERT(sound_data_p(obj),obj,SCM_ARG1,S_sound_data_ref);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chan)),chan,SCM_ARG2,S_sound_data_ref);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(frame)),frame,SCM_ARG3,S_sound_data_ref);
  if (v)
    {
      chn = g_scm2int(chan);
      if ((chn >= 0) && (chn < v->chans))
	{
	  loc = g_scm2int(frame);
	  if ((loc >= 0) && (loc < v->length))
	    return(gh_double2scm(MUS_SAMPLE_TO_DOUBLE(v->data[chn][loc])));
	  else scm_misc_error(S_sound_data_ref,"invalid frame: ~S: ~S",SCM_LIST2(obj,frame));
	}
      else scm_misc_error(S_sound_data_ref,"invalid channel: ~S[~S]",SCM_LIST2(obj,chan));
    }
  else scm_misc_error(S_sound_data_ref,"nil sound-data?",SCM_EOL);
  return(gh_double2scm(0.0));
}

static SCM sound_data_set(SCM obj, SCM chan, SCM frame, SCM val)
{
  #define H_sound_data_setB "(" S_sound_data_setB " sd chan i val): set sound-data object sd's i-th element in channel chan to val: sd[chan][i] = val"
  sound_data *v = (sound_data *)GH_VALUE_OF(obj);
  int loc,chn;
  if (v)
    {
      chn = g_scm2int(chan);
      if ((chn >= 0) && (chn < v->chans))
	{
	  loc = g_scm2int(frame);
	  if ((loc >= 0) && (loc < v->length))
	    v->data[chn][loc] = MUS_DOUBLE_TO_SAMPLE(gh_scm2double(val));
	  else scm_misc_error(S_sound_data_setB,"invalid frame: ~S[~S]: ~S",SCM_LIST3(obj,chan,frame));
	}
      else scm_misc_error(S_sound_data_setB,"invalid channel: ~S[~S]: ~S",SCM_LIST3(obj,chan,frame));
    }
  else scm_misc_error(S_sound_data_setB,"nil sound-data?",SCM_EOL);
  return(val);
}

static SCM sound_data2vct(SCM sdobj, SCM chan, SCM vobj)
{
  #define H_sound_data2vct "(" S_sound_data2vct " sd chan v) places sound-data object sd's channel chan data into vct object v"
  vct *v;
  sound_data *sd;
  int len,i,chn;
  SCM_ASSERT(sound_data_p(sdobj),sdobj,SCM_ARG1,S_sound_data2vct);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chan)),chan,SCM_ARG2,S_sound_data2vct);
  SCM_ASSERT(vct_p(vobj),vobj,SCM_ARG3,S_sound_data2vct);
  v = (vct *)get_vct(vobj);
  sd = (sound_data *)GH_VALUE_OF(sdobj);
  chn = g_scm2int(chan);
  if (chn < sd->chans)
    {
      if (sd->length < v->length) len = sd->length; else len = v->length;
      for (i=0;i<len;i++) v->data[i] = (Float)(MUS_SAMPLE_TO_FLOAT(sd->data[chn][i]));
    }
  else scm_misc_error(S_sound_data2vct,"invalid channel: ~S[~S]: ~S",SCM_LIST3(sdobj,chan,vobj));
  return(vobj);
}

static SCM vct2sound_data(SCM vobj, SCM sdobj, SCM chan)
{
  #define H_vct2sound_data "(" S_vct2sound_data " v sd chan) places vct v's data into sound-data sd's channel chan"
  vct *v;
  sound_data *sd;
  int len,i,chn;
  SCM_ASSERT(vct_p(vobj),vobj,SCM_ARG1,S_vct2sound_data);
  SCM_ASSERT(sound_data_p(sdobj),sdobj,SCM_ARG2,S_vct2sound_data);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chan)),chan,SCM_ARG3,S_vct2sound_data);
  v = (vct *)get_vct(vobj);
  sd = (sound_data *)GH_VALUE_OF(sdobj);
  chn = g_scm2int(chan);
  if (chn < sd->chans)
    {
      if (sd->length < v->length) len = sd->length; else len = v->length;
      for (i=0;i<len;i++) sd->data[chn][i] = MUS_FLOAT_TO_SAMPLE(v->data[i]);
    }
  else scm_misc_error(S_vct2sound_data,"invalid channel: ~S[~S]: ~S",SCM_LIST3(vobj,chan,sdobj));
  return(vobj);
}


static SCM g_open_sound_input(SCM file)
{
  #define H_mus_sound_open_input "(" S_mus_sound_open_input " filename) -> fd (int), opens filename for sound input"
  int fd;
  char *tmpstr = NULL;
  ERRS1(file,S_mus_sound_open_input);
  fd = mus_sound_open_input(tmpstr = full_filename(file));
  if (tmpstr) FREE(tmpstr);
  return(gh_int2scm(fd));
}

static SCM g_open_sound_output(SCM file, SCM srate, SCM chans, SCM data_format, SCM header_type, SCM comment)
{

  #define H_mus_sound_open_output "(" S_mus_sound_open_output " filename srate chans data-format header-type &optional comment)\n\
   opens filename for sound output with the given srate and so on,\n\
   returns the file number (int).\n\
   the file size is normally set later via mus-sound-close-output.\n\
   srate is an integer, comment is a string,\n\
   data-format is a sndlib format indicator such as mus-bshort\n\
   header-type is a sndlib type indicator such as mus-aiff,\n\
   sndlib currently only writes 5 or so header types."

  int fd;
  char *com = NULL;
  char *tmpstr = NULL;
  SCM_ASSERT(gh_string_p(file),file,SCM_ARG1,S_mus_sound_open_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(srate)),srate,SCM_ARG2,S_mus_sound_open_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chans)),chans,SCM_ARG3,S_mus_sound_open_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(data_format)),data_format,SCM_ARG4,S_mus_sound_open_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(header_type)),header_type,SCM_ARG5,S_mus_sound_open_output);
  SCM_ASSERT((gh_string_p(comment) || (SCM_UNBNDP(comment))),comment,SCM_ARG6,S_mus_sound_open_output);
  if (gh_string_p(comment)) com = gh_scm2newstr(comment,NULL);
  fd = mus_sound_open_output(tmpstr = full_filename(file),g_scm2int(srate),g_scm2int(chans),g_scm2int(data_format),g_scm2int(header_type),com);
  if (tmpstr) FREE(tmpstr);
  if (com) free(com);
  return(gh_int2scm(fd));
}

static SCM g_reopen_sound_output(SCM file, SCM chans, SCM data_format, SCM header_type, SCM data_loc)
{

  #define H_mus_sound_reopen_output "(" S_mus_sound_reopen_output " filename chans data-format header-type data-location)\n\
   reopen (without alteration) filename for output\n\
   data-format and header-type are sndlib indicators such as mus-bshort or mus-aiff\n\
   data-location should be retrieved from a previous call to mus-sound-data-location"

  int fd;
  char *tmpstr = NULL;
  SCM_ASSERT(gh_string_p(file),file,SCM_ARG1,S_mus_sound_reopen_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chans)),chans,SCM_ARG2,S_mus_sound_reopen_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(data_format)),data_format,SCM_ARG3,S_mus_sound_reopen_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(header_type)),header_type,SCM_ARG4,S_mus_sound_reopen_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(data_loc)),data_loc,SCM_ARG5,S_mus_sound_reopen_output);
  fd = mus_sound_reopen_output(tmpstr = full_filename(file),g_scm2int(chans),g_scm2int(data_format),g_scm2int(header_type),g_scm2int(data_loc));
  if (tmpstr) FREE(tmpstr);
  return(gh_int2scm(fd));
}

static SCM g_close_sound_input(SCM fd)
{
  #define H_mus_sound_close_input "(" S_mus_sound_close_input " fd) closes file number fd"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(fd)),fd,SCM_ARG1,S_mus_sound_close_input);
  return(gh_int2scm(mus_sound_close_input(g_scm2int(fd))));
}

static SCM g_close_sound_output(SCM fd, SCM bytes)
{
  #define H_mus_sound_close_output "(" S_mus_sound_close_output " fd bytes) closes file number fd\n\
   after updating its header (if any) to reflect bytes, the new file data size"

  SCM_ASSERT(SCM_NFALSEP(scm_real_p(fd)),fd,SCM_ARG1,S_mus_sound_close_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(bytes)),bytes,SCM_ARG2,S_mus_sound_close_output);
  return(gh_int2scm(mus_sound_close_output(g_scm2int(fd),g_scm2int(bytes))));
}

static SCM g_read_sound(SCM fd, SCM beg, SCM end, SCM chans, SCM sv)
{
  #define H_mus_sound_read "(" S_mus_sound_read " fd beg end chans sdata) reads sound data from file number fd,\n\
   filling the sound-data object sdata's buffers starting at (buffer location) beg, going to end"

  SCM_ASSERT(SCM_NFALSEP(scm_real_p(fd)),fd,SCM_ARG1,S_mus_sound_read);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(beg)),beg,SCM_ARG2,S_mus_sound_read);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(end)),end,SCM_ARG3,S_mus_sound_read);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chans)),chans,SCM_ARG4,S_mus_sound_read);
  SCM_ASSERT(sound_data_p(sv),sv,SCM_ARG5,S_mus_sound_read);
  return(gh_int2scm(mus_sound_read(g_scm2int(fd),g_scm2int(beg),g_scm2int(end),g_scm2int(chans),get_sound_data(sv))));
}

static SCM g_write_sound(SCM fd, SCM beg, SCM end, SCM chans, SCM sv)
{
  #define H_mus_sound_write "(" S_mus_sound_write " fd beg end chans sdata) writes sound-data object sdata's data\n\
   starting at (buffer location) beg, going to end, writing to file number fd"

  SCM_ASSERT(SCM_NFALSEP(scm_real_p(fd)),fd,SCM_ARG1,S_mus_sound_write);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(beg)),beg,SCM_ARG2,S_mus_sound_write);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(end)),end,SCM_ARG3,S_mus_sound_write);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chans)),chans,SCM_ARG4,S_mus_sound_write);
  SCM_ASSERT(sound_data_p(sv),sv,SCM_ARG5,S_mus_sound_write);
  return(gh_int2scm(mus_sound_write(g_scm2int(fd),g_scm2int(beg),g_scm2int(end),g_scm2int(chans),get_sound_data(sv))));
}

static SCM g_seek_sound(SCM fd, SCM offset, SCM origin)
{
  #define H_mus_sound_seek "(" S_mus_sound_seek " fd offset origin) moves the current read/write location in file number fd\n\
   to the short-wise sample offset given origin (both treated as in lseek)"

  SCM_ASSERT(SCM_NFALSEP(scm_real_p(fd)),fd,SCM_ARG1,S_mus_sound_seek);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(offset)),offset,SCM_ARG2,S_mus_sound_seek);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(origin)),origin,SCM_ARG3,S_mus_sound_seek);
  return(gh_int2scm(mus_sound_seek(g_scm2int(fd),g_scm2int(offset),g_scm2int(origin))));
}

static SCM g_seek_sound_frame(SCM fd, SCM offset)
{
  #define H_mus_sound_seek_frame "(" S_mus_sound_seek_frame " fd frame) moves the current read/write location in file number fd\n\
   to the indicated frame"

  SCM_ASSERT(SCM_NFALSEP(scm_real_p(fd)),fd,SCM_ARG1,S_mus_sound_seek_frame);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(offset)),offset,SCM_ARG2,S_mus_sound_seek_frame);
  return(gh_int2scm(mus_sound_seek_frame(g_scm2int(fd),g_scm2int(offset))));
}

static SCM g_open_audio_output(SCM dev, SCM srate, SCM chans, SCM format, SCM size)
{
  #define H_mus_audio_open_output "(" S_mus_audio_open_output " device srate chans format clipped)\n\
   opens the audio device ready for output at the given srate and so on.\n\
   returns the audio line number:\n\
     (set! line (mus-audio-open-output mus-audio-default 22050 1 mus-lshort 256)"

  SCM_ASSERT(SCM_NFALSEP(scm_real_p(dev)),dev,SCM_ARG1,S_mus_audio_open_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(srate)),srate,SCM_ARG2,S_mus_audio_open_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chans)),chans,SCM_ARG3,S_mus_audio_open_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(format)),format,SCM_ARG4,S_mus_audio_open_output);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(size)),size,SCM_ARG5,S_mus_audio_open_output);
  return(gh_int2scm(mus_audio_open_output(g_scm2int(dev),g_scm2int(srate),g_scm2int(chans),g_scm2int(format),g_scm2int(size))));
}

static SCM g_open_audio_input(SCM dev, SCM srate, SCM chans, SCM format, SCM size)
{
  #define H_mus_audio_open_input "(" S_mus_audio_open_input " (device srate chans format bufsize)\n\
   opens the audio device ready for input with the indicated attributes\n\
   returns the audio line number"

  SCM_ASSERT(SCM_NFALSEP(scm_real_p(dev)),dev,SCM_ARG1,S_mus_audio_open_input);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(srate)),srate,SCM_ARG2,S_mus_audio_open_input);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chans)),chans,SCM_ARG3,S_mus_audio_open_input);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(format)),format,SCM_ARG4,S_mus_audio_open_input);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(size)),size,SCM_ARG5,S_mus_audio_open_input);
  return(gh_int2scm(mus_audio_open_input(g_scm2int(dev),g_scm2int(srate),g_scm2int(chans),g_scm2int(format),g_scm2int(size))));
}

static SCM g_close_audio(SCM line)
{
  #define H_mus_audio_close "(" S_mus_audio_close " line) closes the audio hardware port line"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(line)),line,SCM_ARG1,S_mus_audio_close);
  return(gh_int2scm(mus_audio_close(g_scm2int(line))));
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
  return(gh_int2scm(mus_audio_systems()));
}


/* these take a sndlib buffer (sound_data) and handle the conversion to the interleaved char* internally */
/* so, they take "frames", not "bytes", and a sound_data object, not char* etc */

static SCM g_write_audio(SCM line, SCM sdata, SCM frames)
{
  #define H_mus_audio_write "(" S_mus_audio_write " line sdata frames) writes frames of data (channels*frames = samples)\n\
   to the audio line from the sound-data object sdata."

  /* assuming 16-bit output here for now */
  short *obuf;
  MUS_SAMPLE_TYPE **bufs;
  sound_data *sd;
  int i,j,k,outbytes,val,frms;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(line)),line,SCM_ARG1,S_mus_audio_write);
  SCM_ASSERT(sound_data_p(sdata),sdata,SCM_ARG2,S_mus_audio_write);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(frames)),frames,SCM_ARG3,S_mus_audio_write);
  sd = (sound_data *)GH_VALUE_OF(sdata);
  bufs = sd->data;
  frms = g_scm2int(frames);
  outbytes = frms * sd->chans * 2;
  obuf = (short *)CALLOC(frms * sd->chans,sizeof(short));
  if (sd->chans == 1)
    {
      for (k=0;k<frms;k++) obuf[k] = MUS_SAMPLE_TO_SHORT(bufs[0][k]);
    }
  else
    {
      for (k=0,j=0;k<frms;k++,j+=sd->chans)
	{
	  for (i=0;i<sd->chans;i++) obuf[j+i] = MUS_SAMPLE_TO_SHORT(bufs[i][k]);
	}
    }
  val = mus_audio_write(g_scm2int(line),(char *)obuf,outbytes);
  FREE(obuf);
  return(scm_return_first(gh_int2scm(val),sdata));
}

static SCM g_read_audio(SCM line, SCM sdata, SCM frames)
{
  #define H_mus_audio_read "(" S_mus_audio_read " line sdata frames) reads frames of data (channels*frames = samples)\n\
   from the audio line into the sound-data object sdata."

  MUS_SAMPLE_TYPE *inbuf;
  sound_data *sd;
  int val,inbytes,i,j,k,frms;
  MUS_SAMPLE_TYPE **bufs;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(line)),line,SCM_ARG1,S_mus_audio_read);
  SCM_ASSERT(sound_data_p(sdata),sdata,SCM_ARG2,S_mus_audio_read);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(frames)),frames,SCM_ARG3,S_mus_audio_read);
  sd = (sound_data *)GH_VALUE_OF(sdata);
  bufs = sd->data;
  frms = g_scm2int(frames);
  inbytes = frms * sd->chans * 2;
  inbuf = (MUS_SAMPLE_TYPE *)CALLOC(frms * sd->chans,sizeof(MUS_SAMPLE_TYPE));
  val = mus_audio_read(g_scm2int(line),(char *)inbuf,inbytes);
  if (sd->chans == 1)
    {
      for (k=0;k<frms;k++) bufs[0][k] = inbuf[k];
    }
  else
    {
      for (k=0,j=0;k<frms;k++,j+=sd->chans)
	{
	  for (i=0;i<sd->chans;i++) bufs[i][k] = inbuf[j+i];
	}
    }
  FREE(inbuf);
  return(scm_return_first(gh_int2scm(val),sdata));
}

static SCM g_read_audio_state(SCM dev, SCM field, SCM chan, SCM vals)
{
  #define H_mus_audio_mixer_read "(" S_mus_audio_mixer_read " device field channel vals) reads sound card 'mixer' state"
  int val,i,len;
  float *fvals;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(dev)),dev,SCM_ARG1,S_mus_audio_mixer_read);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(field)),field,SCM_ARG2,S_mus_audio_mixer_read);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chan)),chan,SCM_ARG3,S_mus_audio_mixer_read);
  SCM_ASSERT(gh_vector_p(vals),vals,SCM_ARG4,S_mus_audio_mixer_read);
  len = gh_vector_length(vals);
  if (len == 0)
    fvals = (float *)CALLOC(1,sizeof(float));
  else fvals = (float *)CALLOC(len,sizeof(float));
  val = mus_audio_mixer_read(g_scm2int(dev),g_scm2int(field),g_scm2int(chan),fvals);
  for (i=0;i<len;i++) gh_vector_set_x(vals,gh_int2scm(i),gh_double2scm(fvals[i]));
  FREE(fvals);
  return(gh_int2scm(val));
}

static SCM g_write_audio_state(SCM dev, SCM field, SCM chan, SCM vals)
{
  #define H_mus_audio_mixer_write "(" S_mus_audio_mixer_write " device field channel vals) changes the sound card's 'mixer' state"
  int i,len,res;
  float *fvals;
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(dev)),dev,SCM_ARG1,S_mus_audio_mixer_write);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(field)),field,SCM_ARG2,S_mus_audio_mixer_write);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(chan)),chan,SCM_ARG3,S_mus_audio_mixer_write);
  SCM_ASSERT(gh_vector_p(vals),vals,SCM_ARG4,S_mus_audio_mixer_write);
  len = gh_vector_length(vals);
  if (len == 0)
    fvals = (float *)CALLOC(1,sizeof(float));
  else
    {
      fvals = (float *)CALLOC(len,sizeof(float));
      for (i=0;i<len;i++) fvals[i] = gh_scm2double(gh_vector_ref(vals,gh_int2scm(i)));
    }
  res = mus_audio_mixer_write(g_scm2int(dev),g_scm2int(field),g_scm2int(chan),fvals);
  FREE(fvals);
  return(gh_int2scm(res));
}

static SCM g_mus_set_data_clipped(SCM fd, SCM clipped)
{
  #define H_mus_file_set_data_clipped "(" S_mus_file_set_data_clipped " fd val) sets whether data associated with file fd is clipped or wraps around"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(fd)),fd,SCM_ARG1,S_mus_file_set_data_clipped);
  SCM_ASSERT(gh_boolean_p(clipped),fd,SCM_ARG1,S_mus_file_set_data_clipped);
  return(gh_int2scm(mus_file_set_data_clipped(g_scm2int(fd),(SCM_FALSEP(clipped)) ? 0 : 1)));
}

static SCM g_mus_prescaler(SCM fd)
{
  #define H_mus_file_prescaler "(" S_mus_file_prescaler " fd) -> current prescaler (normally 1.0) associated with fd"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(fd)),fd,SCM_ARG1,S_mus_file_set_prescaler);
  return(gh_double2scm(mus_file_prescaler(g_scm2int(fd))));
}

static SCM g_mus_set_prescaler(SCM fd, SCM val)
{
  #define H_mus_file_set_prescaler "(" S_mus_file_set_prescaler " fd val) sets the current prescaler associated with fd"
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(fd)),fd,SCM_ARG1,S_mus_file_set_prescaler);
  SCM_ASSERT(SCM_NFALSEP(scm_real_p(val)),val,SCM_ARG2,S_mus_file_set_prescaler);
  return(gh_double2scm(mus_file_set_prescaler(g_scm2int(fd),gh_scm2double(val))));
}

#if DEBUGGING
static SCM g_sound_print_cache(void)
{
  mus_sound_print_cache();
  return(SCM_BOOL_F);
}
#endif

#ifdef __cplusplus
  static SCM gh_new_procedure5_1 (char *proc_name,SCM (*fn)(SCM,SCM,SCM,SCM,SCM,SCM)) {return(gh_new_procedure(proc_name,(SCM (*)(...))fn,5,1,0));}
#else
  static SCM gh_new_procedure5_1 (char *proc_name,SCM (*fn)()) {return(gh_new_procedure(proc_name,fn,5,1,0));}
#endif

void mus_sndlib2scm_initialize(void)
{
  SCM local_doc;
  mus_sound_initialize();
  local_doc = scm_permanent_object(scm_string_to_symbol(gh_str02scm("documentation")));

#if (!HAVE_GUILE_1_3_0)
  /* sound_data_tag = scm_make_smob_type_mfpe("sound-data",sizeof(sound_data),mark_sound_data,free_sound_data,print_sound_data,equalp_sound_data); */
  sound_data_tag = scm_make_smob_type("sound-data",sizeof(sound_data));
  scm_set_smob_mark(sound_data_tag,mark_sound_data);
  scm_set_smob_print(sound_data_tag,print_sound_data);
  scm_set_smob_free(sound_data_tag,free_sound_data);
  scm_set_smob_equalp(sound_data_tag,equalp_sound_data);
#else
  sound_data_tag = scm_newsmob(&sound_data_smobfuns);
#endif

  gh_define(S_mus_next,gh_int2scm(MUS_NEXT));
  gh_define(S_mus_aifc,gh_int2scm(MUS_AIFC));
  gh_define(S_mus_riff,gh_int2scm(MUS_RIFF));
  gh_define(S_mus_nist,gh_int2scm(MUS_NIST));
  gh_define(S_mus_raw,gh_int2scm(MUS_RAW));
  gh_define(S_mus_ircam,gh_int2scm(MUS_IRCAM));
  gh_define(S_mus_aiff,gh_int2scm(MUS_AIFF));

  gh_define(S_mus_bshort,gh_int2scm(MUS_BSHORT));
  gh_define(S_mus_lshort,gh_int2scm(MUS_LSHORT));
  gh_define(S_mus_mulaw,gh_int2scm(MUS_MULAW));
  gh_define(S_mus_alaw,gh_int2scm(MUS_ALAW));
  gh_define(S_mus_byte,gh_int2scm(MUS_BYTE));
  gh_define(S_mus_ubyte,gh_int2scm(MUS_UBYTE));
  gh_define(S_mus_bfloat,gh_int2scm(MUS_BFLOAT));
  gh_define(S_mus_lfloat,gh_int2scm(MUS_LFLOAT));
  gh_define(S_mus_bint,gh_int2scm(MUS_BINT));
  gh_define(S_mus_lint,gh_int2scm(MUS_LINT));
  gh_define(S_mus_bintn,gh_int2scm(MUS_BINTN));
  gh_define(S_mus_lintn,gh_int2scm(MUS_LINTN));
  gh_define(S_mus_b24int,gh_int2scm(MUS_B24INT));
  gh_define(S_mus_l24int,gh_int2scm(MUS_L24INT));
  gh_define(S_mus_bdouble,gh_int2scm(MUS_BDOUBLE));
  gh_define(S_mus_ldouble,gh_int2scm(MUS_LDOUBLE));
  gh_define(S_mus_ubshort,gh_int2scm(MUS_UBSHORT));
  gh_define(S_mus_ulshort,gh_int2scm(MUS_ULSHORT));

  gh_define(S_mus_audio_default,gh_int2scm(MUS_AUDIO_DEFAULT));
  gh_define(S_mus_audio_duplex_default,gh_int2scm(MUS_AUDIO_DUPLEX_DEFAULT));
  gh_define(S_mus_audio_line_out,gh_int2scm(MUS_AUDIO_LINE_OUT));
  gh_define(S_mus_audio_line_in,gh_int2scm(MUS_AUDIO_LINE_IN));
  gh_define(S_mus_audio_microphone,gh_int2scm(MUS_AUDIO_MICROPHONE));
  gh_define(S_mus_audio_speakers,gh_int2scm(MUS_AUDIO_SPEAKERS));
  gh_define(S_mus_audio_dac_out,gh_int2scm(MUS_AUDIO_DAC_OUT));
  gh_define(S_mus_audio_adat_in,gh_int2scm(MUS_AUDIO_ADAT_IN));
  gh_define(S_mus_audio_aes_in,gh_int2scm(MUS_AUDIO_AES_IN));
  gh_define(S_mus_audio_digital_in,gh_int2scm(MUS_AUDIO_DIGITAL_IN));
  gh_define(S_mus_audio_digital_out,gh_int2scm(MUS_AUDIO_DIGITAL_OUT));
  gh_define(S_mus_audio_adat_out,gh_int2scm(MUS_AUDIO_ADAT_OUT));
  gh_define(S_mus_audio_aes_out,gh_int2scm(MUS_AUDIO_AES_OUT));
  gh_define(S_mus_audio_dac_filter,gh_int2scm(MUS_AUDIO_DAC_FILTER));
  gh_define(S_mus_audio_mixer,gh_int2scm(MUS_AUDIO_MIXER));
  gh_define(S_mus_audio_line1,gh_int2scm(MUS_AUDIO_LINE1));
  gh_define(S_mus_audio_line2,gh_int2scm(MUS_AUDIO_LINE2));
  gh_define(S_mus_audio_line3,gh_int2scm(MUS_AUDIO_LINE3));
  gh_define(S_mus_audio_aux_input,gh_int2scm(MUS_AUDIO_AUX_INPUT));
  gh_define(S_mus_audio_cd,gh_int2scm(MUS_AUDIO_CD));
  gh_define(S_mus_audio_aux_output,gh_int2scm(MUS_AUDIO_AUX_OUTPUT));
  gh_define(S_mus_audio_spdif_in,gh_int2scm(MUS_AUDIO_SPDIF_IN));
  gh_define(S_mus_audio_spdif_out,gh_int2scm(MUS_AUDIO_SPDIF_OUT));
  gh_define(S_mus_audio_direction,gh_int2scm(MUS_AUDIO_DIRECTION));
  gh_define(S_mus_audio_samples_per_channel,gh_int2scm(MUS_AUDIO_SAMPLES_PER_CHANNEL));

  gh_define(S_mus_audio_amp,gh_int2scm(MUS_AUDIO_AMP));
  gh_define(S_mus_audio_srate,gh_int2scm(MUS_AUDIO_SRATE));
  gh_define(S_mus_audio_channel,gh_int2scm(MUS_AUDIO_CHANNEL));
  gh_define(S_mus_audio_format,gh_int2scm(MUS_AUDIO_FORMAT));
  gh_define(S_mus_audio_port,gh_int2scm(MUS_AUDIO_PORT));
  gh_define(S_mus_audio_imix,gh_int2scm(MUS_AUDIO_IMIX));
  gh_define(S_mus_audio_igain,gh_int2scm(MUS_AUDIO_IGAIN));
  gh_define(S_mus_audio_reclev,gh_int2scm(MUS_AUDIO_RECLEV));
  gh_define(S_mus_audio_pcm,gh_int2scm(MUS_AUDIO_PCM));
  gh_define(S_mus_audio_pcm2,gh_int2scm(MUS_AUDIO_PCM2));
  gh_define(S_mus_audio_ogain,gh_int2scm(MUS_AUDIO_OGAIN));
  gh_define(S_mus_audio_line,gh_int2scm(MUS_AUDIO_LINE));
  gh_define(S_mus_audio_synth,gh_int2scm(MUS_AUDIO_SYNTH));
  gh_define(S_mus_audio_bass,gh_int2scm(MUS_AUDIO_BASS));
  gh_define(S_mus_audio_treble,gh_int2scm(MUS_AUDIO_TREBLE));

  DEFINE_PROC(gh_new_procedure1_0(S_sound_data_length,sound_data_length),H_sound_data_length);
  DEFINE_PROC(gh_new_procedure1_0(S_sound_data_chans,sound_data_chans),H_sound_data_chans);
  DEFINE_PROC(gh_new_procedure3_0(S_sound_data_ref,sound_data_ref),H_sound_data_ref);
  DEFINE_PROC(gh_new_procedure4_0(S_sound_data_setB,sound_data_set),H_sound_data_setB);
  DEFINE_PROC(gh_new_procedure2_0(S_make_sound_data,g_make_sound_data),H_make_sound_data);
  DEFINE_PROC(gh_new_procedure1_0(S_sound_data_p,g_sound_data_p),H_sound_data_p);
  DEFINE_PROC(gh_new_procedure3_0(S_sound_data2vct,sound_data2vct),H_sound_data2vct);
  DEFINE_PROC(gh_new_procedure3_0(S_vct2sound_data,vct2sound_data),H_vct2sound_data);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_samples,g_sound_samples),H_mus_sound_samples);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_frames,g_sound_frames),H_mus_sound_frames);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_duration,g_sound_duration),H_mus_sound_duration);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_datum_size,g_sound_datum_size),H_mus_sound_datum_size);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_data_location,g_sound_data_location),H_mus_sound_data_location);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_chans,g_sound_chans),H_mus_sound_chans);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_srate,g_sound_srate),H_mus_sound_srate);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_header_type,g_sound_header_type),H_mus_sound_header_type);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_data_format,g_sound_data_format),H_mus_sound_data_format);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_length,g_sound_length),H_mus_sound_length	);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_type_specifier,g_sound_type_specifier),H_mus_sound_type_specifier);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_header_type_name,g_sound_type_name),H_mus_header_type_name);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_data_format_name,g_sound_format_name),H_mus_data_format_name);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_comment,g_sound_comment),H_mus_sound_comment);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_data_format_bytes_per_sample,g_sound_bytes_per_sample),H_mus_data_format_bytes_per_sample);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_loop_info,g_sound_loop_info),H_mus_sound_loop_info);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_max_amp,g_sound_max_amp),H_mus_sound_max_amp);
  DEFINE_PROC(gh_new_procedure0_0(S_mus_audio_error,g_audio_error),H_mus_audio_error);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_audio_error_name,g_audio_error_name),H_mus_audio_error_name);
  DEFINE_PROC(gh_new_procedure0_0(S_mus_audio_report,g_report_audio_state),H_mus_audio_report);
  DEFINE_PROC(gh_new_procedure3_0(S_mus_audio_sun_outputs,g_audio_outputs),H_mus_audio_sun_outputs);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_open_input,g_open_sound_input),H_mus_sound_open_input);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_sound_close_input,g_close_sound_input),H_mus_sound_close_input);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_audio_close,g_close_audio),H_mus_audio_close);
  DEFINE_PROC(gh_new_procedure0_0(S_mus_audio_save,g_save_audio_state),H_mus_audio_save);
  DEFINE_PROC(gh_new_procedure0_0(S_mus_audio_restore,g_restore_audio_state),H_mus_audio_restore);
  DEFINE_PROC(gh_new_procedure0_0(S_mus_audio_systems,g_audio_systems),H_mus_audio_systems);
  DEFINE_PROC(gh_new_procedure4_0(S_mus_audio_mixer_read,g_read_audio_state),H_mus_audio_mixer_read);
  DEFINE_PROC(gh_new_procedure4_0(S_mus_audio_mixer_write,g_write_audio_state),H_mus_audio_mixer_write);
  DEFINE_PROC(gh_new_procedure2_0(S_mus_file_set_data_clipped,g_mus_set_data_clipped),H_mus_file_set_data_clipped);
  DEFINE_PROC(gh_new_procedure1_0(S_mus_file_prescaler,g_mus_prescaler),H_mus_file_prescaler);
  DEFINE_PROC(gh_new_procedure2_0(S_mus_file_set_prescaler,g_mus_set_prescaler),H_mus_file_set_prescaler);
  DEFINE_PROC(gh_new_procedure3_0(S_mus_audio_write,g_write_audio),H_mus_audio_write);
  DEFINE_PROC(gh_new_procedure3_0(S_mus_audio_read,g_read_audio),H_mus_audio_read);
  DEFINE_PROC(gh_new_procedure5_1(S_mus_sound_open_output,g_open_sound_output),H_mus_sound_open_output);
  DEFINE_PROC(gh_new_procedure5_0(S_mus_sound_reopen_output,g_reopen_sound_output),H_mus_sound_reopen_output);
  DEFINE_PROC(gh_new_procedure2_0(S_mus_sound_close_output,g_close_sound_output),H_mus_sound_close_output);
  DEFINE_PROC(gh_new_procedure5_0(S_mus_sound_read,g_read_sound),H_mus_sound_read);
  DEFINE_PROC(gh_new_procedure5_0(S_mus_sound_write,g_write_sound),H_mus_sound_write);
  DEFINE_PROC(gh_new_procedure3_0(S_mus_sound_seek,g_seek_sound),H_mus_sound_seek);
  DEFINE_PROC(gh_new_procedure2_0(S_mus_sound_seek_frame,g_seek_sound_frame),H_mus_sound_seek_frame);
  DEFINE_PROC(gh_new_procedure5_0(S_mus_audio_open_output,g_open_audio_output),H_mus_audio_open_output);
  DEFINE_PROC(gh_new_procedure5_0(S_mus_audio_open_input,g_open_audio_input),H_mus_audio_open_input);

#if DEBUGGING
  gh_new_procedure0_0("mus-sound-print-cache",g_sound_print_cache);
#endif

  scm_add_feature("sndlib");
}

#endif

/*
void scm_init_sndlib_sndlib_module ()
{
  scm_register_module_xxx("sndlib sndlib",mus_sndlib2scm_initialize);
}
*/
