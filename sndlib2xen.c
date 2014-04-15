/* Tie sndlib into Xen */

#include <mus-config.h>

#if USE_SND
  #include "snd.h"
#else
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


#ifdef _MSC_VER
  #pragma warning(disable: 4244)
#endif

#include "_sndlib.h"
#include "sndlib-strings.h"
#include "vct.h"
#include "clm.h"
#include "sndlib2xen.h"
#include "clm2xen.h"

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


#if (!DISABLE_DEPRECATED)
#if (!HAVE_SCHEME)
struct sound_data {
  mus_long_t length;
  int chans;
  mus_float_t **data;
  bool wrapped;
};

int mus_sound_data_chans(sound_data *sd)
{
  return(sd->chans);
}

mus_long_t mus_sound_data_length(sound_data *sd)
{
  return(sd->length);
}

static mus_float_t **mus_sound_data_data(sound_data *sd)
{
  return(sd->data);
}

static mus_float_t *mus_sound_data_channel_data(sound_data *sd, int chan)
{
  return(sd->data[chan]);
}

static sound_data *c_make_sound_data(int chans, mus_long_t framples)
{
  int i;
  sound_data *sd;

  sd = (sound_data *)malloc(sizeof(sound_data));
  sd->length = framples;
  sd->chans = chans;
  sd->wrapped = false;
  sd->data = (mus_float_t **)calloc(chans, sizeof(mus_float_t *));
  for (i = 0; i < chans; i++)
    sd->data[i] = (mus_float_t *)calloc(framples, sizeof(mus_float_t));
  return(sd);
}

static sound_data *sound_data_copy(sound_data *sd)
{
  int i, chans;
  mus_long_t len;
  sound_data *sdnew;

  chans = mus_sound_data_chans(sd);
  len = mus_sound_data_length(sd);

  sdnew = c_make_sound_data(chans, len);
  for (i = 0; i < chans; i++)
    memcpy((void *)(mus_sound_data_channel_data(sdnew, i)), (void *)(mus_sound_data_channel_data(sd, i)), len * sizeof(mus_float_t));
  return(sdnew);
}

static void sound_data_free(sound_data *sd)
{
  if (sd)
    {
      mus_float_t **data;
      data = mus_sound_data_data(sd);
      if ((data) && (!(sd->wrapped)))
	{
	  int i, chans;
	  chans = mus_sound_data_chans(sd);
	  for (i = 0; i < chans; i++)
	    if (data[i]) free(data[i]);
	  free(data);
	}
      sd->data = NULL;
      sd->chans = 0;
      free(sd);
    }
}

#else

/* in s7, sound-data is float-vector, so a sound-data* is an s7_pointer to an s7 vector
 */

int mus_sound_data_chans(sound_data *p)
{
  return((int)((s7_vector_dimensions(p))[0]));
}

mus_long_t mus_sound_data_length(sound_data *p)
{
  return(s7_vector_dimensions(p)[1]);
}

static mus_float_t *mus_sound_data_channel_data(sound_data *p, int chan)
{
  if (chan == 0)
    return((mus_float_t *)(s7_float_vector_elements(p)));
  return((mus_float_t *)((s7_Double *)(s7_float_vector_elements(p) + chan * mus_sound_data_length(p))));
}
#endif


/* -------------------------------------------------------------------------------- */

mus_float_t mus_sound_data_ref(sound_data *sd, int chan, mus_long_t pos)
{
  if ((sd) &&
      (chan < mus_sound_data_chans(sd)) &&
      (pos >= 0) &&
      (pos < mus_sound_data_length(sd)))
    return(mus_sound_data_channel_data(sd, chan)[pos]);
  return(0.0);
}

mus_float_t mus_sound_data_set(sound_data *sd, int chan, mus_long_t pos, mus_float_t val)
{
  if ((sd) &&
      (chan < mus_sound_data_chans(sd)) &&
      (pos >= 0) &&
      (pos < mus_sound_data_length(sd)))
    {
      mus_float_t *d;
      d = mus_sound_data_channel_data(sd, chan);
      d[pos] = val;
    }
  return(val);
}

void mus_sound_data_add_frample(sound_data *sd, mus_long_t pos, mus_float_t *data)
{
  int i, chans;
  chans = mus_sound_data_chans(sd);

  if ((pos >= 0) &&
      (pos < mus_sound_data_length(sd)))
    for (i = 0; i < chans; i++)
      {
	mus_float_t *d;
	d = mus_sound_data_channel_data(sd, i);
	d[pos] += data[i];
      }
}
#endif /* (!DISABLE_DEPRECATED) */


/* originally I tried to simplify C GC by using global static strings that were 
 *   freed whenever the associated function was called again, on the assumption
 *   that the preceding value was now unused.  In a multithread context, that
 *   assumption is false, so I didn't use code like this:
 *
 *  static char *tmpstr = NULL;
 *
 *  static char *local_mus_expand_filename(char *name)
 *  {
 *    if (tmpstr) {free(tmpstr); tmpstr = NULL;}
 *       tmpstr = mus_expand_filename(name);
 *    return(tmpstr);
 *  }
 */

static Xen g_mus_sound_loop_info(Xen gfilename)
{
  #define H_mus_sound_loop_info "(" S_mus_sound_loop_info " filename): synth loop info for sound as a list: (start1 \
end1 start2 end2 base-note base-detune mode1 mode2)"
  int *res;
  Xen sres = Xen_empty_list;
  char *str = NULL;

  Xen_check_type(Xen_is_string(gfilename), gfilename, 1, S_mus_sound_loop_info, "a string"); 

  res = mus_sound_loop_info(str = mus_expand_filename(Xen_string_to_C_string(gfilename)));
  if (str) free(str);
  if (res)
    {
      sres = Xen_list_8(C_int_to_Xen_integer(res[0]), C_int_to_Xen_integer(res[1]), C_int_to_Xen_integer(res[2]),
			C_int_to_Xen_integer(res[3]), C_int_to_Xen_integer(res[4]), C_int_to_Xen_integer(res[5]),
			C_int_to_Xen_integer(res[6]), C_int_to_Xen_integer(res[7]));
      free(res);
    }
  return(sres);
}


static Xen g_mus_sound_mark_info(Xen gfilename)
{
  #define H_mus_sound_mark_info "(" S_mus_sound_mark_info " filename): aifc header mark info as a list of lists: ((id pos)...)"
  int *mark_ids, *mark_positions;
  int marks = 0;
  Xen sres = Xen_empty_list;
  char *str = NULL;

  Xen_check_type(Xen_is_string(gfilename), gfilename, 1, S_mus_sound_mark_info, "a string"); 

  marks = mus_sound_mark_info(str = mus_expand_filename(Xen_string_to_C_string(gfilename)), &mark_ids, &mark_positions);
  if (str) free(str);
  if (marks > 0)
    {
      int i;
      for (i = 0; i < marks; i++)
	sres = Xen_cons(Xen_list_2(C_int_to_Xen_integer(mark_ids[i]),
				   C_int_to_Xen_integer(mark_positions[i])),
			sres);
    }
  return(sres);
}


static Xen gmus_sound(const char *caller, int (*func)(const char *file), Xen gfilename)
{
  char *str = NULL;
  Xen result;

  Xen_check_type(Xen_is_string(gfilename), gfilename, 1, caller, "a string"); 
  str = mus_expand_filename(Xen_string_to_C_string(gfilename));
  result = C_int_to_Xen_integer((*func)(str));
  if (str) free(str);
  return(result);
}


static Xen gmus_sound_set(const char *caller, int (*func)(const char *file, int newval), Xen gfilename, Xen val)
{
  char *str = NULL;
  Xen result;

  Xen_check_type(Xen_is_string(gfilename), gfilename, 1, caller, "a string"); 
  Xen_check_type(Xen_is_integer(val), val, 2, caller, "an integer");
  str = mus_expand_filename(Xen_string_to_C_string(gfilename));
  result = C_int_to_Xen_integer((*func)(str, Xen_integer_to_C_int(val)));
  if (str) free(str);
  return(result);
}


static Xen glmus_sound(const char *caller, mus_long_t (*func)(const char *file), Xen gfilename)
{
  char *str = NULL;
  Xen result;

  Xen_check_type(Xen_is_string(gfilename), gfilename, 1, caller, "a string"); 
  str = mus_expand_filename(Xen_string_to_C_string(gfilename));
  result = C_llong_to_Xen_llong((*func)(str));
  if (str) free(str);
  return(result);
}


static Xen glmus_sound_set(const char *caller, int (*func)(const char *file, mus_long_t newval), Xen gfilename, Xen val)
{
  char *str = NULL;
  Xen result;

  Xen_check_type(Xen_is_string(gfilename), gfilename, 1, caller, "a string"); 
  Xen_check_type(Xen_is_number(val), val, 2, caller, "a number");
  str = mus_expand_filename(Xen_string_to_C_string(gfilename));
  result = C_llong_to_Xen_llong((*func)(str, Xen_llong_to_C_llong(val)));
  if (str) free(str);
  return(result);
}


static Xen g_mus_sound_samples(Xen filename) 
{
  #define H_mus_sound_samples "(" S_mus_sound_samples " filename): samples (framples * channels) in sound file"
  return(glmus_sound(S_mus_sound_samples, mus_sound_samples, filename));
}


static Xen g_mus_sound_set_samples(Xen filename, Xen val) 
{
  return(glmus_sound_set(S_setB S_mus_sound_samples, mus_sound_set_samples, filename, val));
}


Xen g_mus_sound_framples(Xen filename) 
{
  #define H_mus_sound_framples "(" S_mus_sound_framples " filename): framples (samples / channel) in sound file"
  return(glmus_sound(S_mus_sound_framples, mus_sound_framples, filename));
}


static Xen g_mus_sound_datum_size(Xen filename) 
{
  #define H_mus_sound_datum_size "(" S_mus_sound_datum_size " filename): bytes per sample used by the data in sound file (data format dependent)"
  return(gmus_sound(S_mus_sound_datum_size, mus_sound_datum_size, filename));
}


static Xen g_mus_sound_data_location(Xen filename) 
{
  #define H_mus_sound_data_location "(" S_mus_sound_data_location " filename): location (in bytes) of first sample of sound data"
  return(glmus_sound(S_mus_sound_data_location, mus_sound_data_location, filename));
}


static Xen g_mus_sound_set_data_location(Xen filename, Xen val) 
{
  return(glmus_sound_set(S_setB S_mus_sound_data_location, mus_sound_set_data_location, filename, val));
}


Xen g_mus_sound_chans(Xen filename) 
{
  #define H_mus_sound_chans "(" S_mus_sound_chans " filename): channels of data in sound file"
  return(gmus_sound(S_mus_sound_chans, mus_sound_chans, filename));
}


static Xen g_mus_sound_set_chans(Xen filename, Xen val) 
{
  return(gmus_sound_set(S_setB S_mus_sound_chans, mus_sound_set_chans, filename, val));
}


Xen g_mus_sound_srate(Xen filename) 
{
  #define H_mus_sound_srate "(" S_mus_sound_srate " filename): sampling rate of sound file"
  return(gmus_sound(S_mus_sound_srate, mus_sound_srate, filename));
}


static Xen g_mus_sound_set_srate(Xen filename, Xen val) 
{
  return(gmus_sound_set(S_setB S_mus_sound_srate, mus_sound_set_srate, filename, val));
}


static Xen g_mus_sound_header_type(Xen filename) 
{
  #define H_mus_sound_header_type "(" S_mus_sound_header_type " filename): header type (e.g. " S_mus_aifc ") of sound file"
  return(gmus_sound(S_mus_sound_header_type, mus_sound_header_type, filename));
}


static Xen g_mus_sound_set_header_type(Xen filename, Xen val) 
{
  return(gmus_sound_set(S_setB S_mus_sound_header_type, mus_sound_set_header_type, filename, val));
}


static Xen g_mus_sound_data_format(Xen filename) 
{
  #define H_mus_sound_data_format "(" S_mus_sound_data_format " filename): data format (e.g. " S_mus_bshort ") of data in sound file"
  return(gmus_sound(S_mus_sound_data_format, mus_sound_data_format, filename));
}


static Xen g_mus_sound_set_data_format(Xen filename, Xen val) 
{
  return(gmus_sound_set(S_setB S_mus_sound_data_format, mus_sound_set_data_format, filename, val));
}


static Xen g_mus_sound_length(Xen filename) 
{
  #define H_mus_sound_length "(" S_mus_sound_length " filename): sound file length in bytes"
  return(glmus_sound(S_mus_sound_length, mus_sound_length, filename));
}


static Xen g_mus_sound_type_specifier(Xen filename) 
{
  #define H_mus_sound_type_specifier "(" S_mus_sound_type_specifier " filename): original sound file header type identifier (e.g. 0x2e736e64)"
  return(gmus_sound(S_mus_sound_type_specifier, mus_sound_type_specifier, filename));
}


static Xen g_mus_sound_forget(Xen filename) 
{
  #define H_mus_sound_forget "(" S_mus_sound_forget " filename): remove 'filename' from sound cache.  If you create, then later \
delete a sound file, " S_mus_sound_forget " can be used to clear it from sndlib's cache of sound files"
  return(gmus_sound(S_mus_sound_forget, mus_sound_forget, filename));
}


static Xen g_mus_sound_prune(void) 
{
  #define H_mus_sound_prune "(" S_mus_sound_prune "): remove all defunct entries from sndlib's sound file cache."
  return(C_int_to_Xen_integer(mus_sound_prune()));
}


static Xen g_mus_sound_comment(Xen gfilename) 
{
  #define H_mus_sound_comment "(" S_mus_sound_comment " filename): comment (a string) found in sound file's header"
  char *res = NULL, *str = NULL; 
  Xen newstr;

  Xen_check_type(Xen_is_string(gfilename), gfilename, 1, S_mus_sound_comment, "a string"); 

  res = mus_sound_comment(str = mus_expand_filename(Xen_string_to_C_string(gfilename)));
  if (str) free(str);
  newstr = C_string_to_Xen_string(res);
  if (res) free(res);
  return(newstr);
}


static Xen g_mus_sound_write_date(Xen filename) 
{
  char *str = NULL;
  Xen result;

  #define H_mus_sound_write_date "(" S_mus_sound_write_date " filename): write date of sound file"
  Xen_check_type(Xen_is_string(filename), filename, 1, S_mus_sound_write_date, "a string"); 
  str = mus_expand_filename(Xen_string_to_C_string(filename));
  result = C_ulong_to_Xen_ulong((unsigned long)mus_sound_write_date(str)); /* actually time_t */
  if (str) free(str);
  return(result);
}


static Xen g_mus_header_writable(Xen head, Xen data)
{
  #define H_mus_header_writable "(" S_mus_header_writable " header-type data-format) returns " PROC_TRUE " if the header can handle the data format"
  Xen_check_type(Xen_is_integer(head), head, 1, S_mus_header_writable, "a header type");
  Xen_check_type(Xen_is_integer(data), data, 2, S_mus_header_writable, "a data format");
  return(C_bool_to_Xen_boolean(mus_header_writable(Xen_integer_to_C_int(head), Xen_integer_to_C_int(data))));
}

static Xen g_mus_header_raw_defaults(void)
{
  #define H_mus_header_raw_defaults "(" S_mus_header_raw_defaults "): returns list '(srate chans format) of current raw sound default attributes"
  int srate, chans, data_format;
  mus_header_raw_defaults(&srate, &chans, &data_format);
  return(Xen_list_3(C_int_to_Xen_integer(srate),
		    C_int_to_Xen_integer(chans),
		    C_int_to_Xen_integer(data_format)));
}


static Xen g_mus_header_set_raw_defaults(Xen lst)
{
  Xen_check_type((Xen_is_list(lst)) && (Xen_list_length(lst) == 3), lst, 1, S_mus_header_raw_defaults, "a list: '(srate chans data-format)");
  Xen_check_type(Xen_is_integer(Xen_car(lst)), Xen_car(lst), 1, S_mus_header_raw_defaults, "an integer = srate");
  Xen_check_type(Xen_is_integer(Xen_cadr(lst)), Xen_cadr(lst), 2, S_mus_header_raw_defaults, "an integer = chans");
  Xen_check_type(Xen_is_integer(Xen_caddr(lst)), Xen_caddr(lst), 3, S_mus_header_raw_defaults, "an integer = data-format");
  mus_header_set_raw_defaults(Xen_integer_to_C_int(Xen_car(lst)),
			      Xen_integer_to_C_int(Xen_cadr(lst)),
			      Xen_integer_to_C_int(Xen_caddr(lst)));
  return(lst);
}


static Xen g_mus_header_type_name(Xen type) 
{
  #define H_mus_header_type_name "(" S_mus_header_type_name " type): header type (e.g. " S_mus_aiff ") as a string"
  Xen_check_type(Xen_is_integer(type), type, 1, S_mus_header_type_name, "an integer (header-type id)"); 
  return(C_string_to_Xen_string(mus_header_type_name(Xen_integer_to_C_int(type))));
}


static Xen g_mus_header_type_to_string(Xen type) 
{
  #define H_mus_header_type_to_string "(" S_mus_header_type_to_string " type): header type (e.g. " S_mus_aiff ") as a string"
  Xen_check_type(Xen_is_integer(type), type, 1, S_mus_header_type_to_string, "an integer (header-type id)"); 
  return(C_string_to_Xen_string(mus_header_type_to_string(Xen_integer_to_C_int(type))));
}


static Xen g_mus_data_format_name(Xen format) 
{
  #define H_mus_data_format_name "(" S_mus_data_format_name " format): data format (e.g. " S_mus_bshort ") as a string"
  Xen_check_type(Xen_is_integer(format), format, 1, S_mus_data_format_name, "an integer (data-format id)"); 
  return(C_string_to_Xen_string(mus_data_format_name(Xen_integer_to_C_int(format))));
}


static Xen g_mus_data_format_to_string(Xen format) 
{
  #define H_mus_data_format_to_string "(" S_mus_data_format_to_string " format): data format (e.g. " S_mus_bshort ") as a string"
  Xen_check_type(Xen_is_integer(format), format, 1, S_mus_data_format_to_string, "an integer (data-format id)"); 
  return(C_string_to_Xen_string(mus_data_format_to_string(Xen_integer_to_C_int(format))));
}


static Xen g_mus_bytes_per_sample(Xen format) 
{
  #define H_mus_bytes_per_sample "(" S_mus_bytes_per_sample " format): number of bytes per sample in \
format (e.g. " S_mus_bshort " = 2)"
  Xen_check_type(Xen_is_integer(format), format, 1, S_mus_bytes_per_sample, "an integer (data-format id)"); 
  return(C_int_to_Xen_integer(mus_bytes_per_sample(Xen_integer_to_C_int(format))));
}


static Xen g_mus_sound_duration(Xen gfilename) 
{
  #define H_mus_sound_duration "(" S_mus_sound_duration " filename): duration (in seconds) of sound file"
  float res;
  char *str = NULL;

  Xen_check_type(Xen_is_string(gfilename), gfilename, 1, S_mus_sound_duration, "a string"); 
  res = mus_sound_duration(str = mus_expand_filename(Xen_string_to_C_string(gfilename)));
  if (str) free(str);
  return(C_double_to_Xen_real(res));

}


static Xen g_mus_oss_set_buffers(Xen num, Xen size)
{
  #define H_mus_oss_set_buffers "(" S_mus_oss_set_buffers " num size): set Linux OSS 'fragment' number and size. \
If Snd's controls seem sluggish, try (" S_mus_oss_set_buffers " 4 12) or even (" S_mus_oss_set_buffers " 2 12). \
This reduces the on-card buffering, but may introduce clicks."

#if (HAVE_OSS || HAVE_ALSA)
  Xen_check_type(Xen_is_integer(num), num, 1, S_mus_oss_set_buffers, "an integer");
  Xen_check_type(Xen_is_integer(size), size, 2, S_mus_oss_set_buffers, "an integer");
  mus_oss_set_buffers(Xen_integer_to_C_int(num),
			    Xen_integer_to_C_int(size));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_buffers(void)
{
  #define H_mus_alsa_buffers "(" S_mus_alsa_buffers "): current number of ALSA periods."
#if HAVE_ALSA
  return(C_int_to_Xen_integer(mus_alsa_buffers()));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_set_buffers(Xen val)
{
  Xen_check_type(Xen_is_integer(val), val, 1, S_setB S_mus_alsa_buffers, "an integer");
#if HAVE_ALSA
  return(C_int_to_Xen_integer(mus_alsa_set_buffers(Xen_integer_to_C_int(val))));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_buffer_size(void)
{
  #define H_mus_alsa_buffer_size "(" S_mus_alsa_buffer_size "): current size of ALSA buffers."
#if HAVE_ALSA
  return(C_int_to_Xen_integer(mus_alsa_buffer_size()));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_set_buffer_size(Xen val)
{
  Xen_check_type(Xen_is_integer(val), val, 1, S_setB S_mus_alsa_buffer_size, "an integer");
#if HAVE_ALSA
  return(C_int_to_Xen_integer(mus_alsa_set_buffer_size(Xen_integer_to_C_int(val))));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_device(void)
{
  #define H_mus_alsa_device "(" S_mus_alsa_device "): current ALSA device."
#if HAVE_ALSA
  return(C_string_to_Xen_string(mus_alsa_device()));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_set_device(Xen val)
{
  Xen_check_type(Xen_is_string(val), val, 1, S_setB S_mus_alsa_device, "a string (ALSA device name)");
#if HAVE_ALSA
  return(C_string_to_Xen_string(mus_alsa_set_device(Xen_string_to_C_string(val))));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_playback_device(void)
{
  #define H_mus_alsa_playback_device "(" S_mus_alsa_playback_device "): current ALSA playback device."
#if HAVE_ALSA
  return(C_string_to_Xen_string(mus_alsa_playback_device()));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_set_playback_device(Xen val)
{
  Xen_check_type(Xen_is_string(val), val, 1, S_setB S_mus_alsa_playback_device, "a string (ALSA device name)");
#if HAVE_ALSA
  return(C_string_to_Xen_string(mus_alsa_set_playback_device(Xen_string_to_C_string(val))));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_capture_device(void)
{
  #define H_mus_alsa_capture_device "(" S_mus_alsa_capture_device "): current ALSA capture device."
#if HAVE_ALSA
  return(C_string_to_Xen_string(mus_alsa_capture_device()));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_set_capture_device(Xen val)
{
  Xen_check_type(Xen_is_string(val), val, 1, S_setB S_mus_alsa_capture_device, "a string (ALSA device name)");
#if HAVE_ALSA
  return(C_string_to_Xen_string(mus_alsa_set_capture_device(Xen_string_to_C_string(val))));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_squelch_warning(void)
{
  #define H_mus_alsa_squelch_warning "(" S_mus_alsa_squelch_warning "): whether to squelch ALSA srate mismatch warnings."
#if HAVE_ALSA
  return(C_bool_to_Xen_boolean(mus_alsa_squelch_warning()));
#endif
  return(Xen_false);
}


static Xen g_mus_alsa_set_squelch_warning(Xen val)
{
  Xen_check_type(Xen_is_boolean(val), val, 1, S_setB S_mus_alsa_squelch_warning, "a boolean");
#if HAVE_ALSA
  return(C_bool_to_Xen_boolean(mus_alsa_set_squelch_warning(Xen_boolean_to_C_bool(val))));
#endif
  return(Xen_false);
}


static Xen g_mus_sound_maxamp_exists(Xen file)
{
  #define H_mus_sound_maxamp_exists "(" S_mus_sound_maxamp_exists " filename): " PROC_TRUE " if sound's maxamp data is available \
in the sound cache; if it isn't, a call on " S_mus_sound_maxamp " has to open and read the data to get the maxamp."
  bool val;
  char *str = NULL;

  Xen_check_type(Xen_is_string(file), file, 1, S_mus_sound_maxamp_exists, "a string");
  val = mus_sound_maxamp_exists(str = mus_expand_filename(Xen_string_to_C_string(file)));
  if (str) free(str);
  return(C_bool_to_Xen_boolean(val));
}


Xen g_mus_sound_maxamp(Xen file)
{
  #define H_mus_sound_maxamp "(" S_mus_sound_maxamp " filename): maxamps in sound (a list of paired amps (floats) and locations (samples))"
  int chans;
  char *filename;
  Xen res = Xen_empty_list;

  Xen_check_type(Xen_is_string(file), file, 1, S_mus_sound_maxamp, "a string");

  filename = mus_expand_filename(Xen_string_to_C_string(file));
  chans = mus_sound_chans(filename);
  if (chans > 0)
    {
      mus_long_t rtn;
      int i;
      mus_float_t *vals;
      mus_long_t *times;

      vals = (mus_float_t *)calloc(chans, sizeof(mus_float_t));
      times = (mus_long_t *)calloc(chans, sizeof(mus_long_t));

      rtn = mus_sound_maxamps(filename, chans, vals, times);
      if (rtn != MUS_ERROR)
	for (i = chans - 1; i >= 0; i--)
	  res = Xen_cons(C_llong_to_Xen_llong(times[i]),
		  Xen_cons(C_double_to_Xen_real(vals[i]), res));
      free(vals);
      free(times);
      if (filename) free(filename);
    }
  else 
    {
      if (filename) free(filename);
      Xen_error(BAD_HEADER,
		Xen_list_1(C_string_to_Xen_string(S_mus_sound_maxamp ": chans <= 0")));
    }
  return(res);
}


static Xen g_mus_sound_set_maxamp(Xen file, Xen vals)
{
  int chans;
  char *filename;

  Xen_check_type(Xen_is_string(file), file, 1, S_setB S_mus_sound_maxamp, "a string");
  Xen_check_type(Xen_is_list(vals), vals, 2, S_setB S_mus_sound_maxamp, "a list");

  filename = mus_expand_filename(Xen_string_to_C_string(file));
  chans = mus_sound_chans(filename);

  /* presumably any actual error here will be trapped via mus-error (raised in mus_header_read via read_sound_file_header),
   *   so chans <= 0 is actually in the header?
   */
  if (chans > 0)
    {
      Xen lst;
      int i, j, len;
      mus_float_t *mvals;
      mus_long_t *times;

      len = Xen_list_length(vals);
      if (len < (chans * 2))
	Xen_wrong_type_arg_error(S_setB S_mus_sound_maxamp, 2, vals, "max amp list length must = 2 * chans");
      if (len > chans * 2) len = chans * 2;

      mvals = (mus_float_t *)calloc(chans, sizeof(mus_float_t));
      times = (mus_long_t *)calloc(chans, sizeof(mus_long_t));

      for (i = 0, j = 0, lst = Xen_copy_arg(vals); i < len; i += 2, j++, lst = Xen_cddr(lst))
	{
	  times[j] = Xen_llong_to_C_llong(Xen_car(lst));
	  mvals[j] = Xen_real_to_C_double(Xen_cadr(lst));
	}

      fprintf(stderr, "set in g_mus_sound_set_maxamp\n");
      mus_sound_set_maxamps(filename, chans, mvals, times);
      free(mvals);
      free(times);
      if (filename) free(filename);
    }
  else 
    {
      if (filename) free(filename);
      Xen_error(BAD_HEADER,
		Xen_list_1(C_string_to_Xen_string(S_setB S_mus_sound_maxamp ": chans <= 0")));
    }
  return(vals);
}


#if (!DISABLE_DEPRECATED)
static Xen g_mus_sound_open_input(Xen file)
{
  #define H_mus_sound_open_input "(" S_mus_sound_open_input " filename): open filename for (low-level) sound input, \
return file descriptor (an integer)"
  int fd;
  char *str = NULL;

  Xen_check_type(Xen_is_string(file), file, 1, S_mus_sound_open_input, "a string");
  fd = mus_sound_open_input(str = mus_expand_filename(Xen_string_to_C_string(file)));
  if (str) free(str);
  return(C_int_to_Xen_integer(fd));
}


static Xen g_mus_sound_open_output(Xen file, Xen srate, Xen chans, Xen data_format, Xen header_type, Xen comment)
{

  #define H_mus_sound_open_output "(" S_mus_sound_open_output " filename :optional (srate 44100) (chans 1) data-format header-type (comment \"\")): \
open filename for (low-level) sound output with the given srate and so on; return the file descriptor (an integer). \
The file size is normally set later via " S_mus_sound_close_output ". srate is an integer, comment is a string, \
data-format is a sndlib format indicator such as " S_mus_bshort ", if " PROC_FALSE " if defaults to a format compatible with sndlib, \
header-type is a sndlib type indicator such as " S_mus_aiff "; sndlib currently only writes 5 or so header types."

  int fd = -1, df;
  Xen_check_type(Xen_is_string(file), file, 1, S_mus_sound_open_output, "a string");
  Xen_check_type(Xen_is_integer_boolean_or_unbound(srate), srate, 2, S_mus_sound_open_output, "an integer or " PROC_FALSE);
  Xen_check_type(Xen_is_integer_boolean_or_unbound(chans), chans, 3, S_mus_sound_open_output, "an integer or " PROC_FALSE);
  Xen_check_type(Xen_is_integer_boolean_or_unbound(data_format), data_format, 4, S_mus_sound_open_output, "a data-format or " PROC_FALSE);
  Xen_check_type(Xen_is_integer_boolean_or_unbound(header_type), header_type, 5, S_mus_sound_open_output, "a header-type or " PROC_FALSE);
  Xen_check_type((Xen_is_string(comment) || (!Xen_is_bound(comment))), comment, 6, S_mus_sound_open_output, "a string");

  df = (Xen_is_integer(data_format)) ? Xen_integer_to_C_int(data_format) : (int)MUS_OUT_FORMAT;
  if (mus_is_data_format(df))
    {
      int ht;
#if MUS_LITTLE_ENDIAN
      ht = (Xen_is_integer(header_type)) ? Xen_integer_to_C_int(header_type) : (int)MUS_RIFF;
#else
      ht = (Xen_is_integer(header_type)) ? Xen_integer_to_C_int(header_type) : (int)MUS_NEXT;
#endif
      /* now check that data format and header type are ok together */
      if (mus_is_header_type(ht))
	{
	  int chns;

	  if (!mus_header_writable(ht, df))
	    {
	      if ((Xen_is_integer(data_format)) &&
		  (Xen_is_integer(header_type)))
		Xen_error(Xen_make_error_type("bad-data"),
			  Xen_list_3(C_string_to_Xen_string(S_mus_sound_open_output ": ~A header can't handle ~A data"),
				     C_string_to_Xen_string(mus_header_type_name(ht)),
				     C_string_to_Xen_string(mus_data_format_name(df))));

	      if (!(Xen_is_integer(data_format)))
		{
		  switch (df)
		    {
		    case MUS_BFLOAT:  df = MUS_LFLOAT;  break;
		    case MUS_BDOUBLE: df = MUS_LDOUBLE; break;
		    case MUS_BINT:    df = MUS_LINT;    break;
		    case MUS_LFLOAT:  df = MUS_BFLOAT;  break;
		    case MUS_LDOUBLE: df = MUS_BDOUBLE; break;
		    case MUS_LINT:    df = MUS_BINT;    break;
		    }
		  if (!mus_header_writable(ht, df))
		    {
		      int i;
		      for (i = 1; i < MUS_NUM_DATA_FORMATS; i++) /* MUS_UNSUPPORTED is 0 */
			{
			  df = i;
			  if (mus_header_writable(ht, df))
			    break;
			}
		    }
		}
	      else
		{
		  ht = MUS_NEXT;
		}
	  
	      if (!mus_header_writable(ht, df))
		Xen_error(Xen_make_error_type("bad-data"),
			  Xen_list_3(C_string_to_Xen_string(S_mus_sound_open_output ": ~A header can't handle ~A data"),
				     C_string_to_Xen_string(mus_header_type_name(ht)),
				     C_string_to_Xen_string(mus_data_format_name(df))));
	    }

	  chns = (Xen_is_integer(chans)) ? Xen_integer_to_C_int(chans) : 1;
	  if (chns > 0)
	    {
	      const char *com = NULL;
	      char *str = NULL;
	      str = (char *)Xen_string_to_C_string(file);
	      if ((str) && (*str))
		{
		  str = mus_expand_filename(str);
		  if (Xen_is_string(comment)) com = Xen_string_to_C_string(comment);
		  fd = mus_sound_open_output(str, 
					     (Xen_is_integer(srate)) ? Xen_integer_to_C_int(srate) : 44100,  /* not DEFAULT_OUTPUT_SRATE here because that depends on Snd */
					     chns, df, ht, com);
		  if (str) free(str);
		}
	      else Xen_out_of_range_error(S_mus_sound_open_output, 1, file, "a filename");
	    }
	  else Xen_out_of_range_error(S_mus_sound_open_output, 3, chans, "chans <= 0?");
	}
      else Xen_out_of_range_error(S_mus_sound_open_output, 5, header_type, "invalid header type");
    }
  else Xen_out_of_range_error(S_mus_sound_open_output, 4, data_format, "invalid data format");
  return(C_int_to_Xen_integer(fd));
}


static Xen g_mus_sound_reopen_output(Xen file, Xen chans, Xen data_format, Xen header_type, Xen data_loc)
{

  #define H_mus_sound_reopen_output "(" S_mus_sound_reopen_output " filename :optional (chans 1) data-format header-type data-location): \
reopen (without alteration) filename for output \
data-format and header-type are sndlib indicators such as " S_mus_bshort " or " S_mus_aiff ". \
data-location should be retrieved from a previous call to " S_mus_sound_data_location "."

  int fd = -1, df;
  char *filename;
  Xen_check_type(Xen_is_string(file), file, 1, S_mus_sound_reopen_output, "a string");
  Xen_check_type(Xen_is_integer_boolean_or_unbound(chans), chans, 2, S_mus_sound_reopen_output, "an integer or " PROC_FALSE);
  Xen_check_type(Xen_is_integer_boolean_or_unbound(data_format), data_format, 3, S_mus_sound_reopen_output, "a data-format or " PROC_FALSE);
  Xen_check_type(Xen_is_integer_boolean_or_unbound(header_type), header_type, 4, S_mus_sound_reopen_output, "a header-type or " PROC_FALSE);
  Xen_check_type(Xen_is_llong(data_loc) || Xen_is_false(data_loc) || !Xen_is_bound(data_loc), 
		  data_loc, 5, S_mus_sound_reopen_output, "an integer or " PROC_FALSE);

  filename = mus_expand_filename(Xen_string_to_C_string(file));

  /* use existing settings if unbound as args */
  if (Xen_is_integer(data_format))
    df = Xen_integer_to_C_int(data_format);
  else
    {
      df = mus_sound_data_format(filename);
      if (df == MUS_ERROR)
	df = MUS_OUT_FORMAT;
    }
  if (mus_is_data_format(df))
    {
      int ht;
      if (Xen_is_integer(header_type))
	ht = Xen_integer_to_C_int(header_type);
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
      if (mus_is_header_type(ht))
	{
	  int chns;
	  if (Xen_is_integer(chans))
	    chns = Xen_integer_to_C_int(chans);
	  else
	    {
	      chns = mus_sound_chans(filename);
	      if (chns == MUS_ERROR)
		chns = 1;
	    }
	  if (chns > 0)
	    {
	      mus_long_t dloc;
	      if (Xen_is_llong(data_loc))
		dloc = Xen_llong_to_C_llong(data_loc);
	      else
		{
		  dloc = mus_sound_data_location(filename);
		  if (dloc == MUS_ERROR)
		    dloc = 0;
		}
	      fd = mus_sound_reopen_output(filename, chns, df, ht, dloc);
	      if (filename) free(filename);
	    }
	  else Xen_out_of_range_error(S_mus_sound_reopen_output, 2, chans, "chans <= 0?");
	}
      else Xen_out_of_range_error(S_mus_sound_reopen_output, 4, header_type, "invalid header type");
    }
  else Xen_out_of_range_error(S_mus_sound_reopen_output, 3, data_format, "invalid data format");
  return(C_int_to_Xen_integer(fd));
}


#if _MSC_VER
#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif
#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif
#ifndef STDERR_FILENO
#define STDERR_FILENO 2
#endif
#endif

static Xen g_mus_sound_close_input(Xen fd)
{
  #define H_mus_sound_close_input "(" S_mus_sound_close_input " fd): close (low-level) file fd that was opened \
by " S_mus_sound_open_input "."
  int nfd;
  Xen_check_type(Xen_is_integer(fd), fd, 1, S_mus_sound_close_input, "an integer");
  nfd = Xen_integer_to_C_int(fd);
  if ((nfd < 0) || (nfd == STDIN_FILENO) || (nfd == STDOUT_FILENO) || (nfd == STDERR_FILENO))
    Xen_out_of_range_error(S_mus_sound_close_input, 1, fd, "invalid file number");
  return(C_int_to_Xen_integer(mus_sound_close_input(Xen_integer_to_C_int(fd))));
}


static Xen g_mus_sound_close_output(Xen fd, Xen bytes)
{
  #define H_mus_sound_close_output "(" S_mus_sound_close_output " fd bytes): close (low-level) file fd \
that was opened by " S_mus_sound_open_output " after updating its header (if any) to reflect bytes, the new file data size"

  int nfd;
  Xen_check_type(Xen_is_integer(fd), fd, 1, S_mus_sound_close_output, "an integer");
  Xen_check_type(Xen_is_llong(bytes), bytes, 2, S_mus_sound_close_output, "an integer");
  nfd = Xen_integer_to_C_int(fd);
  if ((nfd < 0) || (nfd == STDIN_FILENO) || (nfd == STDOUT_FILENO) || (nfd == STDERR_FILENO))
    Xen_out_of_range_error(S_mus_sound_close_output, 1, fd, "invalid file number");
  return(C_int_to_Xen_integer(mus_sound_close_output(Xen_integer_to_C_int(fd), Xen_llong_to_C_llong(bytes))));
}


static Xen g_mus_sound_read(Xen fd, Xen beg, Xen num, Xen chans, Xen sv)
{
  #define H_mus_sound_read "(" S_mus_sound_read " fd beg num chans sdata): read sound data from file fd, \
filling sound-data sdata's buffers starting at frample beg in the file, and writing num samples to the buffers"

  sound_data *sd;
  mus_long_t bg, nd;
  int i, nchans;
  Xen val;
  mus_float_t **bufs;

  Xen_check_type(Xen_is_integer(fd), fd, 1, S_mus_sound_read, "an integer");
  Xen_check_type(Xen_is_llong(beg), beg, 2, S_mus_sound_read, "an integer");
  Xen_check_type(Xen_is_llong(num), num, 3, S_mus_sound_read, "an integer");
  Xen_check_type(Xen_is_integer(chans), chans, 4, S_mus_sound_read, "an integer");
  Xen_check_type(xen_is_sound_data(sv), sv, 5, S_mus_sound_read, "a sound-data object");

  sd = Xen_to_sound_data(sv);
  bg = Xen_llong_to_C_llong(beg);
  nd = Xen_llong_to_C_llong(num);
  if (nd > mus_sound_data_length(sd))
    Xen_error(Xen_make_error_type("out-of-range"),
	      Xen_list_3(C_string_to_Xen_string(S_mus_sound_read ": num ~A > sound-data array length, ~A"),
			 num, 
			 C_llong_to_Xen_llong(mus_sound_data_length(sd))));

  nchans = mus_sound_data_chans(sd);
  bufs = (mus_float_t **)malloc(nchans * sizeof(mus_float_t *));
  for (i = 0; i < nchans; i++)
    bufs[i] = mus_sound_data_channel_data(sd, i);

  val = C_int_to_Xen_integer(mus_file_read(Xen_integer_to_C_int(fd), bg, nd, Xen_integer_to_C_int(chans), bufs));
  
  free(bufs);
  return(val);
}


static Xen g_mus_sound_write(Xen fd, Xen beg, Xen end, Xen chans, Xen sv)
{
  #define H_mus_sound_write "(" S_mus_sound_write " fd beg end chans sdata): write sound-data sdata's data to file fd, \
starting at beg (buffer location), going to end"

  sound_data *sd;
  mus_long_t bg, nd;
  int i, nchans;
  Xen val;
  mus_float_t **bufs;

  Xen_check_type(Xen_is_integer(fd), fd, 1, S_mus_sound_write, "an integer");
  Xen_check_type(Xen_is_llong(beg), beg, 2, S_mus_sound_write, "an integer");
  Xen_check_type(Xen_is_llong(end), end, 3, S_mus_sound_write, "an integer");
  Xen_check_type(Xen_is_integer(chans), chans, 4, S_mus_sound_write, "an integer");
  Xen_check_type(xen_is_sound_data(sv), sv, 5, S_mus_sound_write, "a sound-data object");

  /* even here we can write memory that doesn't belong to us if clipping */
  sd = Xen_to_sound_data(sv);
  bg = Xen_llong_to_C_llong(beg);
  nd = Xen_llong_to_C_llong(end);
  if ((nd - bg) >= mus_sound_data_length(sd))
    Xen_error(Xen_make_error_type("out-of-range"),
	      Xen_list_4(C_string_to_Xen_string(S_mus_sound_write ": end - beg (~A - ~A) >= sound-data array length, ~A"),
			 end, 
			 beg, 
			 C_llong_to_Xen_llong(mus_sound_data_length(sd))));

  nchans = mus_sound_data_chans(sd);
  bufs = (mus_float_t **)malloc(nchans * sizeof(mus_float_t *));
  for (i = 0; i < nchans; i++)
    bufs[i] = mus_sound_data_channel_data(sd, i);

  val = C_int_to_Xen_integer(mus_file_write(Xen_integer_to_C_int(fd), bg, nd, Xen_integer_to_C_int(chans), bufs));
  
  free(bufs);
  return(val);
}


static Xen g_mus_sound_seek_frample(Xen fd, Xen offset)
{
  #define H_mus_sound_seek_frample "(" S_mus_sound_seek_frample " fd frample): move the current read/write location in file fd \
to the frample offset"

  Xen_check_type(Xen_is_integer(fd), fd, 1, S_mus_sound_seek_frample, "an integer");
  Xen_check_type(Xen_is_llong(offset), offset, 2, S_mus_sound_seek_frample, "an integer");
  return(C_llong_to_Xen_llong(mus_file_seek_frample(Xen_integer_to_C_int(fd),
					    Xen_llong_to_C_llong(offset))));
}
#endif /* (!DISABLE_DEPRECATED) */


#define S_mus_sound_preload "mus-sound-preload"
static Xen g_mus_sound_preload(Xen file)
{
  #define H_mus_sound_preload "(" S_mus_sound_preload " filename): save filename's data in memory (faster opens and so on)."
  char *str;

  Xen_check_type(Xen_is_string(file), file, 1, S_mus_sound_preload, "a string");
  str = mus_expand_filename(Xen_string_to_C_string(file));

  if (str)
    {
      int i, ifd, chans;
      mus_float_t **bufs;
      mus_long_t framples;

      ifd = mus_sound_open_input(str);
      if (ifd != MUS_ERROR)
	{
	  chans = mus_sound_chans(str);
	  framples = mus_sound_framples(str) + 8; /* + 8 for readers than wander off the end */
	  bufs = (mus_float_t **)malloc(chans * sizeof(mus_float_t *));
	  for (i = 0; i < chans; i++)
	    bufs[i] = (mus_float_t *)malloc(framples * sizeof(mus_float_t));
      
	  mus_file_seek_frample(ifd, 0);
	  mus_file_read_file(ifd, 0, chans, framples, bufs);
	  mus_sound_set_saved_data(str, bufs);
	  mus_sound_close_input(ifd);
	}
      free(str);
    }
  return(file);
}


/* since mus-audio-read|write assume sound-data vectors communicating with Scheme,
 *   we can't assume that the user has had a chance to deal with type problems.
 *   So, we keep track of each audio line's current read|write format and
 *   translate in mus_audio_read|write if necessary
 */

static int audio_io_size = 0;
static int *audio_io_lines = NULL;
static int *audio_io_formats = NULL;

int mus_audio_io_format(int line);
int mus_audio_io_format(int line)
{
  int i;
  for (i = 0; i < audio_io_size; i++)
    if (audio_io_lines[i] == line)
      return(audio_io_formats[i]);
  return(MUS_UNKNOWN);
}


#define audio_io_read_format(Line) (mus_audio_io_format(Line) & 0xffff)
#define audio_io_write_format(Line) (mus_audio_io_format(Line) >> 16)

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
      audio_io_lines = (int *)malloc(audio_io_size * sizeof(int));
      audio_io_formats = (int *)malloc(audio_io_size * sizeof(int));
    }
  else
    {
      audio_io_lines = (int *)realloc(audio_io_lines, audio_io_size * sizeof(int));
      audio_io_formats = (int *)realloc(audio_io_formats, audio_io_size * sizeof(int));
    }
  for (i = old_size + 1; i < audio_io_size; i++)
    {
      audio_io_lines[i] = -1;
      audio_io_formats[i] = MUS_UNKNOWN;
    }
  audio_io_lines[old_size] = line;
  audio_io_formats[old_size] = format;
}


#define audio_io_set_read_format(Line, Format) audio_io_set_format(Line, ((mus_audio_io_format(Line) & 0xffff0000) | Format))
#define audio_io_set_write_format(Line, Format) audio_io_set_format(Line, ((mus_audio_io_format(Line) & 0xffff) | (Format << 16)))


static Xen g_mus_audio_open_output(Xen dev, Xen srate, Xen chans, Xen format, Xen size)
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

  Xen_check_type(Xen_is_integer(dev), dev, 1, S_mus_audio_open_output, "an integer");
  Xen_check_type(Xen_is_number(srate), srate, 2, S_mus_audio_open_output, "a number");
  Xen_check_type(Xen_is_integer(chans), chans, 3, S_mus_audio_open_output, "an integer");
  Xen_check_type(Xen_is_integer(format), format, 4, S_mus_audio_open_output, "an integer");
  Xen_check_type(Xen_is_number(size), size, 5, S_mus_audio_open_output, "a number");

  idev = Xen_integer_to_C_int(dev);
  israte = Xen_integer_to_C_int(srate);
  ichans = Xen_integer_to_C_int(chans);
  ifmt = Xen_integer_to_C_int(format);
  isize = Xen_integer_to_C_int(size);

  if (!(mus_is_data_format(ifmt)))
    Xen_out_of_range_error(S_mus_audio_open_output, 4, format, "invalid data format");
  if (isize < 0)
    Xen_out_of_range_error(S_mus_audio_open_output, 5, size, "size < 0?");
  if (israte <= 0)
    Xen_out_of_range_error(S_mus_audio_open_output, 2, srate, "srate <= 0?");
  if ((ichans <= 0) || (ichans > 256))
    Xen_out_of_range_error(S_mus_audio_open_output, 3, chans, "chans <= 0 or > 256?");

  line = mus_audio_open_output(idev, israte, ichans, ifmt, isize);
  audio_io_set_write_format(line, ifmt);
  return(C_int_to_Xen_integer(line));
}


static Xen g_mus_audio_open_input(Xen dev, Xen srate, Xen chans, Xen format, Xen size)
{
  #define H_mus_audio_open_input "(" S_mus_audio_open_input " device srate chans format bufsize): \
open the audio device ready for input with the indicated attributes; return the audio line number"

  int line, idev, ifmt, isize, israte, ichans;

  Xen_check_type(Xen_is_integer(dev), dev, 1, S_mus_audio_open_input, "an integer");
  Xen_check_type(Xen_is_number(srate), srate, 2, S_mus_audio_open_input, "a number");
  Xen_check_type(Xen_is_integer(chans), chans, 3, S_mus_audio_open_input, "an integer");
  Xen_check_type(Xen_is_integer(format), format, 4, S_mus_audio_open_input, "an integer");
  Xen_check_type(Xen_is_number(size), size, 5, S_mus_audio_open_input, "a number");

  idev = Xen_integer_to_C_int(dev);
  israte = Xen_integer_to_C_int(srate);
  ichans = Xen_integer_to_C_int(chans);
  ifmt = Xen_integer_to_C_int(format);
  isize = Xen_integer_to_C_int(size);

  if (!(mus_is_data_format(ifmt)))
    Xen_out_of_range_error(S_mus_audio_open_input, 4, format, "invalid data format");
  if (isize < 0)
    Xen_out_of_range_error(S_mus_audio_open_input, 5, size, "size < 0?");
  if (israte <= 0)
    Xen_out_of_range_error(S_mus_audio_open_input, 2, srate, "srate <= 0?");
  if (ichans <= 0)
    Xen_out_of_range_error(S_mus_audio_open_input, 3, chans, "chans <= 0?");

  line = mus_audio_open_input(idev, israte, ichans, ifmt, isize);
  audio_io_set_read_format(line, ifmt);
  return(C_int_to_Xen_integer(line));
}


static Xen g_mus_audio_close(Xen line)
{
  int res;
  #define H_mus_audio_close "(" S_mus_audio_close " line): close the audio hardware line"
  Xen_check_type(Xen_is_integer(line), line, 1, S_mus_audio_close, "an integer");
  res = (int)Xen_integer_to_C_int(line);
  if (res < 0)
    Xen_out_of_range_error(S_mus_audio_close, 1, line, "line < 0?");
  res = mus_audio_close(res);
  return(C_int_to_Xen_integer(res));
}


/* these take a sndlib buffer (sound_data) and handle the conversion to the interleaved char* internally */
/* so, they take "framples", not "bytes", and a sound_data object, not char* etc */

static Xen g_mus_audio_write(Xen line, Xen sdata, Xen framples, Xen start)
{
  #define H_mus_audio_write "(" S_mus_audio_write " line sdata framples (start 0)): write framples of data (channels * framples = samples) \
to the audio line from sound-data sdata."

#if 0
  char *obuf;
  sound_data *sd;
  int outbytes, val, fmt, fd, i, chans;
  mus_long_t frms, beg = 0;
  mus_float_t **bufs;

  Xen_check_type(Xen_is_integer(line), line, 1, S_mus_audio_write, "an integer");
  Xen_check_type(xen_is_sound_data(sdata), sdata, 2, S_mus_audio_write, "a sound-data object");
  Xen_check_type(Xen_is_llong(framples), framples, 3, S_mus_audio_write, "an integer");
  Xen_check_type(Xen_is_llong(start) || !Xen_is_bound(start), start, 4, S_mus_audio_write, "an integer");

  sd = Xen_to_sound_data(sdata);
  frms = Xen_llong_to_C_llong(framples);

  if (frms > mus_sound_data_length(sd))
    Xen_out_of_range_error(S_mus_audio_write, 3, framples, "framples > sound-data buffer length");

  if (Xen_is_bound(start))
    beg = Xen_llong_to_C_llong(start);

  chans = mus_sound_data_chans(sd);

  fd = Xen_integer_to_C_int(line);
  fmt = audio_io_write_format(fd);
  outbytes = frms * chans * mus_bytes_per_sample(fmt);
  obuf = (char *)calloc(outbytes, sizeof(char));

  bufs = (mus_float_t **)malloc(chans * sizeof(mus_float_t *));
  for (i = 0; i < chans; i++)
    bufs[i] = mus_sound_data_channel_data(sd, i);

  mus_file_write_buffer(fmt, beg, beg + frms - 1, chans, bufs, obuf, true); /* true -> clipped */
  val = mus_audio_write(fd, obuf, outbytes);
  free(obuf);
  free(bufs);

  return(C_int_to_Xen_integer(val));
#else
  return(Xen_false);
#endif
}


static Xen g_mus_audio_read(Xen line, Xen sdata, Xen framples)
{
  #define H_mus_audio_read "(" S_mus_audio_read " line sdata framples): read framples of data (channels * framples = samples) \
from the audio line into sound-data sdata."

#if 0
  char *inbuf;
  sound_data *sd;
  int val, inbytes, fd, fmt, i, chans;
  mus_long_t frms;
  mus_float_t **bufs;

  Xen_check_type(Xen_is_integer(line), line, 1, S_mus_audio_read, "an integer");
  Xen_check_type(xen_is_sound_data(sdata), sdata, 2, S_mus_audio_read, "a sound-data object");
  Xen_check_type(Xen_is_llong(framples), framples, 3, S_mus_audio_read, "an integer");

  sd = Xen_to_sound_data(sdata);
  frms = Xen_llong_to_C_llong(framples);
  fd = Xen_integer_to_C_int(line);

  chans = mus_sound_data_chans(sd);
  fmt = audio_io_read_format(fd);
  inbytes = frms * chans * mus_bytes_per_sample(fmt);
  inbuf = (char *)calloc(inbytes, sizeof(char));
  val = mus_audio_read(fd, inbuf, inbytes);

  bufs = (mus_float_t **)malloc(chans * sizeof(mus_float_t *));
  for (i = 0; i < chans; i++)
    bufs[i] = mus_sound_data_channel_data(sd, i);

  mus_file_read_buffer(fmt, 0, chans, frms, bufs, inbuf); /* frms here because this is "nints" not "end"! */
  free(inbuf);
  free(bufs);

  return(C_int_to_Xen_integer(val));
#else
  return(Xen_false);
#endif
}


/* global default clipping values */

static Xen g_mus_clipping(void)
{
  #define H_mus_clipping "(" S_mus_clipping "): whether sound data written to a file should be clipped"
  return(C_bool_to_Xen_boolean(mus_clipping()));
}


static Xen g_mus_set_clipping(Xen clipped)
{
  Xen_check_type(Xen_is_boolean(clipped), clipped, 1, S_setB S_mus_clipping, "a boolean");
  return(C_bool_to_Xen_boolean(mus_set_clipping(Xen_boolean_to_C_bool(clipped))));
}


/* file local clipping values */

static Xen g_mus_file_clipping(Xen fd)
{
  #define H_mus_file_clipping "(" S_mus_file_clipping " fd): whether sound data written to file 'fd' should be clipped"
  Xen_check_type(Xen_is_integer(fd), fd, 1, S_mus_file_clipping, "an integer");
  return(C_bool_to_Xen_boolean(mus_file_clipping(Xen_integer_to_C_int(fd))));
}


static Xen g_mus_file_set_clipping(Xen fd, Xen clipped)
{
  Xen_check_type(Xen_is_integer(fd), fd, 1, S_setB S_mus_file_clipping, "an integer");
  Xen_check_type(Xen_is_boolean(clipped), clipped, 2, S_setB S_mus_file_clipping, "a boolean");
  return(C_bool_to_Xen_boolean(mus_file_set_clipping(Xen_integer_to_C_int(fd), Xen_boolean_to_C_bool(clipped))));
}



Xen g_mus_expand_filename(Xen file)
{
  #define H_mus_expand_filename "(" S_mus_expand_filename " name): expand 'name' into a canonical or absolute filename, that is, \
one in which all directories in the path are explicit."
  char *str = NULL;
  Xen result;

  Xen_check_type(Xen_is_string(file), file, 1, S_mus_expand_filename, "a string");
  str = mus_expand_filename(Xen_string_to_C_string(file));
  result = C_string_to_Xen_string(str);
  if (str) free(str);
  return(result);
}


static Xen g_mus_sound_report_cache(Xen file)
{
  #define H_mus_sound_report_cache "(" S_mus_sound_report_cache " (name)): print the current sound \
cache info to the file given or stdout"
  FILE *fd;
  const char *name;
  char *str = NULL;

  if (!Xen_is_bound(file))
    {
      mus_sound_report_cache(stdout);
      return(Xen_false);
    }

  Xen_check_type(Xen_is_string(file), file, 1, S_mus_sound_report_cache, "a string");
  name = Xen_string_to_C_string(file);
  if (name)
    {
      str = mus_expand_filename(name);
      if (str)
	{
	  fd = FOPEN(str, "w");
	  free(str);
	  if (fd)
	    {
	      mus_sound_report_cache(fd);
	      FCLOSE(fd, name);
	      return(file);
	    }
	}
    }

  Xen_error(Xen_make_error_type("cannot-save"),
	    Xen_list_3(C_string_to_Xen_string(S_mus_sound_report_cache ": ~S ~A"),
		       file,
		       C_string_to_Xen_string(STRERROR(errno))));
  return(Xen_false);
}


static Xen g_mus_error_type_to_string(Xen err)
{
  #define H_mus_error_type_to_string "(" S_mus_error_type_to_string " err): string description of err (a sndlib error type)"
  Xen_check_type(Xen_is_integer(err), err, 1, S_mus_error_type_to_string, "an integer");
  return(C_string_to_Xen_string((char *)mus_error_type_to_string(Xen_integer_to_C_int(err))));
}


static Xen g_array_to_file(Xen filename, Xen data, Xen len, Xen srate, Xen channels)
{
  #define H_array_to_file "(" S_array_to_file " filename data len srate channels): write 'data', \
a " S_vct " of interleaved samples, to the sound file 'filename' set up to have the given \
srate and channels.  'len' samples are written."

  /* this exists for compatibility with the Common Lisp version of CLM. Ideally, we'd get rid
   *   of it and provide vct<->file and sound-data<->file instead so that the channels aren't
   *   handled through interleaving.  But that means extensive changes to the Lisp code...
   */

  mus_long_t olen, samps;
  vct *v;

  Xen_check_type(Xen_is_string(filename), filename, 1, S_array_to_file, "a string");
  Xen_check_type(mus_is_vct(data), data, 2, S_array_to_file, "a " S_vct);
  Xen_check_type(Xen_is_llong(len), len, 3, S_array_to_file, "an integer");
  Xen_check_type(Xen_is_integer(srate), srate, 4, S_array_to_file, "an integer");
  Xen_check_type(Xen_is_integer(channels), channels, 5, S_array_to_file, "an integer");

  v = Xen_to_vct(data);
  samps = Xen_llong_to_C_llong(len);
  if (samps <= 0)
    Xen_out_of_range_error(S_array_to_file, 3, len, "samples <= 0?");
  if (samps > mus_vct_length(v))
    samps = mus_vct_length(v);

  olen = mus_float_array_to_file(Xen_string_to_C_string(filename),
				 mus_vct_data(v),
				 samps,
				 Xen_integer_to_C_int(srate),
				 Xen_integer_to_C_int(channels));
  return(C_llong_to_Xen_llong(olen));
}


static Xen g_file_to_array(Xen filename, Xen chan, Xen start, Xen samples, Xen data)
{
  #define H_file_to_array "(" S_file_to_array " filename chan start samples data): read the sound file \
'filename' placing samples from channel 'chan' into the " S_vct " 'data' starting in the file \
at frample 'start' and reading 'samples' samples altogether."

  int chn;
  mus_long_t samps;
  vct *v;
  const char *name = NULL;

  Xen_check_type(Xen_is_string(filename), filename, 1, S_file_to_array, "a string");
  Xen_check_type(Xen_is_integer(chan), chan, 2, S_file_to_array, "an integer");
  Xen_check_type(Xen_is_llong(start), start, 3, S_file_to_array, "an integer");
  Xen_check_type(Xen_is_llong(samples), samples, 4, S_file_to_array, "an integer");
  Xen_check_type((mus_is_vct(data)), data, 5, S_file_to_array, "a " S_vct);

  name = Xen_string_to_C_string(filename);
  if (!(mus_file_probe(name)))
    Xen_error(NO_SUCH_FILE,
	      Xen_list_3(C_string_to_Xen_string(S_file_to_array ": ~S ~A"),
			 filename,
			 C_string_to_Xen_string(STRERROR(errno))));

  v = Xen_to_vct(data);

  samps = Xen_llong_to_C_llong(samples);
  if (samps <= 0) 
    Xen_out_of_range_error(S_file_to_array, 4, samples, "samples <= 0?");
  if (samps > mus_vct_length(v))
    samps = mus_vct_length(v);

  chn = Xen_integer_to_C_int(chan);
  if ((chn < 0) || (chn > mus_sound_chans(name)))
    Xen_error(NO_SUCH_CHANNEL,
	      Xen_list_4(C_string_to_Xen_string(S_file_to_array ": invalid chan: ~A, ~S has ~A chans"),
			 chan,
			 filename,
			 C_int_to_Xen_integer(mus_sound_chans(name))));

  if (mus_sound_chans(name) <= 0)
    Xen_error(BAD_HEADER,
	      Xen_list_2(C_string_to_Xen_string(S_file_to_array ": ~S chans <= 0"),
			 filename));

  mus_file_to_float_array(name, chn, Xen_llong_to_C_llong(start), samps, mus_vct_data(v));
  return(data);
}


static Xen new_sound_hook;

static void g_new_sound_hook(const char *filename)
{
  if (Xen_hook_has_list(new_sound_hook))
    {
#if HAVE_SCHEME
      s7_call(s7, new_sound_hook, s7_cons(s7, C_string_to_Xen_string(filename), Xen_empty_list));
#else
      Xen procs, fname;
      fname = C_string_to_Xen_string(filename);
      procs = Xen_hook_list(new_sound_hook);
      while (!Xen_is_null(procs))
	{
	  Xen_call_with_1_arg(Xen_car(procs), fname, S_new_sound_hook);
	  procs = Xen_cdr(procs);
	}
#endif
    }
}


#if HAVE_OSS
#define S_mus_audio_reinitialize "mus-audio-reinitialize"
static Xen g_mus_audio_reinitialize(void)
{
  #define H_mus_audio_reinitialize "(" S_mus_audio_reinitialize "): force audio device re-initialization"
  return(C_int_to_Xen_integer(mus_audio_reinitialize()));
}
#endif


#if (!DISABLE_DEPRECATED)
#if (!HAVE_SCHEME)
static Xen_object_type_t sound_data_tag = 0;

bool xen_is_sound_data(Xen obj) {return(Xen_c_object_is_type(obj, sound_data_tag));}

static Xen g_is_sound_data(Xen obj) 
{
  #define H_is_sound_data "(" S_is_sound_data " obj): is 'obj' is a sound-data object"
  return(C_bool_to_Xen_boolean(xen_is_sound_data(obj)));
}


Xen_wrap_free(sound_data, free_sound_data, sound_data_free)


static char *sound_data_to_string(sound_data *sd)
{
  char *buf;
  int chans; /* len=print length */
  mus_long_t len, sdlen;
  char flt[24];

  if (sd == NULL) return(NULL);

  len = mus_vct_print_length();
  sdlen = mus_sound_data_length(sd);
  if (len > sdlen) len = sdlen;

  chans = mus_sound_data_chans(sd);
  buf = (char *)calloc(64 + len * 24 * chans, sizeof(char));

  sprintf(buf, "#<sound-data[chans=%d, length=%lld]:", chans, sdlen);
  if (len > 0)
    {
      int i, chn;
      for (chn = 0; chn < chans; chn++)
	{
	  mus_float_t *d;
	  d = mus_sound_data_channel_data(sd, chn);
	  snprintf(flt, 24, "\n    (%.3f", d[0]);
	  strcat(buf, flt);
	  for (i = 1; i < len; i++)
	    {
	      snprintf(flt, 24, " %.3f", d[i]);
	      strcat(buf, flt);
	    }
	  if (sdlen > mus_vct_print_length())
	    strcat(buf, " ...");
	  strcat(buf, ")");
	}
    }
  strcat(buf, ">");
  return(buf);
}

Xen_wrap_print(sound_data, print_sound_data, sound_data_to_string)


static bool sound_data_equalp(sound_data *sd1, sound_data *sd2)
{
  if (sd1 == sd2) return(true);
  if ((sd1) && (sd2) &&
      (mus_sound_data_chans(sd1) == mus_sound_data_chans(sd2)) &&
      (mus_sound_data_length(sd1) == mus_sound_data_length(sd2)))
    {
      int chn, chans;
      chans = mus_sound_data_chans(sd1);
      for (chn = 0; chn < chans; chn++)
	if (!(mus_arrays_are_equal(mus_sound_data_channel_data(sd1, chn), 
				   mus_sound_data_channel_data(sd2, chn),
				   mus_float_equal_fudge_factor(),
				   mus_sound_data_length(sd1))))
	  return(false);
      return(true);
    }
  return(false);
}


static Xen equalp_sound_data(Xen obj1, Xen obj2)
{
#if HAVE_RUBY || HAVE_FORTH
  if ((!(xen_is_sound_data(obj1))) || (!(xen_is_sound_data(obj2)))) return(Xen_false);
#endif
  return(C_bool_to_Xen_boolean(sound_data_equalp(Xen_to_sound_data(obj1), Xen_to_sound_data(obj2))));
}

static Xen g_sound_data_length(Xen obj)
{
  #define H_sound_data_length "(" S_sound_data_length " sd): length (in samples) of each channel of sound-data sd"
  sound_data *sd;
  Xen_check_type(xen_is_sound_data(obj), obj, 1, S_sound_data_length, "a sound-data object");
  sd = Xen_to_sound_data(obj);
  return(C_llong_to_Xen_llong(mus_sound_data_length(sd)));
}


static Xen g_sound_data_chans(Xen obj)
{
  #define H_sound_data_chans "(" S_sound_data_chans " sd): number of channels in sound-data sd"
  sound_data *sd;
  Xen_check_type(xen_is_sound_data(obj), obj, 1, S_sound_data_chans, "a sound-data object");
  sd = Xen_to_sound_data(obj);
  return(C_int_to_Xen_integer(mus_sound_data_chans(sd)));
}

static Xen make_sound_data(int chans, mus_long_t framples)
{
  #define H_make_sound_data "(" S_make_sound_data " chans framples): return a new sound-data object with 'chans' channels, each having 'framples' samples"
  sound_data *sd;
  sd = c_make_sound_data(chans, framples);
  return(Xen_make_object(sound_data_tag, sd, 0, free_sound_data));
}


Xen wrap_sound_data(int chans, mus_long_t framples, mus_float_t **data)
{
  sound_data *sd;
  sd = (sound_data *)malloc(sizeof(sound_data));
  sd->length = framples;
  sd->chans = chans;
  sd->wrapped = true;
  sd->data = data;
  return(Xen_make_object(sound_data_tag, sd, 0, free_sound_data));
}


static Xen g_make_sound_data(Xen chans, Xen framples)
{
  int chns;
  mus_long_t frms;

  Xen_check_type(Xen_is_integer(chans), chans, 1, S_make_sound_data, "an integer");
  Xen_check_type(Xen_is_llong(framples), framples, 2, S_make_sound_data, "an integer");

  chns = Xen_integer_to_C_int(chans);
  if (chns <= 0)
    Xen_out_of_range_error(S_make_sound_data, 1, chans, "chans <= 0?");
  if (chns > (1 << 26))
    Xen_out_of_range_error(S_make_sound_data, 1, chans, "chans arg too large");

  frms = Xen_llong_to_C_llong(framples);
  if (frms <= 0)
    Xen_out_of_range_error(S_make_sound_data, 2, framples, "framples <= 0?");
  if ((frms > mus_max_malloc()) ||
      (((mus_long_t)(frms * sizeof(mus_float_t))) > mus_max_malloc()))
    Xen_out_of_range_error(S_make_sound_data, 2, framples, "framples arg too large (see mus-max-malloc)");

  return(make_sound_data(chns, frms));
}


static Xen g_sound_data_ref(Xen obj, Xen chan, Xen frample_num)
{
  #define H_sound_data_ref "(" S_sound_data_ref " sd chan i): sample in channel chan at location i of sound-data sd: sd[chan][i]"
  sound_data *sd;
  mus_long_t loc;
  int chn;

  Xen_check_type(xen_is_sound_data(obj), obj, 1, S_sound_data_ref, "a sound-data object");
  Xen_check_type(Xen_is_integer(chan), chan, 2, S_sound_data_ref, "an integer");
  Xen_check_type(Xen_is_llong(frample_num), frample_num, 3, S_sound_data_ref, "an integer");

  sd = Xen_to_sound_data(obj);

  chn = Xen_integer_to_C_int(chan);
  if (chn < 0)
    Xen_out_of_range_error(S_sound_data_ref, 2, chan, "invalid channel");
  if (chn >= mus_sound_data_chans(sd))
    Xen_error(Xen_make_error_type("out-of-range"),
	      Xen_list_3(C_string_to_Xen_string(S_sound_data_ref ": chan: ~A >= sound-data chans, ~A"),
			 chan, 
			 C_int_to_Xen_integer(mus_sound_data_chans(sd))));

  loc = Xen_llong_to_C_llong(frample_num);
  if (loc < 0)
    Xen_out_of_range_error(S_sound_data_ref, 3, frample_num, "invalid frample");
  if (loc >= mus_sound_data_length(sd))
    Xen_error(Xen_make_error_type("out-of-range"),
	      Xen_list_3(C_string_to_Xen_string(S_sound_data_ref ": frample: ~A >= sound-data length, ~A"),
			 frample_num, 
			 C_llong_to_Xen_llong(mus_sound_data_length(sd))));

  return(C_double_to_Xen_real(mus_sound_data_ref(sd, chn, loc)));
}

#else /* scheme */

bool xen_is_sound_data(Xen obj) 
{
  return((s7_is_float_vector(obj)) &&
	 (s7_vector_rank(obj) == 2));
}


Xen wrap_sound_data(int chans, mus_long_t framples, mus_float_t **data)
{
  /* data here is not necessarily continguous (dac_buffers, io_state)
   */
  s7_Int di[1];
  int i;
  s7_pointer lst;
  di[0] = framples;
  lst = s7_nil(s7);
  for (i = chans - 1; i >= 0; i--)
    lst = s7_cons(s7, s7_make_float_vector_wrapper(s7, framples, (s7_Double *)data[i], 1, di), lst);
  return(lst);
}

#endif


Xen g_sound_data_maxamp(Xen obj)
{
  #define H_sound_data_maxamp "(" S_sound_data_maxamp " sd): list of maxamps of data in sd"
  sound_data *sd;
  int i, chans;
  mus_long_t j, len;

  Xen lst = Xen_empty_list;
  Xen_check_type(xen_is_sound_data(obj), obj, 1, S_sound_data_maxamp, "a sound-data object");

  sd = Xen_to_sound_data(obj);
  chans = mus_sound_data_chans(sd);
  len = mus_sound_data_length(sd);

  for (i = chans - 1; i >= 0; i--)
    {
      mus_float_t mx;
      mus_float_t *buf;
      mx = -10000.0;
      buf = mus_sound_data_channel_data(sd, i);
      for (j = 0; j < len; j++)
	{
	  mus_float_t x;
	  x = fabs(buf[j]);
	  if (x > mx) mx = x;
	}
      lst = Xen_cons(C_double_to_Xen_real(mx), lst);
    }
  return(lst);
}


#if (!HAVE_SCHEME)
static mus_float_t sound_data_peak(sound_data *sd)
{
  int chans, chn;
  mus_long_t i, len;
  mus_float_t mx = 0.0;

  chans = mus_sound_data_chans(sd);
  len = mus_sound_data_length(sd);

  for (chn = 0; chn < chans; chn++)
    {
      mus_float_t *d;
      d = mus_sound_data_channel_data(sd, chn);
      for (i = 0; i < len; i++) 
	if (fabs(d[i]) > mx)
	  mx = fabs(d[i]);
    }
  return(mx);
}


static Xen g_sound_data_peak(Xen obj)
{
  #define H_sound_data_peak "(" S_sound_data_peak " sd): overall maxamp of data in sd"
  Xen_check_type(xen_is_sound_data(obj), obj, 1, S_sound_data_maxamp, "a sound-data object");
  return(C_double_to_Xen_real(sound_data_peak(Xen_to_sound_data(obj))));
}


#if HAVE_FORTH
static Xen sound_data_apply(Xen obj, Xen chan, Xen i)
{
  return(g_sound_data_ref(obj, chan, i));
}
#endif


static Xen g_sound_data_set(Xen obj, Xen chan, Xen frample_num, Xen val)
{
  #define H_sound_data_setB "(" S_sound_data_setB " sd chan i val): set sound-data sd's i-th element in channel chan to val: sd[chan][i] = val"
  sound_data *sd;
  int chn;
  mus_long_t loc;
  mus_float_t *d;

  Xen_check_type(xen_is_sound_data(obj), obj, 1, S_sound_data_setB, "a sound-data object");
  Xen_check_type(Xen_is_integer(chan), chan, 2, S_sound_data_setB, "an integer");
  Xen_check_type(Xen_is_llong(frample_num), frample_num, 3, S_sound_data_setB, "an integer");
  Xen_check_type(Xen_is_number(val), val, 4, S_sound_data_setB, "a number");

  sd = Xen_to_sound_data(obj);

  chn = Xen_integer_to_C_int(chan);
  if (chn < 0)
    Xen_out_of_range_error(S_sound_data_setB, 2, chan, "invalid channel");
  if (chn >= mus_sound_data_chans(sd))
    Xen_error(Xen_make_error_type("out-of-range"),
	      Xen_list_3(C_string_to_Xen_string(S_sound_data_setB ": chan: ~A >= sound-data chans, ~A"),
			 chan, 
			 C_int_to_Xen_integer(mus_sound_data_chans(sd))));

  loc = Xen_llong_to_C_llong(frample_num);
  if (loc < 0)
    Xen_out_of_range_error(S_sound_data_setB, 3, frample_num, "invalid frample");
  if (loc >= mus_sound_data_length(sd))
    Xen_error(Xen_make_error_type("out-of-range"),
	      Xen_list_3(C_string_to_Xen_string(S_sound_data_setB ": frample: ~A >= sound-data length, ~A"),
			 frample_num, 
			 C_llong_to_Xen_llong(mus_sound_data_length(sd))));

  d = mus_sound_data_channel_data(sd, chn);
  d[loc] = Xen_real_to_C_double(val);
  return(val);
}


static void sound_data_scale(sound_data *sd, mus_float_t scaler)
{
  int chans, chn;
  mus_long_t i, len;

  chans = mus_sound_data_chans(sd);
  len = mus_sound_data_length(sd);

  if (scaler == 0.0)
    {
      for (chn = 0; chn < chans; chn++)
	mus_clear_array(mus_sound_data_channel_data(sd, chn), len);
    }
  else
    {
      if (scaler != 1.0)
	for (chn = 0; chn < chans; chn++)
	  {
	    mus_float_t *d;
	    d = mus_sound_data_channel_data(sd, chn);
	    for (i = 0; i < len; i++) 
	      d[i] *= scaler;
	  }
    }
}


static Xen g_sound_data_scaleB(Xen sdobj, Xen scl)
{
  #define H_sound_data_scaleB "(" S_sound_data_scaleB " sd scl): scales (multiplies) sound-data sd's data by scl"

  Xen_check_type(xen_is_sound_data(sdobj), sdobj, 1, S_sound_data_scaleB, "a sound-data object");
  Xen_check_type(Xen_is_number(scl), scl, 2, S_sound_data_scaleB, "a number");

  sound_data_scale(Xen_to_sound_data(sdobj), Xen_real_to_C_double(scl));
  return(sdobj);
}


static sound_data *sound_data_fill(sound_data *sd, mus_float_t scaler)
{
  int chans, chn;
  mus_long_t i, len;

  chans = mus_sound_data_chans(sd);
  len = mus_sound_data_length(sd);

  if (scaler == 0.0)
    {
      for (chn = 0; chn < chans; chn++)
	mus_clear_array(mus_sound_data_channel_data(sd, chn), len);
    }
  else
    {
      for (chn = 0; chn < chans; chn++)
	{
	  mus_float_t *d;
	  d = mus_sound_data_channel_data(sd, chn);
	  for (i = 0; i < len; i++) 
	    d[i] = scaler;
	}
    }
  return(sd);
}


static Xen g_sound_data_fillB(Xen sdobj, Xen scl)
{
  #define H_sound_data_fillB "(" S_sound_data_fillB " sd value): fills the sound-data object sd with value"

  Xen_check_type(xen_is_sound_data(sdobj), sdobj, 1, S_sound_data_fillB, "a sound-data object");
  Xen_check_type(Xen_is_number(scl), scl, 2, S_sound_data_fillB, "a number");

  sound_data_fill(Xen_to_sound_data(sdobj), Xen_real_to_C_double(scl));
  return(sdobj);
}

static Xen g_sound_data_copy(Xen obj)
{
  sound_data *sdnew;
  #define H_sound_data_copy "(" S_sound_data_copy " sd): returns a copy of the sound-data object sd"

  Xen_check_type(xen_is_sound_data(obj), obj, 1, S_sound_data_copy, "a sound-data object");

  sdnew = sound_data_copy(Xen_to_sound_data(obj));
  return(Xen_make_object(sound_data_tag, sdnew, 0, free_sound_data));
}


static void sound_data_reverse(sound_data *sd)
{
  int chn, chans;
  mus_long_t i, j, len;

  chans = mus_sound_data_chans(sd);
  len = mus_sound_data_length(sd);

  for (chn = 0; chn < chans; chn++)
    {
      mus_float_t *d;
      d = mus_sound_data_channel_data(sd, chn);
      for (i = 0, j = len - 1; i < j; i++, j--)
	{
	  mus_float_t tmp;
	  tmp = d[i];
	  d[i] = d[j];
	  d[j] = tmp;
	}
    }
}


static Xen g_sound_data_reverseB(Xen obj)
{
  #define H_sound_data_reverseB "(" S_sound_data_reverseB " sd): reverses the elements (within each channel) of sound-data object sd"

  Xen_check_type(xen_is_sound_data(obj), obj, 1, S_sound_data_reverseB, "a sound-data object");

  sound_data_reverse(Xen_to_sound_data(obj));
  return(obj);
}


static void sound_data_add(sound_data *sd1, sound_data *sd2)
{
  int i, chns;
  mus_long_t j, len;

  chns = mus_sound_data_chans(sd1);
  if (chns > mus_sound_data_chans(sd2)) chns = mus_sound_data_chans(sd2);
  len = mus_sound_data_length(sd1);
  if (len > mus_sound_data_length(sd2)) len = mus_sound_data_length(sd2);

  for (i = 0; i < chns; i++)
    {
      mus_float_t *d1, *d2;
      d1 = mus_sound_data_channel_data(sd1, i);
      d2 = mus_sound_data_channel_data(sd2, i);
      for (j = 0; j < len; j++)
	d1[j] += d2[j];
    }
}


static Xen g_sound_data_addB(Xen obj1, Xen obj2)
{
  #define H_sound_data_addB "(" S_sound_data_addB " sd1 sd2): adds (element-wise) sd2 to sd1, returning sd1"

  Xen_check_type(xen_is_sound_data(obj1), obj1, 1, S_sound_data_addB, "a sound-data object");
  Xen_check_type(xen_is_sound_data(obj2), obj2, 2, S_sound_data_addB, "a sound-data object");

  sound_data_add(Xen_to_sound_data(obj1), Xen_to_sound_data(obj2));
  return(obj1);
}


static void sound_data_multiply(sound_data *sd1, sound_data *sd2)
{
  int i, chns;
  mus_long_t j, len;

  chns = mus_sound_data_chans(sd1);
  if (chns > mus_sound_data_chans(sd2)) chns = mus_sound_data_chans(sd2);
  len = mus_sound_data_length(sd1);
  if (len > mus_sound_data_length(sd2)) len = mus_sound_data_length(sd2);

  for (i = 0; i < chns; i++)
    {
      mus_float_t *d1, *d2;
      d1 = mus_sound_data_channel_data(sd1, i);
      d2 = mus_sound_data_channel_data(sd2, i);
      for (j = 0; j < len; j++)
	d1[j] *= d2[j];
    }
}


static Xen g_sound_data_multiplyB(Xen obj1, Xen obj2)
{
  #define H_sound_data_multiplyB "(" S_sound_data_multiplyB " sd1 sd2): multiplies (element-wise) sd1 by sd2, returning sd1"

  Xen_check_type(xen_is_sound_data(obj1), obj1, 1, S_sound_data_multiplyB, "a sound-data object");
  Xen_check_type(xen_is_sound_data(obj2), obj2, 2, S_sound_data_multiplyB, "a sound-data object");

  sound_data_multiply(Xen_to_sound_data(obj1), Xen_to_sound_data(obj2));
  return(obj1);
}


static void sound_data_offset(sound_data *sd, mus_float_t off)
{
  if (off != 0.0)
    {
      int i, chans;
      mus_long_t j, len;

      len = mus_sound_data_length(sd);
      chans = mus_sound_data_chans(sd);

      for (i = 0; i < chans; i++)
	{
	  mus_float_t *d;
	  d = mus_sound_data_channel_data(sd, i);
	  for (j = 0; j < len; j++)
	    d[j] += off;
	}
    }
}


static Xen g_sound_data_offsetB(Xen obj, Xen offset)
{
  #define H_sound_data_offsetB "(" S_sound_data_offsetB " sd val): adds val to each element of sd, returning sd"

  Xen_check_type(xen_is_sound_data(obj), obj, 1, S_sound_data_offsetB, "a sound-data object");
  Xen_check_type(Xen_is_number(offset), offset, 2, S_sound_data_offsetB, "a number");

  sound_data_offset(Xen_to_sound_data(obj), Xen_real_to_C_double(offset));
  return(obj);
}


static Xen g_sound_data_add(Xen obj1, Xen obj2)
{
  #define H_sound_data_add "(" S_sound_data_add " obj1 obj2): adds obj1 to obj2, either or both of which can be sound-data objects"

  Xen_check_type(xen_is_sound_data(obj1) || Xen_is_number(obj1), obj1, 1, S_sound_data_add, "a sound-data object or a number");
  Xen_check_type(xen_is_sound_data(obj2) || Xen_is_number(obj2), obj2, 2, S_sound_data_add, "a sound-data object or a number");

  if (xen_is_sound_data(obj1))
    {
      if (xen_is_sound_data(obj2))
	return(g_sound_data_addB(obj1, obj2));
      return(g_sound_data_offsetB(obj1, obj2));
    }
  if (xen_is_sound_data(obj2))
    return(g_sound_data_offsetB(obj2, obj1));
  return(C_double_to_Xen_real(Xen_real_to_C_double(obj1) + Xen_real_to_C_double(obj2)));
}


static Xen g_sound_data_multiply(Xen obj1, Xen obj2)
{
  #define H_sound_data_multiply "(" S_sound_data_multiply " obj1 obj2): multiplies obj1 by obj2, either or both of which can be sound-data objects"

  Xen_check_type(xen_is_sound_data(obj1) || Xen_is_number(obj1), obj1, 1, S_sound_data_multiply, "a sound-data object or a number");
  Xen_check_type(xen_is_sound_data(obj2) || Xen_is_number(obj2), obj2, 2, S_sound_data_multiply, "a sound-data object or a number");

  if (xen_is_sound_data(obj1))
    {
      if (xen_is_sound_data(obj2))
	return(g_sound_data_multiplyB(obj1, obj2));
      return(g_sound_data_scaleB(obj1, obj2));
    }
  if (xen_is_sound_data(obj2))
    return(g_sound_data_scaleB(obj2, obj1));
  return(C_double_to_Xen_real(Xen_real_to_C_double(obj1) * Xen_real_to_C_double(obj2)));
}


static Xen g_sound_data_to_vct(Xen sdobj, Xen chan, Xen vobj)
{
  #define H_sound_data_to_vct "(" S_sound_data_to_vct " sd chan v): copies sound-data sd's channel chan data into the " S_vct " v"
  vct *v;
  sound_data *sd;
  int chn;
  mus_long_t len, sdlen;

  Xen_check_type(xen_is_sound_data(sdobj), sdobj, 1, S_sound_data_to_vct, "a sound-data object");
  Xen_check_type(Xen_is_integer_or_unbound(chan), chan, 2, S_sound_data_to_vct, "an integer");
  Xen_check_type(!Xen_is_bound(vobj) || mus_is_vct(vobj), vobj, 3, S_sound_data_to_vct, "a " S_vct);

  sd = Xen_to_sound_data(sdobj);
  sdlen = mus_sound_data_length(sd);

  chn = (Xen_is_integer(chan)) ? Xen_integer_to_C_int(chan) : 0;
  if (chn < 0)
    Xen_out_of_range_error(S_sound_data_to_vct, 2, chan, "invalid channel");
  if (chn >= mus_sound_data_chans(sd))
    Xen_error(Xen_make_error_type("out-of-range"),
	      Xen_list_3(C_string_to_Xen_string(S_sound_data_to_vct ": chan: ~A >= sound-data chans, ~A"),
			 chan, 
			 C_int_to_Xen_integer(mus_sound_data_chans(sd))));

  if (!(mus_is_vct(vobj))) 
    vobj = xen_make_vct(sdlen, (mus_float_t *)calloc(sdlen, sizeof(mus_float_t)));
  v = Xen_to_vct(vobj);

  if (sdlen < mus_vct_length(v)) 
    len = sdlen; 
  else len = mus_vct_length(v);

  memcpy((void *)(mus_vct_data(v)), (void *)(mus_sound_data_channel_data(sd, chn)), len * sizeof(mus_float_t));

  return(vobj);
}


static Xen g_vct_to_sound_data(Xen vobj, Xen sdobj, Xen chan)
{
  #define H_vct_to_sound_data "(" S_vct_to_sound_data " v sd chan): copies " S_vct "'s data into sound-data sd's channel chan"
  vct *v;
  sound_data *sd;
  Xen obj = Xen_false;
  int chn;
  mus_long_t len;

  Xen_check_type(mus_is_vct(vobj), vobj, 1, S_vct_to_sound_data, "a " S_vct);
  Xen_check_type(!Xen_is_bound(sdobj) || xen_is_sound_data(sdobj), sdobj, 2, S_vct_to_sound_data, "a sound-data object");
  Xen_check_type(Xen_is_integer_or_unbound(chan), chan, 3, S_vct_to_sound_data, "an integer");

  v = Xen_to_vct(vobj);
  chn = (Xen_is_integer(chan)) ? Xen_integer_to_C_int(chan) : 0;
  if (chn < 0)
    Xen_out_of_range_error(S_vct_to_sound_data, 3, chan, "invalid channel");
  if (!(xen_is_sound_data(sdobj)))
    {
      if (chn > 0)
	Xen_out_of_range_error(S_vct_to_sound_data, 3, chan, "invalid channel");
      obj = make_sound_data(1, mus_vct_length(v));
      sd = Xen_to_sound_data(obj);
    }
  else
    {
      sd = Xen_to_sound_data(sdobj);
      if (chn >= mus_sound_data_chans(sd))
	Xen_error(Xen_make_error_type("out-of-range"),
		  Xen_list_3(C_string_to_Xen_string(S_vct_to_sound_data ": chan: ~A >= sound-data chans, ~A"),
			     chan, 
			     C_int_to_Xen_integer(mus_sound_data_chans(sd))));
      obj = sdobj;
    }
  if (mus_sound_data_length(sd) < mus_vct_length(v)) 
    len = mus_sound_data_length(sd); 
  else len = mus_vct_length(v);
  memcpy((void *)(mus_sound_data_channel_data(sd, chn)), (void *)(mus_vct_data(v)), len * sizeof(mus_float_t));
  return(obj);
}
#endif


#if HAVE_FORTH
#define S_sound_data_to_vector          "sound-data->vector"

static Xen g_sound_data_to_vector(Xen sdata)
{
#define H_sound_data_to_vector "(" S_sound_data_to_vector " sd):  \
returns a vector of chans dimensions containing all channels of sound-data sd as vct."
  int chn, chans;
  sound_data *sd;
  FTH vec;

  Xen_check_type(xen_is_sound_data(sdata), sdata, 1, S_sound_data_to_vector, "a sound-data object");
  sd = Xen_to_sound_data(sdata);
  chans = mus_sound_data_chans(sd);
  vec = Xen_make_vector(chans, FTH_NIL);

  for (chn = 0; chn < chans; chn++)
    Xen_vector_set(vec, chn, g_sound_data_to_vct(sdata, C_int_to_Xen_integer(chn), Xen_undefined));
  return vec;
}
#endif


#if HAVE_RUBY
static Xen sound_data_each(Xen obj)
{
  int j, chans;
  mus_long_t i, len;
  sound_data *sd;

  sd = Xen_to_sound_data(obj);
  len = mus_sound_data_length(sd);
  chans = mus_sound_data_chans(sd);

  for (j = 0; j < chans; j++)
    {
      mus_float_t *d;
      d = mus_sound_data_channel_data(sd, j);
      for (i = 0; i < len; i++)
	rb_yield(C_double_to_Xen_real(d[i]));
    }
  return(obj);
}


static Xen sound_data_compare(Xen vr1, Xen vr2)
{
  mus_long_t i, len;
  int j, sd1chans, sd2chans;
  sound_data *sd1, *sd2;
  if ((xen_is_sound_data(vr1)) && (xen_is_sound_data(vr2)))
    {
      sd1 = Xen_to_sound_data(vr1);
      sd2 = Xen_to_sound_data(vr2);
      sd1chans = mus_sound_data_chans(sd1);
      sd2chans = mus_sound_data_chans(sd2);

      if (sd1chans > sd2chans) 
	return(C_int_to_Xen_integer(1));
      if (sd1chans < sd2chans)
	return(C_int_to_Xen_integer(-1));

      len = mus_sound_data_length(sd1);
      if (len > mus_sound_data_length(sd2)) len = mus_sound_data_length(sd2);

      for (j = 0; j < sd1chans; j++)
	{
	  mus_float_t *d1, *d2;
	  d1 = mus_sound_data_channel_data(sd1, j);
	  d2 = mus_sound_data_channel_data(sd2, j);
	  for (i = 0; i < len; i++) 
	    if (d1[i] < d2[i])
	      return(C_int_to_Xen_integer(-1));
	    else
	      if (d1[i] > d2[i])
		return(C_int_to_Xen_integer(1));
	}
      len = mus_sound_data_length(sd1) - mus_sound_data_length(sd2);
      if (len == 0) return(Xen_integer_zero);
      if (len > 0) return(C_int_to_Xen_integer(1));
    }
  return(C_int_to_Xen_integer(-1));
}


static Xen sound_data_size(Xen obj)
{
  sound_data *sd;
  sd = Xen_to_sound_data(obj);
  return(C_llong_to_Xen_llong(mus_sound_data_length(sd) * mus_sound_data_chans(sd)));
}


static Xen sound_data_chans(Xen obj)
{
  sound_data *sd;
  sd = Xen_to_sound_data(obj);
  return(C_int_to_Xen_integer(mus_sound_data_chans(sd)));
}


static Xen g_rb_sound_data_fill(Xen obj, Xen val)
{
  sound_data_fill(Xen_to_sound_data(obj), Xen_real_to_C_double(val));
  return(val);
}


static Xen g_rb_make_sound_data(Xen self, Xen chans, Xen framples)
{
  return(g_make_sound_data(chans, framples));
}
#endif
#endif /* (!DISABLE_DEPRECATED) */


static Xen g_mus_max_malloc(void)
{
  #define H_mus_max_malloc "(" S_mus_max_malloc "): maximum number of bytes we will try to malloc."
  return(C_llong_to_Xen_llong(mus_max_malloc()));
}

#if HAVE_SCHEME
  static s7_pointer mus_max_malloc_symbol;
#endif

static Xen g_mus_set_max_malloc(Xen val)
{
  mus_long_t size;
  Xen_check_type(Xen_is_llong(val), val, 1, S_setB S_mus_max_malloc, "an integer");
  size = Xen_llong_to_C_llong(val);
#if HAVE_SCHEME
  s7_symbol_set_value(s7, mus_max_malloc_symbol, s7_make_integer(s7, size));
#endif
  return(C_llong_to_Xen_llong(mus_set_max_malloc(size)));
}



static Xen g_mus_max_table_size(void)
{
  #define H_mus_max_table_size "(" S_mus_max_table_size "): maximum table size."
  return(C_llong_to_Xen_llong(mus_max_table_size()));
}


#if HAVE_SCHEME
  static s7_pointer mus_max_table_size_symbol;
#endif

static Xen g_mus_set_max_table_size(Xen val)
{
  mus_long_t size;
  Xen_check_type(Xen_is_llong(val), val, 1, S_setB S_mus_max_table_size, "an integer");
  size = Xen_llong_to_C_llong(val);
#if HAVE_SCHEME
  s7_symbol_set_value(s7, mus_max_table_size_symbol, s7_make_integer(s7, size));
#endif
  return(C_llong_to_Xen_llong(mus_set_max_table_size(size)));
}




#if __APPLE__
#define S_mus_audio_output_properties_mutable "mus-audio-output-properties-mutable"
static Xen g_mus_audio_output_properties_mutable(Xen mut)
{
  #define H_mus_audio_output_properties_mutable "(" S_mus_audio_output_properties_mutable " val): can DAC settings be changed to match the current sound"
  Xen_check_type(Xen_is_boolean(mut), mut, 1, S_mus_audio_output_properties_mutable, "a boolean");
  return(C_bool_to_Xen_boolean(mus_audio_output_properties_mutable(Xen_boolean_to_C_bool(mut))));
}
#endif

#if (!DISABLE_DEPRECATED)
#if (!HAVE_SCHEME)
Xen_wrap_1_arg(g_sound_data_copy_w, g_sound_data_copy)
Xen_wrap_2_args(g_sound_data_fillB_w, g_sound_data_fillB)
Xen_wrap_1_arg(g_sound_data_peak_w, g_sound_data_peak)
Xen_wrap_1_arg(g_sound_data_length_w, g_sound_data_length)
Xen_wrap_1_arg(g_sound_data_chans_w, g_sound_data_chans)
Xen_wrap_2_args(g_make_sound_data_w, g_make_sound_data)
Xen_wrap_2_args(g_sound_data_addB_w, g_sound_data_addB)
Xen_wrap_2_args(g_sound_data_add_w, g_sound_data_add)
Xen_wrap_2_args(g_sound_data_offsetB_w, g_sound_data_offsetB)
Xen_wrap_2_args(g_sound_data_multiplyB_w, g_sound_data_multiplyB)
Xen_wrap_2_args(g_sound_data_multiply_w, g_sound_data_multiply)
Xen_wrap_1_arg(g_sound_data_reverseB_w, g_sound_data_reverseB)
Xen_wrap_1_arg(g_sound_data_maxamp_w, g_sound_data_maxamp)
Xen_wrap_2_args(g_sound_data_scaleB_w, g_sound_data_scaleB)
Xen_wrap_3_args(g_sound_data_ref_w, g_sound_data_ref)
Xen_wrap_4_args(g_sound_data_set_w, g_sound_data_set)
Xen_wrap_1_arg(g_is_sound_data_w, g_is_sound_data)
Xen_wrap_3_optional_args(g_sound_data_to_vct_w, g_sound_data_to_vct)
Xen_wrap_3_optional_args(g_vct_to_sound_data_w, g_vct_to_sound_data)
#endif

Xen_wrap_1_arg(g_mus_sound_open_input_w, g_mus_sound_open_input)
Xen_wrap_1_arg(g_mus_sound_close_input_w, g_mus_sound_close_input)
Xen_wrap_6_optional_args(g_mus_sound_open_output_w, g_mus_sound_open_output)
Xen_wrap_5_optional_args(g_mus_sound_reopen_output_w, g_mus_sound_reopen_output)
Xen_wrap_2_args(g_mus_sound_close_output_w, g_mus_sound_close_output)
Xen_wrap_5_args(g_mus_sound_read_w, g_mus_sound_read)
Xen_wrap_5_args(g_mus_sound_write_w, g_mus_sound_write)
Xen_wrap_2_args(g_mus_sound_seek_frample_w, g_mus_sound_seek_frample)

#endif

Xen_wrap_1_arg(g_mus_sound_samples_w, g_mus_sound_samples)
Xen_wrap_2_args(g_mus_sound_set_samples_w, g_mus_sound_set_samples)
Xen_wrap_1_arg(g_mus_sound_framples_w, g_mus_sound_framples)
Xen_wrap_1_arg(g_mus_sound_duration_w, g_mus_sound_duration)
Xen_wrap_1_arg(g_mus_sound_datum_size_w, g_mus_sound_datum_size)
Xen_wrap_1_arg(g_mus_sound_data_location_w, g_mus_sound_data_location)
Xen_wrap_2_args(g_mus_sound_set_data_location_w, g_mus_sound_set_data_location)
Xen_wrap_1_arg(g_mus_sound_chans_w, g_mus_sound_chans)
Xen_wrap_2_args(g_mus_sound_set_chans_w, g_mus_sound_set_chans)
Xen_wrap_1_arg(g_mus_sound_srate_w, g_mus_sound_srate)
Xen_wrap_2_args(g_mus_sound_set_srate_w, g_mus_sound_set_srate)
Xen_wrap_1_arg(g_mus_sound_header_type_w, g_mus_sound_header_type)
Xen_wrap_2_args(g_mus_sound_set_header_type_w, g_mus_sound_set_header_type)
Xen_wrap_1_arg(g_mus_sound_data_format_w, g_mus_sound_data_format)
Xen_wrap_2_args(g_mus_sound_set_data_format_w, g_mus_sound_set_data_format)
Xen_wrap_1_arg(g_mus_sound_length_w, g_mus_sound_length)
Xen_wrap_1_arg(g_mus_sound_type_specifier_w, g_mus_sound_type_specifier)
Xen_wrap_1_arg(g_mus_header_type_name_w, g_mus_header_type_name)
Xen_wrap_1_arg(g_mus_header_type_to_string_w, g_mus_header_type_to_string)
Xen_wrap_1_arg(g_mus_data_format_name_w, g_mus_data_format_name)
Xen_wrap_1_arg(g_mus_data_format_to_string_w, g_mus_data_format_to_string)
Xen_wrap_1_arg(g_mus_sound_comment_w, g_mus_sound_comment)
Xen_wrap_1_arg(g_mus_sound_write_date_w, g_mus_sound_write_date)
Xen_wrap_1_arg(g_mus_bytes_per_sample_w, g_mus_bytes_per_sample)
Xen_wrap_1_arg(g_mus_sound_loop_info_w, g_mus_sound_loop_info)
Xen_wrap_1_arg(g_mus_sound_mark_info_w, g_mus_sound_mark_info)
Xen_wrap_1_arg(g_mus_sound_maxamp_w, g_mus_sound_maxamp)
Xen_wrap_2_args(g_mus_sound_set_maxamp_w, g_mus_sound_set_maxamp)
Xen_wrap_1_arg(g_mus_sound_maxamp_exists_w, g_mus_sound_maxamp_exists)
Xen_wrap_1_arg(g_mus_sound_preload_w, g_mus_sound_preload)

Xen_wrap_1_arg(g_mus_audio_close_w, g_mus_audio_close)
Xen_wrap_4_optional_args(g_mus_audio_write_w, g_mus_audio_write)
Xen_wrap_3_args(g_mus_audio_read_w, g_mus_audio_read)
Xen_wrap_5_args(g_mus_audio_open_output_w, g_mus_audio_open_output)
Xen_wrap_5_args(g_mus_audio_open_input_w, g_mus_audio_open_input)

Xen_wrap_no_args(g_mus_clipping_w, g_mus_clipping)
Xen_wrap_1_arg(g_mus_set_clipping_w, g_mus_set_clipping)
Xen_wrap_1_arg(g_mus_file_clipping_w, g_mus_file_clipping)
Xen_wrap_2_args(g_mus_file_set_clipping_w, g_mus_file_set_clipping)
Xen_wrap_no_args(g_mus_header_raw_defaults_w, g_mus_header_raw_defaults)
Xen_wrap_1_arg(g_mus_header_set_raw_defaults_w, g_mus_header_set_raw_defaults)
Xen_wrap_2_args(g_mus_header_writable_w, g_mus_header_writable)
Xen_wrap_1_arg(g_mus_expand_filename_w, g_mus_expand_filename)
Xen_wrap_1_optional_arg(g_mus_sound_report_cache_w, g_mus_sound_report_cache)
Xen_wrap_1_arg(g_mus_sound_forget_w, g_mus_sound_forget)
Xen_wrap_no_args(g_mus_sound_prune_w, g_mus_sound_prune)
Xen_wrap_1_arg(g_mus_error_type_to_string_w, g_mus_error_type_to_string)
Xen_wrap_2_args(g_mus_oss_set_buffers_w, g_mus_oss_set_buffers)
Xen_wrap_5_args(g_array_to_file_w, g_array_to_file)
Xen_wrap_5_args(g_file_to_array_w, g_file_to_array)
Xen_wrap_no_args(g_mus_alsa_buffers_w, g_mus_alsa_buffers)
Xen_wrap_1_arg(g_mus_alsa_set_buffers_w, g_mus_alsa_set_buffers)
Xen_wrap_no_args(g_mus_alsa_buffer_size_w, g_mus_alsa_buffer_size)
Xen_wrap_1_arg(g_mus_alsa_set_buffer_size_w, g_mus_alsa_set_buffer_size)
Xen_wrap_no_args(g_mus_alsa_device_w, g_mus_alsa_device)
Xen_wrap_1_arg(g_mus_alsa_set_device_w, g_mus_alsa_set_device)
Xen_wrap_no_args(g_mus_alsa_playback_device_w, g_mus_alsa_playback_device)
Xen_wrap_1_arg(g_mus_alsa_set_playback_device_w, g_mus_alsa_set_playback_device)
Xen_wrap_no_args(g_mus_alsa_capture_device_w, g_mus_alsa_capture_device)
Xen_wrap_1_arg(g_mus_alsa_set_capture_device_w, g_mus_alsa_set_capture_device)
Xen_wrap_no_args(g_mus_alsa_squelch_warning_w, g_mus_alsa_squelch_warning)
Xen_wrap_1_arg(g_mus_alsa_set_squelch_warning_w, g_mus_alsa_set_squelch_warning)

#if HAVE_OSS
  Xen_wrap_no_args(g_mus_audio_reinitialize_w, g_mus_audio_reinitialize)
#endif

#if __APPLE__
Xen_wrap_1_arg(g_mus_audio_output_properties_mutable_w, g_mus_audio_output_properties_mutable)
#endif

Xen_wrap_no_args(g_mus_max_malloc_w, g_mus_max_malloc)
Xen_wrap_1_arg(g_mus_set_max_malloc_w, g_mus_set_max_malloc)
Xen_wrap_no_args(g_mus_max_table_size_w, g_mus_max_table_size)
Xen_wrap_1_arg(g_mus_set_max_table_size_w, g_mus_set_max_table_size)




void mus_sndlib_xen_initialize(void)
{
  mus_sound_initialize();

#if (!DISABLE_DEPRECATED)
#if (!HAVE_SCHEME)
  sound_data_tag = Xen_make_object_type("SoundData", sizeof(sound_data));
#endif

#if HAVE_FORTH
  fth_set_object_inspect(sound_data_tag, print_sound_data);
  fth_set_object_equal(sound_data_tag, equalp_sound_data);
  fth_set_object_to_array(sound_data_tag, g_sound_data_to_vector);
  fth_set_object_length(sound_data_tag, g_sound_data_length);
  fth_set_object_free(sound_data_tag, free_sound_data);
  fth_set_object_apply(sound_data_tag, Xen_procedure_cast sound_data_apply, 2, 0, 0);
#endif

#if HAVE_RUBY
  rb_include_module(sound_data_tag, rb_mComparable);
  rb_include_module(sound_data_tag, rb_mEnumerable);
  rb_define_method(sound_data_tag, "to_s",   Xen_procedure_cast print_sound_data,     0);
  rb_define_method(sound_data_tag, "eql?",   Xen_procedure_cast equalp_sound_data,    1);
  rb_define_method(sound_data_tag, "==",     Xen_procedure_cast equalp_sound_data,    1);
  rb_define_method(sound_data_tag, "each",   Xen_procedure_cast sound_data_each,      0);
  rb_define_method(sound_data_tag, "<=>",    Xen_procedure_cast sound_data_compare,   1);
  rb_define_method(sound_data_tag, "[]",     Xen_procedure_cast g_sound_data_ref,     2);
  rb_define_method(sound_data_tag, "[]=",    Xen_procedure_cast g_sound_data_set,     3);
  rb_define_method(sound_data_tag, "length", Xen_procedure_cast sound_data_size,      0);
  rb_define_method(sound_data_tag, "size",   Xen_procedure_cast sound_data_size,      0);
  rb_define_method(sound_data_tag, "fill",   Xen_procedure_cast g_rb_sound_data_fill, 1);
  rb_define_method(sound_data_tag, "dup",    Xen_procedure_cast g_sound_data_copy,    0);
  rb_define_method(sound_data_tag, "chans",  Xen_procedure_cast sound_data_chans,     0);
  rb_define_method(sound_data_tag, "peak",   Xen_procedure_cast g_sound_data_peak,    0);
  rb_define_method(sound_data_tag, "offset!",   Xen_procedure_cast g_sound_data_offsetB,   1);
  rb_define_method(sound_data_tag, "multiply!", Xen_procedure_cast g_sound_data_multiplyB, 1);

  rb_define_method(sound_data_tag, "add!",      Xen_procedure_cast g_sound_data_addB,      1);
  rb_define_method(sound_data_tag, "scale!",    Xen_procedure_cast g_sound_data_scaleB,    1);
  rb_define_method(sound_data_tag, "reverse!",  Xen_procedure_cast g_sound_data_reverseB,  0);

  rb_define_singleton_method(sound_data_tag, "new", Xen_procedure_cast g_rb_make_sound_data, 2);
#endif
#endif

#if HAVE_RUBY
  Init_Hook();
#endif

  Xen_define_constant(S_mus_out_format,           MUS_OUT_FORMAT,           "sample format for fastest IO");
  Xen_define_constant(S_mus_unsupported,          MUS_UNSUPPORTED,          "unsupported header id");
  Xen_define_constant(S_mus_next,                 MUS_NEXT,                 "NeXT (Sun) sound header id");
  Xen_define_constant(S_mus_aifc,                 MUS_AIFC,                 "AIFC sound header id");
  Xen_define_constant(S_mus_rf64,                 MUS_RF64,                 "RF64 sound header id");
  Xen_define_constant(S_mus_riff,                 MUS_RIFF,                 "RIFF (MS wave) sound header id");
  Xen_define_constant(S_mus_nist,                 MUS_NIST,                 "NIST (Sphere) sound header id");
  Xen_define_constant(S_mus_raw,                  MUS_RAW,                  "raw (headerless) sound header id");
  Xen_define_constant(S_mus_ircam,                MUS_IRCAM,                "IRCAM sound header id");
  Xen_define_constant(S_mus_aiff,                 MUS_AIFF,                 "AIFF (old-style) sound header id");
  Xen_define_constant(S_mus_bicsf,                MUS_BICSF,                "BICSF header id");
  Xen_define_constant(S_mus_voc,                  MUS_VOC,                  "VOC header id");
  Xen_define_constant(S_mus_svx,                  MUS_SVX,                  "SVX (IFF) header id");
  Xen_define_constant(S_mus_soundfont,            MUS_SOUNDFONT,            "soundfont header id");
  Xen_define_constant(S_mus_caff,                 MUS_CAFF,                 "Apple Core Audio File Format header id");

  Xen_define_constant(S_mus_unknown,              MUS_UNKNOWN,              "unknown data format");
  Xen_define_constant(S_mus_bshort,               MUS_BSHORT,               "big-endian short data format id");
  Xen_define_constant(S_mus_lshort,               MUS_LSHORT,               "little-endian short data format id");
  Xen_define_constant(S_mus_mulaw,                MUS_MULAW,                "mulaw (8-bit) data format id");
  Xen_define_constant(S_mus_alaw,                 MUS_ALAW,                 "alaw (8-bit) data format id");
  Xen_define_constant(S_mus_byte,                 MUS_BYTE,                 "signed byte data format id");
  Xen_define_constant(S_mus_ubyte,                MUS_UBYTE,                "unsigned byte data format id");
  Xen_define_constant(S_mus_bfloat,               MUS_BFLOAT,               "big-endian float data format id");
  Xen_define_constant(S_mus_lfloat,               MUS_LFLOAT,               "little-endian float data format id");
  Xen_define_constant(S_mus_bint,                 MUS_BINT,                 "big-endian int data format id");
  Xen_define_constant(S_mus_lint,                 MUS_LINT,                 "little-endian int data format id");
  Xen_define_constant(S_mus_bintn,                MUS_BINTN,                "normalized big-endian int data format id");
  Xen_define_constant(S_mus_lintn,                MUS_LINTN,                "normalized little-endian int data format id");
  Xen_define_constant(S_mus_b24int,               MUS_B24INT,               "big-endian 24-bit data format id");
  Xen_define_constant(S_mus_l24int,               MUS_L24INT,               "little-endian 24-bit data format id");
  Xen_define_constant(S_mus_bdouble,              MUS_BDOUBLE,              "big-endian double data format id");
  Xen_define_constant(S_mus_ldouble,              MUS_LDOUBLE,              "little-endian double data format id");
  Xen_define_constant(S_mus_ubshort,              MUS_UBSHORT,              "unsigned big-endian short data format id");
  Xen_define_constant(S_mus_ulshort,              MUS_ULSHORT,              "unsigned little-endian short data format id");
  Xen_define_constant(S_mus_bdouble_unscaled,     MUS_BDOUBLE_UNSCALED,     "unscaled big-endian double data format id");
  Xen_define_constant(S_mus_ldouble_unscaled,     MUS_LDOUBLE_UNSCALED,     "unscaled little-endian double data format id");
  Xen_define_constant(S_mus_bfloat_unscaled,      MUS_BFLOAT_UNSCALED,      "unscaled big-endian float data format id");
  Xen_define_constant(S_mus_lfloat_unscaled,      MUS_LFLOAT_UNSCALED,      "unscaled little-endian float data format id");

  Xen_define_constant(S_mus_audio_default,        MUS_AUDIO_DEFAULT,        "default audio device");

  Xen_define_procedure_with_setter(S_mus_sound_samples, g_mus_sound_samples_w, H_mus_sound_samples, 
				   S_setB S_mus_sound_samples, g_mus_sound_set_samples_w, 1, 0, 2, 0);
  Xen_define_procedure_with_setter(S_mus_sound_data_location, g_mus_sound_data_location_w, H_mus_sound_data_location,
				   S_setB S_mus_sound_data_location, g_mus_sound_set_data_location_w, 1, 0, 2, 0);
  Xen_define_procedure_with_setter(S_mus_sound_chans, g_mus_sound_chans_w, H_mus_sound_chans,
				   S_setB S_mus_sound_chans, g_mus_sound_set_chans_w, 1, 0, 2, 0);
  Xen_define_procedure_with_setter(S_mus_sound_srate, g_mus_sound_srate_w, H_mus_sound_srate,
				   S_setB S_mus_sound_srate, g_mus_sound_set_srate_w, 1, 0, 2, 0);
  Xen_define_procedure_with_setter(S_mus_sound_header_type, g_mus_sound_header_type_w, H_mus_sound_header_type,
				   S_setB S_mus_sound_header_type, g_mus_sound_set_header_type_w, 1, 0, 2, 0);
  Xen_define_procedure_with_setter(S_mus_sound_data_format, g_mus_sound_data_format_w, H_mus_sound_data_format,
				   S_setB S_mus_sound_data_format, g_mus_sound_set_data_format_w, 1, 0, 2, 0);


  /* -------------------------------------------------------------------------------- */

#if (!DISABLE_DEPRECATED)
#if HAVE_SCHEME
  s7_eval_c_string(s7, "(define sound-data-ref float-vector-ref)");
  s7_eval_c_string(s7, "(define sound-data-set! float-vector-set!)");
  s7_eval_c_string(s7, "(define sound-data? float-vector?)");
  s7_eval_c_string(s7, "(define sound-data-copy copy)");
  s7_eval_c_string(s7, "(define sound-data-fill! fill!)");
  s7_eval_c_string(s7, "(define (sound-data-chans sd) (car (vector-dimensions sd)))");
  s7_eval_c_string(s7, "(define (sound-data-length sd) (cadr (vector-dimensions sd)))");
  s7_eval_c_string(s7, "(define (make-sound-data chans framples) (make-vector (list chans framples) 0.0 #t))");
#else
  Xen_define_safe_procedure(S_is_sound_data,             g_is_sound_data_w,               1, 0, 0, H_is_sound_data);
  Xen_define_safe_procedure(S_sound_data_setB,          g_sound_data_set_w,             4, 0, 0, H_sound_data_setB);

  Xen_define_procedure_with_setter(S_sound_data_ref, g_sound_data_ref_w, H_sound_data_ref,
				   S_setB S_sound_data_ref, g_sound_data_set_w,  3, 0, 4, 0);

  Xen_define_safe_procedure(S_sound_data_to_vct,        g_sound_data_to_vct_w,          1, 2, 0, H_sound_data_to_vct);
  Xen_define_safe_procedure(S_vct_to_sound_data,        g_vct_to_sound_data_w,          1, 2, 0, H_vct_to_sound_data);
  Xen_define_safe_procedure(S_sound_data_maxamp,        g_sound_data_maxamp_w,          1, 0, 0, H_sound_data_maxamp);
  Xen_define_safe_procedure(S_make_sound_data,          g_make_sound_data_w,            2, 0, 0, H_make_sound_data);
  Xen_define_safe_procedure(S_sound_data_copy,          g_sound_data_copy_w,            1, 0, 0, H_sound_data_copy);
  Xen_define_safe_procedure(S_sound_data_fillB,         g_sound_data_fillB_w,           2, 0, 0, H_sound_data_fillB);
  Xen_define_safe_procedure(S_sound_data_peak,          g_sound_data_peak_w,            1, 0, 0, H_sound_data_peak);
  Xen_define_safe_procedure(S_sound_data_chans,         g_sound_data_chans_w,           1, 0, 0, H_sound_data_chans);
  Xen_define_safe_procedure(S_sound_data_length,        g_sound_data_length_w,          1, 0, 0, H_sound_data_length);

  Xen_define_safe_procedure(S_sound_data_scaleB,        g_sound_data_scaleB_w,          2, 0, 0, H_sound_data_scaleB);
  Xen_define_safe_procedure(S_sound_data_offsetB,       g_sound_data_offsetB_w,         2, 0, 0, H_sound_data_offsetB);
  Xen_define_safe_procedure(S_sound_data_addB,          g_sound_data_addB_w,            2, 0, 0, H_sound_data_addB);
  Xen_define_safe_procedure(S_sound_data_add,           g_sound_data_add_w,             2, 0, 0, H_sound_data_add);
  Xen_define_safe_procedure(S_sound_data_multiplyB,     g_sound_data_multiplyB_w,       2, 0, 0, H_sound_data_multiplyB);
  Xen_define_safe_procedure(S_sound_data_multiply,      g_sound_data_multiply_w,        2, 0, 0, H_sound_data_multiply);
  Xen_define_safe_procedure(S_sound_data_reverseB,      g_sound_data_reverseB_w,        1, 0, 0, H_sound_data_reverseB);
#endif
#endif
  /* -------------------------------------------------------------------------------- */


  Xen_define_safe_procedure(S_mus_sound_framples,       g_mus_sound_framples_w,         1, 0, 0, H_mus_sound_framples);
  Xen_define_safe_procedure("mus-sound-frames",         g_mus_sound_framples_w,         1, 0, 0, H_mus_sound_framples);
  Xen_define_safe_procedure(S_mus_sound_duration,       g_mus_sound_duration_w,         1, 0, 0, H_mus_sound_duration);
  Xen_define_safe_procedure(S_mus_sound_datum_size,     g_mus_sound_datum_size_w,       1, 0, 0, H_mus_sound_datum_size);
  Xen_define_safe_procedure(S_mus_sound_length,         g_mus_sound_length_w,           1, 0, 0, H_mus_sound_length);
  Xen_define_safe_procedure(S_mus_sound_type_specifier, g_mus_sound_type_specifier_w,   1, 0, 0, H_mus_sound_type_specifier);
  Xen_define_safe_procedure(S_mus_header_type_name,     g_mus_header_type_name_w,       1, 0, 0, H_mus_header_type_name);
  Xen_define_safe_procedure(S_mus_header_type_to_string,g_mus_header_type_to_string_w,  1, 0, 0, H_mus_header_type_to_string);
  Xen_define_safe_procedure(S_mus_header_writable,      g_mus_header_writable_w,        2, 0, 0, H_mus_header_writable);
  Xen_define_safe_procedure(S_mus_data_format_name,     g_mus_data_format_name_w,       1, 0, 0, H_mus_data_format_name);
  Xen_define_safe_procedure(S_mus_data_format_to_string,g_mus_data_format_to_string_w,  1, 0, 0, H_mus_data_format_to_string);
  Xen_define_safe_procedure(S_mus_sound_comment,        g_mus_sound_comment_w,          1, 0, 0, H_mus_sound_comment);
  Xen_define_safe_procedure(S_mus_sound_write_date,     g_mus_sound_write_date_w,       1, 0, 0, H_mus_sound_write_date);
  Xen_define_safe_procedure(S_mus_bytes_per_sample,     g_mus_bytes_per_sample_w,       1, 0, 0, H_mus_bytes_per_sample);
  Xen_define_safe_procedure(S_mus_sound_loop_info,      g_mus_sound_loop_info_w,        1, 0, 0, H_mus_sound_loop_info);
  Xen_define_safe_procedure(S_mus_sound_mark_info,      g_mus_sound_mark_info_w,        1, 0, 0, H_mus_sound_mark_info);
  Xen_define_safe_procedure(S_mus_sound_maxamp_exists,  g_mus_sound_maxamp_exists_w,    1, 0, 0, H_mus_sound_maxamp_exists);
  Xen_define_safe_procedure(S_mus_sound_forget,         g_mus_sound_forget_w,           1, 0, 0, H_mus_sound_forget);
  Xen_define_safe_procedure(S_mus_sound_prune,          g_mus_sound_prune_w,            0, 0, 0, H_mus_sound_prune);

#if (!DISABLE_DEPRECATED)
  Xen_define_safe_procedure(S_mus_sound_open_input,     g_mus_sound_open_input_w,       1, 0, 0, H_mus_sound_open_input);
  Xen_define_safe_procedure(S_mus_sound_close_input,    g_mus_sound_close_input_w,      1, 0, 0, H_mus_sound_close_input);
  Xen_define_safe_procedure(S_mus_sound_open_output,    g_mus_sound_open_output_w,      1, 5, 0, H_mus_sound_open_output);
  Xen_define_safe_procedure(S_mus_sound_reopen_output,  g_mus_sound_reopen_output_w,    1, 4, 0, H_mus_sound_reopen_output);
  Xen_define_safe_procedure(S_mus_sound_close_output,   g_mus_sound_close_output_w,     2, 0, 0, H_mus_sound_close_output);
  Xen_define_safe_procedure(S_mus_sound_read,           g_mus_sound_read_w,             5, 0, 0, H_mus_sound_read);
  Xen_define_safe_procedure(S_mus_sound_write,          g_mus_sound_write_w,            5, 0, 0, H_mus_sound_write);
  Xen_define_safe_procedure(S_mus_sound_seek_frample,   g_mus_sound_seek_frample_w,     2, 0, 0, H_mus_sound_seek_frample); 
  Xen_define_safe_procedure("mus-sound-seek-frame",     g_mus_sound_seek_frample_w,     2, 0, 0, H_mus_sound_seek_frample); 
#endif

  Xen_define_safe_procedure(S_mus_audio_close,          g_mus_audio_close_w,            1, 0, 0, H_mus_audio_close);
  Xen_define_safe_procedure(S_mus_audio_write,          g_mus_audio_write_w,            3, 1, 0, H_mus_audio_write);
  Xen_define_safe_procedure(S_mus_audio_read,           g_mus_audio_read_w,             3, 0, 0, H_mus_audio_read);
  Xen_define_safe_procedure(S_mus_audio_open_output,    g_mus_audio_open_output_w,      5, 0, 0, H_mus_audio_open_output);
  Xen_define_safe_procedure(S_mus_audio_open_input,     g_mus_audio_open_input_w,       5, 0, 0, H_mus_audio_open_input);

  Xen_define_safe_procedure(S_mus_expand_filename,      g_mus_expand_filename_w,        1, 0, 0, H_mus_expand_filename);
  Xen_define_safe_procedure(S_mus_sound_report_cache,   g_mus_sound_report_cache_w,     0, 1, 0, H_mus_sound_report_cache);
  Xen_define_safe_procedure(S_mus_error_type_to_string, g_mus_error_type_to_string_w,   1, 0, 0, H_mus_error_type_to_string);
  Xen_define_safe_procedure(S_mus_oss_set_buffers,      g_mus_oss_set_buffers_w,        2, 0, 0, H_mus_oss_set_buffers);
  Xen_define_safe_procedure(S_array_to_file,            g_array_to_file_w,              5, 0, 0, H_array_to_file);
  Xen_define_safe_procedure(S_file_to_array,            g_file_to_array_w,              5, 0, 0, H_file_to_array);

  Xen_define_safe_procedure(S_mus_sound_preload,        g_mus_sound_preload_w,          1, 0, 0, H_mus_sound_preload);

  Xen_define_procedure_with_setter(S_mus_header_raw_defaults, g_mus_header_raw_defaults_w, H_mus_header_raw_defaults,
				   S_setB S_mus_header_raw_defaults, g_mus_header_set_raw_defaults_w, 0, 0, 1, 0);

  Xen_define_procedure_with_setter(S_mus_clipping, g_mus_clipping_w, H_mus_clipping,
				   S_setB S_mus_clipping, g_mus_set_clipping_w, 0, 0, 1, 0);
  Xen_define_procedure_with_setter(S_mus_file_clipping, g_mus_file_clipping_w, H_mus_file_clipping,
				   S_setB S_mus_file_clipping, g_mus_file_set_clipping_w, 1, 0, 2, 0);

  Xen_define_procedure_with_setter(S_mus_sound_maxamp, g_mus_sound_maxamp_w, H_mus_sound_maxamp,
				   S_setB S_mus_sound_maxamp, g_mus_sound_set_maxamp_w, 1, 0, 2, 0);

  /* these are no-ops if not ALSA, but that makes it easier to maintain global initialization files */
  Xen_define_procedure_with_setter(S_mus_alsa_buffers, g_mus_alsa_buffers_w, H_mus_alsa_buffers,
				   S_setB S_mus_alsa_buffers, g_mus_alsa_set_buffers_w, 0, 0, 1, 0);
  Xen_define_procedure_with_setter(S_mus_alsa_buffer_size, g_mus_alsa_buffer_size_w, H_mus_alsa_buffer_size,
				   S_setB S_mus_alsa_buffer_size, g_mus_alsa_set_buffer_size_w, 0, 0, 1, 0);
  Xen_define_procedure_with_setter(S_mus_alsa_device, g_mus_alsa_device_w, H_mus_alsa_device,
				   S_setB S_mus_alsa_device, g_mus_alsa_set_device_w, 0, 0, 1, 0);
  Xen_define_procedure_with_setter(S_mus_alsa_playback_device, g_mus_alsa_playback_device_w, H_mus_alsa_playback_device,
				   S_setB S_mus_alsa_playback_device, g_mus_alsa_set_playback_device_w, 0, 0, 1, 0);
  Xen_define_procedure_with_setter(S_mus_alsa_capture_device, g_mus_alsa_capture_device_w, H_mus_alsa_capture_device,
				   S_setB S_mus_alsa_capture_device, g_mus_alsa_set_capture_device_w, 0, 0, 1, 0);
  Xen_define_procedure_with_setter(S_mus_alsa_squelch_warning, g_mus_alsa_squelch_warning_w, H_mus_alsa_squelch_warning,
				   S_setB S_mus_alsa_squelch_warning, g_mus_alsa_set_squelch_warning_w, 0, 0, 1, 0);

  Xen_define_procedure_with_setter(S_mus_max_malloc, g_mus_max_malloc_w, H_mus_max_malloc,
				   S_setB S_mus_max_malloc, g_mus_set_max_malloc_w, 0, 0, 1, 0);
  Xen_define_procedure_with_setter(S_mus_max_table_size, g_mus_max_table_size_w, H_mus_max_table_size,
				   S_setB S_mus_max_table_size, g_mus_set_max_table_size_w, 0, 0, 1, 0);

#if HAVE_SCHEME
  mus_max_table_size_symbol = s7_define_variable(s7, "*" S_mus_max_table_size "*", s7_make_integer(s7, MUS_MAX_TABLE_SIZE_DEFAULT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_mus_max_table_size "*) (list #f (lambda (s v) (set! (" S_mus_max_table_size ") v)) #f))");
  s7_symbol_set_documentation(s7, mus_max_table_size_symbol, "*mus-max-table-size*: maximum table size.");

  mus_max_malloc_symbol = s7_define_variable(s7, "*" S_mus_max_malloc "*", s7_make_integer(s7, MUS_MAX_MALLOC_DEFAULT));
  s7_eval_c_string(s7, "(set! (symbol-access '*" S_mus_max_malloc "*) (list #f (lambda (s v) (set! (" S_mus_max_malloc ") v)) #f))");
  s7_symbol_set_documentation(s7, mus_max_malloc_symbol, "*mus-max-malloc*: maximum number of bytes we will try to malloc.");
#endif

#if HAVE_OSS
  Xen_define_procedure(S_mus_audio_reinitialize,   g_mus_audio_reinitialize_w, 0, 0, 0,  H_mus_audio_reinitialize);
#endif

#if (!DISABLE_DEPRECATED)
#if HAVE_FORTH
  Xen_define_procedure(S_sound_data_to_vector, g_sound_data_to_vector /* no _w! */, 1, 0, 0, H_sound_data_to_vector);
#endif
#endif

#if __APPLE__
  Xen_define_procedure(S_mus_audio_output_properties_mutable, g_mus_audio_output_properties_mutable_w, 1, 0, 0, H_mus_audio_output_properties_mutable);
#endif

  #define H_new_sound_hook S_new_sound_hook "(name): called when a new sound file is being created"
  new_sound_hook = Xen_define_hook(S_new_sound_hook, "(make-hook 'name)", 1, H_new_sound_hook);
  mus_header_write_set_hook(g_new_sound_hook);

  Xen_provide_feature("sndlib");
}
