/* snd-xrec and snd-grec shared code */

/* TODO: split out the Jack cases */

#include "snd.h"
#include "snd-rec.h"

FONT_TYPE *get_vu_font(Float size)
{
  char font_name[LABEL_BUFFER_SIZE];
  int font_size;
  char *vu_font_name;
  FONT_TYPE *label_font;
  font_size = (int)(size * 12 * vu_font_size(ss));
  if (font_size < 5) font_size = 5;
  vu_font_name = vu_font(ss);
  if (!vu_font_name)
    {
      if (size < 0.75) 
#ifndef SGI
	vu_font_name = "fixed";
#else
        vu_font_name = "courier";
#endif
      else
	{
	  if (size < 1.25) 
	    vu_font_name = "courier";
	  else vu_font_name = "times";
	}
    }
#if USE_GTK
  label_font = LOAD_FONT(font_name);
#else
  mus_snprintf(font_name, LABEL_BUFFER_SIZE, "-*-%s-%s-r-*-*-%d-*-*-*-*-*-*",
	  vu_font_name,
	  (font_size > 10) ? "bold" : "*",
	  font_size);
  
  label_font = LOAD_FONT(font_name);
  if (!(label_font))
    {
      mus_snprintf(font_name, LABEL_BUFFER_SIZE, "-*-%s-*-*-*-*-%d-*-*-*-*-*-*", vu_font_name, font_size);
      label_font = LOAD_FONT(font_name);
      if (!(label_font))
	{
	  mus_snprintf(font_name, LABEL_BUFFER_SIZE, "-*-courier-*-*-*-*-%d-*-*-*-*-*-*", font_size);
	  label_font = LOAD_FONT(font_name);
	  while (!(label_font))
	    {
	      mus_snprintf(font_name, LABEL_BUFFER_SIZE, "-*-*-*-*-*-*-%d-*-*-*-*-*-*", font_size);
	      label_font = LOAD_FONT(font_name);
	      font_size++;
	      if (font_size > 60) 
		{
		  label_font = LOAD_FONT("-*-*-*-*-*-*-*-*-*-*-*-*-*");
		  break;
		}
	    }
	}
    }
#endif
  return(label_font);
}

int recorder_columns(int vu_meters)
{
  if ((vu_meters % 4) == 0)
    return(4);
  else 
    {
      if ((vu_meters % 5) == 0)
	return(5);
      else return(vu_meters);
    }
}

char *recorder_device_name(int dev)
{
  /* format label at top of pane */
  switch (dev)
    {
    case MUS_AUDIO_DIGITAL_OUT: return(_("Digital Out")); break;
    case MUS_AUDIO_LINE_OUT:    return(_("Line Out"));    break;
    case MUS_AUDIO_DEFAULT: 
    case MUS_AUDIO_DAC_OUT:     return(_("Output"));      break; /* default here means that linuxppc reports "Output" as analog-in pane name */
    case MUS_AUDIO_DUPLEX_DEFAULT: 
    case MUS_AUDIO_SPEAKERS:    return(_("Speakers"));    break;
    case MUS_AUDIO_ADAT_IN:     return(_("Adat In"));     break;
    case MUS_AUDIO_AES_IN:      return(_("Aes In"));      break;
#if (HAVE_OSS || HAVE_ALSA)
    case MUS_AUDIO_LINE_IN:     return(_("Analog In"));   break;
#else
    case MUS_AUDIO_LINE_IN:     return(_("Line In"));     break;
#endif
    case MUS_AUDIO_MICROPHONE:  return(_("Microphone"));  break;
    case MUS_AUDIO_DIGITAL_IN:  return(_("Digital In"));  break;
    case MUS_AUDIO_ADAT_OUT:    return(_("Adat Out"));    break;
    case MUS_AUDIO_AES_OUT:     return(_("Aes Out"));     break;
    case MUS_AUDIO_DAC_FILTER:  return(_("Tone"));          break;
    case MUS_AUDIO_MIXER:       return(_("Mixer"));         break;
    case MUS_AUDIO_AUX_INPUT:   return(_("Aux Input"));     break;
    case MUS_AUDIO_CD:          return(_("CD"));            break;
    case MUS_AUDIO_AUX_OUTPUT:  return(_("Aux Output"));    break;
    case MUS_AUDIO_SPDIF_IN:    return(_("S/PDIF In"));     break;
    case MUS_AUDIO_SPDIF_OUT:   return(_("S/PDIF Out"));    break;
    default: 
      snd_error("%s[%d] %s: unknown device: %d", 
		__FILE__, __LINE__, c__FUNCTION__, 
		dev); 
      return("Input"); 
      break;
    }
}

static char sysdevstr[LABEL_BUFFER_SIZE];
char *recorder_system_and_device_name(int sys, int dev)
{
  if (strcmp("OSS", mus_audio_system_name(sys)) == 0) 
    return(recorder_device_name(dev));
  mus_snprintf(sysdevstr, LABEL_BUFFER_SIZE,
	       "%s: %s", 
	       mus_audio_system_name(sys), 
	       recorder_device_name(dev));
  return(sysdevstr);
}

bool recorder_input_device(int dev)
{
  switch (dev)
    {
    case MUS_AUDIO_DIGITAL_OUT:
    case MUS_AUDIO_LINE_OUT:
    case MUS_AUDIO_DEFAULT:
    case MUS_AUDIO_ADAT_OUT:
    case MUS_AUDIO_AES_OUT:
    case MUS_AUDIO_SPDIF_OUT:
    case MUS_AUDIO_SPEAKERS:
    case MUS_AUDIO_MIXER:
    case MUS_AUDIO_DAC_FILTER:
    case MUS_AUDIO_AUX_OUTPUT:
    case MUS_AUDIO_DAC_OUT: 
      return(false); 
      break;
    case MUS_AUDIO_DUPLEX_DEFAULT: 
    case MUS_AUDIO_ADAT_IN: 
    case MUS_AUDIO_AES_IN:
    case MUS_AUDIO_SPDIF_IN:
    case MUS_AUDIO_LINE_IN: 
    case MUS_AUDIO_MICROPHONE: 
    case MUS_AUDIO_DIGITAL_IN: 
    case MUS_AUDIO_CD:
    default: 
      return(true); 
      break;
    }
  snd_error(_("unknown audio device: %d"), dev);
  return(false);
}

bool recorder_output_device(int dev)
{
  return((dev != MUS_AUDIO_DAC_FILTER) && 
	 (dev != MUS_AUDIO_MIXER) &&
	 (!(recorder_input_device(dev))));
}

#if (HAVE_OSS || HAVE_ALSA)
char *recorder_field_abbreviation(int fld)
{
  switch (fld)
    {
    case MUS_AUDIO_IMIX:       return("imx"); break;
    case MUS_AUDIO_IGAIN:      return("ign"); break;
    case MUS_AUDIO_RECLEV:     return("rec"); break;
    case MUS_AUDIO_PCM:        return("pcm"); break;
    case MUS_AUDIO_PCM2:       return("pc2"); break;
    case MUS_AUDIO_OGAIN:      return("ogn"); break;
    case MUS_AUDIO_LINE:       return("lin"); break;
    case MUS_AUDIO_MICROPHONE: return("mic"); break;
    case MUS_AUDIO_LINE1:      return("l1");  break;
    case MUS_AUDIO_LINE2:      return("l2");  break;
    case MUS_AUDIO_LINE3:      return("l3");  break;
    case MUS_AUDIO_SYNTH:      return("syn"); break;
    case MUS_AUDIO_BASS:       return("ton"); break;
    case MUS_AUDIO_TREBLE:     return("ton"); break;
    case MUS_AUDIO_CD:         return("cd");  break;
    }
  return("oops");
}
#endif

int recorder_sort_mixer_device(void *wd, int i, int chan, bool input, int device, int *mixflds)
{
#if (HAVE_OSS || HAVE_ALSA)
  int k;
  /* we're moving right to left here, chan is counting down, we need to fill out MIXER|MUS_AUDIO_DAC_FILTER fields and channels */
  /* and also handle whatever else comes along */
  /* treble and bass are actually stereo -- we'll do both at once */
  if (!input)
    {
      if (mus_audio_api() == ALSA_API) 
	{
	  /* the existing code assumes there are tone controls and that
	   * a certain device is the output (MUS_AUDIO_DAC_OUT), for now
	   * no tone controls at all */
	  recorder_fill_wd(wd, 1, MUS_AUDIO_AMP, device);
	  return(device);
	}
      else
	{
	  /* count back from speaker 1 0 treble bass */
	  switch (i)
	    {
	    case 0: recorder_fill_wd(wd, 1, MUS_AUDIO_AMP, MUS_AUDIO_DAC_OUT);       break;
	    case 1: recorder_fill_wd(wd, 0, MUS_AUDIO_AMP, MUS_AUDIO_DAC_OUT);       break;
	    case 2: recorder_fill_wd(wd, 0, MUS_AUDIO_TREBLE, MUS_AUDIO_DAC_FILTER); break;
	    case 3: recorder_fill_wd(wd, 0, MUS_AUDIO_BASS, MUS_AUDIO_DAC_FILTER);   break;
	    }
	  if (i > 1) return(MUS_AUDIO_DAC_FILTER); else return(MUS_AUDIO_DAC_OUT);
	}
    }
  else
    {
      /* we want speaker/line-in gains on the far right, then whatever else */
      if (mixflds[MUS_AUDIO_MICROPHONE] > 0)
	{
	  recorder_fill_wd(wd, mixflds[MUS_AUDIO_MICROPHONE] - 1, MUS_AUDIO_AMP, MUS_AUDIO_MICROPHONE);
	  mixflds[MUS_AUDIO_MICROPHONE]--;
	  return(MUS_AUDIO_MICROPHONE);
	}
      else
	{
	  if (mixflds[MUS_AUDIO_LINE] > 0)
	    {
	      recorder_fill_wd(wd, mixflds[MUS_AUDIO_LINE] - 1, MUS_AUDIO_AMP, MUS_AUDIO_LINE_IN);
	      mixflds[MUS_AUDIO_LINE]--;
	      return(MUS_AUDIO_LINE);
	    }
	  else
	    {
	      for (k = 0; k < MAX_AUDIO_FIELD; k++)
		{
		  if (mixflds[k] > 0)
		    {
		      recorder_fill_wd(wd, mixflds[k] - 1, k, MUS_AUDIO_MIXER);
		      mixflds[k]--;
		      return(k);
		    }
		}
	    }
	}
    }
#else
  recorder_fill_wd(wd, chan, MUS_AUDIO_AMP, device);
#endif
  return(device);
}

int recorder_check_device(int system, int device, int *mixer_gains_posted, int *tone_controls_posted, int *mixflds, int *gains, bool *inp)
{
  int vu_meters = 0, num_gains;
  bool input;
#if (HAVE_OSS || HAVE_ALSA)
  int k;
  float mixer_field_chans[MAX_AUDIO_FIELD];
#endif
  vu_meters = device_channels(MUS_AUDIO_PACK_SYSTEM(system) | device);
  input = recorder_input_device(device);
  num_gains = device_gains(MUS_AUDIO_PACK_SYSTEM(system) | device);

#if (HAVE_OSS || HAVE_ALSA)
  if (num_gains == 0)
    {
      mixer_gains_posted[system] = 0;
      tone_controls_posted[system] = 0;
    }
  else
    {
      if ((input) && (!mixer_gains_posted[system]))
	{
	  for (k = 0; k < MAX_AUDIO_FIELD; k++) 
	    mixer_field_chans[k] = 0.0;
	  mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(system) | MUS_AUDIO_MIXER, 
			       MUS_AUDIO_FORMAT, 
			       MAX_AUDIO_FIELD, 
			       mixer_field_chans);
	  for (k = 0; k < MAX_AUDIO_FIELD; k++) 
	    mixflds[k] = (int)mixer_field_chans[k]; /* simplify life later */
	  mixer_gains_posted[system] = device_gains(MUS_AUDIO_PACK_SYSTEM(system) | MUS_AUDIO_MIXER);
	  num_gains = mixer_gains_posted[system]; /* includes the MUS_AUDIO_LINE_IN gains */
	}
      if ((!input) && (!tone_controls_posted[system]))
	{
	  tone_controls_posted[system] = device_gains(MUS_AUDIO_PACK_SYSTEM(system) | MUS_AUDIO_DAC_FILTER);
	  num_gains += tone_controls_posted[system];
	}
    }
#endif
  (*inp) = input;
  (*gains) = num_gains;
  return(vu_meters);
}

/* now recorder_info dependent code */

static recorder_info *rp = NULL;

static void init_recorder(void)
{
  int i;
  if (rp) return;
  rp = (recorder_info *)CALLOC(1, sizeof(recorder_info));
  rp->recording = false;
  rp->autoload = DEFAULT_RECORDER_AUTOLOAD;
  rp->buffer_size = DEFAULT_RECORDER_BUFFER_SIZE;
  rp->out_chans = DEFAULT_RECORDER_OUT_CHANS;
  rp->output_data_format = DEFAULT_RECORDER_OUT_FORMAT;
  rp->in_format = DEFAULT_RECORDER_IN_FORMAT;
  rp->srate = DEFAULT_RECORDER_SRATE;
  rp->trigger = DEFAULT_RECORDER_TRIGGER;
  rp->max_duration = DEFAULT_RECORDER_MAX_DURATION;
  if (DEFAULT_RECORDER_FILE != (char *)NULL) rp->output_file = copy_string(DEFAULT_RECORDER_FILE); else rp->output_file = NULL;
  rp->in_device = MUS_AUDIO_DEFAULT;
  rp->triggering = false;
  rp->triggered = true;
  rp->monitor_chans = 2;
  rp->monitor_port = -1;
  rp->monitoring = false;
  rp->taking_input = false;
  rp->systems = 1;
  rp->mixer_settings_saved = false;
#if LINUX || __bsdi__
  rp->output_header_type = MUS_RIFF;
#else
  rp->output_header_type = MUS_AIFC;
#endif
  rp->output_file_descriptor = -1;

  rp->out_amps = (Float *)CALLOC(MAX_OUT_CHANS, sizeof(Float));
  rp->mixer_gains = (Float *)CALLOC(MAX_MIXER_GAINS, sizeof(Float));
  rp->in_amps = (Float **)CALLOC(MAX_IN_CHANS, sizeof(Float *));
  for (i = 0; i < MAX_IN_CHANS; i++) rp->in_amps[i] = (Float *)CALLOC(MAX_OUT_CHANS, sizeof(Float));
  rp->chan_in_active = (bool *)CALLOC(MAX_IN_CHANS, sizeof(bool));
  rp->chan_out_active = (bool *)CALLOC(MAX_OUT_CHANS, sizeof(bool));
}

recorder_info *get_recorder_info(void) 
{
  init_recorder();
  return(rp);
}

bool record_in_progress(void)
{
  return((rp) && (record_dialog_is_active()) && (rp->recording));
}

int in_chans_active(void)
{
  int val = 0, i;
  for (i = 0; i < MAX_IN_CHANS; i++) 
    if (rp->chan_in_active[i]) 
      val++;
  return(val);
}

int out_chans_active(void)
{
  int val = 0, i;
  for (i = 0; i < MAX_OUT_CHANS; i++) 
    if (rp->chan_out_active[i]) 
      val++;
  return(val);
}

#if HAVE_RUBY
static char *b2s(bool val) {if (val) return("true"); else return("false");}
#else
static char *b2s(bool val) {if (val) return("#t"); else return("#f");}
#endif

static bool fneq(Float a, Float b) {return(fabs(a - b) > .00001);}

#if HAVE_RUBY
  #define TO_PROC_NAME(Str) xen_scheme_procedure_to_ruby(Str)
#endif

void save_recorder_state(FILE *fd)
{
  if (!rp) return;
#if HAVE_GUILE
  if (rp->autoload != DEFAULT_RECORDER_AUTOLOAD) fprintf(fd, "(set! (%s) %s)\n", S_recorder_autoload, b2s(rp->autoload));
  if (rp->buffer_size != DEFAULT_RECORDER_BUFFER_SIZE) fprintf(fd, "(set! (%s) %d)\n", S_recorder_buffer_size, rp->buffer_size);
  if (rp->out_chans != DEFAULT_RECORDER_OUT_CHANS) fprintf(fd, "(set! (%s) %d)\n", S_recorder_out_chans, rp->out_chans);
  if (rp->output_data_format != DEFAULT_RECORDER_OUT_FORMAT) 
    fprintf(fd, "(set! (%s) %s)\n", 
	    S_recorder_out_format, 
	    mus_data_format_to_constant_name(rp->output_data_format));
  fprintf(fd, "(set! (%s) %s)\n", 
	  S_recorder_out_type, 
	  mus_header_type_to_constant_name(rp->output_header_type));
  if (rp->in_format != DEFAULT_RECORDER_IN_FORMAT) 
    fprintf(fd, "(set! (%s) %s)\n", 
	    S_recorder_in_format, 
	    mus_data_format_to_constant_name(rp->in_format));
  if (rp->in_device != MUS_AUDIO_DEFAULT) fprintf(fd, "(set! (%s) %d)\n", S_recorder_in_device, rp->in_device);
  if (rp->srate != DEFAULT_RECORDER_SRATE) fprintf(fd, "(set! (%s) %d)\n", S_recorder_srate, rp->srate);
  if (rp->output_file != NULL) fprintf(fd, "(set! (%s) \"%s\")\n", S_recorder_file, rp->output_file);
  if (fneq(rp->trigger, DEFAULT_RECORDER_TRIGGER)) fprintf(fd, "(set! (%s) %.4f)\n", S_recorder_trigger, rp->trigger);
  if (fneq(rp->max_duration, DEFAULT_RECORDER_MAX_DURATION)) fprintf(fd, "(set! (%s) %.4f)\n", S_recorder_max_duration, rp->max_duration);
#endif
#if HAVE_RUBY
  if (rp->autoload != DEFAULT_RECORDER_AUTOLOAD) fprintf(fd, "set_%s %s\n", TO_PROC_NAME(S_recorder_autoload), b2s(rp->autoload));
  if (rp->buffer_size != DEFAULT_RECORDER_BUFFER_SIZE) fprintf(fd, "set_%s %d\n", TO_PROC_NAME(S_recorder_buffer_size), rp->buffer_size);
  if (rp->out_chans != DEFAULT_RECORDER_OUT_CHANS) fprintf(fd, "set_%s %d\n", TO_PROC_NAME(S_recorder_out_chans), rp->out_chans);
  if (rp->output_data_format != DEFAULT_RECORDER_OUT_FORMAT) 
    fprintf(fd, "set_%s %s\n", 
	    TO_PROC_NAME(S_recorder_out_format), 
	    mus_data_format_to_constant_name(rp->output_data_format));
  fprintf(fd, "set_%s %s\n", 
	  TO_PROC_NAME(S_recorder_out_type), 
	  mus_header_type_to_constant_name(rp->output_header_type));
  if (rp->in_format != DEFAULT_RECORDER_IN_FORMAT) 
    fprintf(fd, "set_%s %s\n", 
	    TO_PROC_NAME(S_recorder_in_format),
	    mus_data_format_to_constant_name(rp->in_format));
  if (rp->in_device != MUS_AUDIO_DEFAULT) fprintf(fd, "set_%s %d\n", TO_PROC_NAME(S_recorder_in_device), rp->in_device);
  if (rp->srate != DEFAULT_RECORDER_SRATE) fprintf(fd, "set_%s %d\n", TO_PROC_NAME(S_recorder_srate), rp->srate);
  if (rp->output_file != NULL) fprintf(fd, "set_%s \"%s\"\n", TO_PROC_NAME(S_recorder_file), rp->output_file);
  if (fneq(rp->trigger, DEFAULT_RECORDER_TRIGGER)) fprintf(fd, "set_%s %.4f\n", TO_PROC_NAME(S_recorder_trigger), rp->trigger);
  if (fneq(rp->max_duration, DEFAULT_RECORDER_MAX_DURATION)) fprintf(fd, "set_%s %.4f\n", TO_PROC_NAME(S_recorder_max_duration), rp->max_duration);
#endif
}

static char numbuf[LABEL_BUFFER_SIZE];
char *channel_name(int in_chans, int out_chans, int chan)
{
  bool use_numbers;
  use_numbers = ((out_chans > 4) || (in_chans > 4));
  if (use_numbers)
    mus_snprintf(numbuf, LABEL_BUFFER_SIZE, "%d", chan + 1);
  else mus_snprintf(numbuf, LABEL_BUFFER_SIZE, "%c", (char)('A' + chan));
  return(numbuf);
}

char *gain_channel_name(int in_chans, int out_chans, bool input, int dev_in, int out)
{
  bool use_numbers;
  if (input)
    {
      use_numbers = ((out_chans > 4) || (in_chans > 4));
      if (use_numbers)
	mus_snprintf(numbuf, LABEL_BUFFER_SIZE, "%d->%d:", dev_in + 1, out + 1);
      else mus_snprintf(numbuf, LABEL_BUFFER_SIZE, "%c->%c:", (char)('A' + dev_in), (char)('A' + out));
    }
  else
    {
      use_numbers = (out_chans > 4);
      if (use_numbers)
	mus_snprintf(numbuf, LABEL_BUFFER_SIZE, "%d:", out + 1);
      else mus_snprintf(numbuf, LABEL_BUFFER_SIZE, "%c:", (char)('A' + out));
    }
  return(numbuf);
}

Float mixer_gain(int system, int device, int chan, int gain, int field)
{
  float g[1];
  mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(system) | (device), field, chan, g);
  if (gain > rp->num_mixer_gains) 
    snd_error(_("gain (slider) number too high: %d > %d"),
	      gain, rp->num_mixer_gains);
  else rp->mixer_gains[gain] = g[0];
  return(g[0]);
}

void set_mixer_gain(int system, int device, int chan, int gain, int field, Float amp) 
{
  float g[1];
  g[0] = amp;
  if (device == MUS_AUDIO_DAC_FILTER) /* bass or treble control affects both channels at once */
    {
      mus_audio_mixer_write(MUS_AUDIO_PACK_SYSTEM(system) | (device), field, 0, g);
      mus_audio_mixer_write(MUS_AUDIO_PACK_SYSTEM(system) | (device), field, 1, g);
    }
  else 
    mus_audio_mixer_write(MUS_AUDIO_PACK_SYSTEM(system) | (device), field, chan, g);
  if (gain > rp->num_mixer_gains) 
    snd_error(_("gain (slider) number too high: %d > %d"),
	      gain, rp->num_mixer_gains);
  else rp->mixer_gains[gain] = amp;
}


void recorder_set_audio_srate(int device, int srate, int system, bool aud)
{
  float g[1];
#if (!NEW_SGI_AL)
  if (aud) close_recorder_audio();
  g[0] = (Float)srate;
  mus_audio_mixer_write(MUS_AUDIO_PACK_SYSTEM(system) | device, MUS_AUDIO_SRATE, 0, g);
  if (aud) fire_up_recorder();
#endif
}

#if OLD_SGI_AL
void set_line_source(bool in_digital)
{
  int aud, err;
  aud = rp->taking_input;
  if (aud) close_recorder_audio();
  rp->input_channels[0] = ((in_digital) ? 2 : 4);
  rp->monitor_chans = rp->input_channels[0];
  err = mus_audio_mixer_write(MUS_AUDIO_DEFAULT, 
			      MUS_AUDIO_PORT, 
			      ((in_digital) ? MUS_AUDIO_DIGITAL_IN : MUS_AUDIO_MICROPHONE), 
			      NULL);
  if (err == MUS_ERROR) 
    recorder_error(_("set input source: "));
  rp->input_channel_active[0] = (!in_digital); 
  rp->input_channel_active[1] = (!in_digital);
  rp->input_channel_active[2] = (!in_digital); 
  rp->input_channel_active[3] = (!in_digital);
  rp->input_channel_active[4] = in_digital; 
  rp->input_channel_active[5] = in_digital;
  if (aud) fire_up_recorder();
}
#endif


void set_record_size (int new_size)
{
  int i;
  lock_recording_audio();
  if (new_size > rp->buffer_size)
    {
      rp->buffer_size = new_size;
      if (rp->one_system_input_buf)
	{
	  FREE(rp->one_system_input_buf);
	  rp->one_system_input_buf = (mus_sample_t *)CALLOC(new_size, sizeof(mus_sample_t));
	}
      if (rp->output_bufs)
	for (i = 0; i < rp->out_chans; i++) 
	  if (rp->output_bufs[i]) 
	    {
	      FREE(rp->output_bufs[i]);
	      rp->output_bufs[i] = (mus_sample_t *)CALLOC(new_size, sizeof(mus_sample_t));
	    }
      for (i = 0; i < rp->systems; i++)
	if (rp->raw_input_bufs[i]) 
	  {
	    FREE(rp->raw_input_bufs[i]);
	    rp->raw_input_bufs[i] = (char *)CALLOC(new_size, sizeof(int));
	  }
      if (rp->all_systems_input_buf) 
	{
	  FREE(rp->all_systems_input_buf);
	  rp->system_input_buffer_size = new_size;
	  rp->all_systems_input_buf = (mus_sample_t *)CALLOC(rp->system_input_buffer_size, sizeof(mus_sample_t));
	}
    }
  else rp->buffer_size = new_size;
  reflect_record_size(rp->buffer_size);
  unlock_recording_audio();
}

#if (!SUN)
static void get_input_channels(int i)
{
  rp->input_channels[i] = device_channels(MUS_AUDIO_PACK_SYSTEM(i) | rp->in_device);
  if (rp->input_channels[i] == 0)
    {
      rp->input_channels[i] = device_channels(MUS_AUDIO_PACK_SYSTEM(i) | MUS_AUDIO_DEFAULT);
      if (rp->input_channels[i] == 0)
	{
	  rp->input_channels[i] = device_channels(MUS_AUDIO_PACK_SYSTEM(i) | MUS_AUDIO_MICROPHONE);
	  if (rp->input_channels[i] == 0)
	    {
	      rp->input_channels[i] = device_channels(MUS_AUDIO_PACK_SYSTEM(i) | MUS_AUDIO_LINE_IN);
	      if (rp->input_channels[i] == 0)
		{
		  rp->input_channels[i] = device_channels(MUS_AUDIO_PACK_SYSTEM(i) | MUS_AUDIO_ADAT_IN);
		  if (rp->input_channels[i] == 0)
		    {
		      rp->input_channels[i] = device_channels(MUS_AUDIO_PACK_SYSTEM(i) | MUS_AUDIO_DIGITAL_IN);
		      if (rp->input_channels[i] == 0)
			{
			  rp->input_channels[i] = device_channels(MUS_AUDIO_PACK_SYSTEM(i) | MUS_AUDIO_SPDIF_IN);
			  if (rp->input_channels[i] == 0)
			    rp->input_channels[i] = device_channels(MUS_AUDIO_PACK_SYSTEM(i) | MUS_AUDIO_AES_IN);
			}}}}}}}

static void get_input_devices(void)
{
  int i;
  for (i = 0; i < rp->systems; i++)
    {
      rp->input_ports[i] = mus_audio_open_input(MUS_AUDIO_PACK_SYSTEM(i) | rp->in_device,
						rp->srate,
						rp->input_channels[i],
						rp->in_format,
						rp->buffer_size);
      if (rp->input_ports[i] == -1)
	{
	  rp->input_ports[i] = mus_audio_open_input(MUS_AUDIO_PACK_SYSTEM(i) | MUS_AUDIO_DUPLEX_DEFAULT,
						    rp->srate,
						    rp->input_channels[i],
						    rp->in_format,
						    rp->buffer_size);
	  if (rp->input_ports[i] == -1) /* perhaps not full-duplex */
	    {
	      rp->input_ports[i] = mus_audio_open_input(MUS_AUDIO_PACK_SYSTEM(i) | MUS_AUDIO_DEFAULT,
							rp->srate,
							rp->input_channels[i],
							rp->in_format,
							rp->buffer_size);
	    }
	}
    }
}
#endif

#if (HAVE_ALSA || HAVE_OSS)
void fire_up_recorder(void)
{
  int i, j;
  float val[64];
  float direction = 0.0;
  int size, sys, dev, sysdev, in_count;
  int err;
  init_recorder();
  if (!rp->all_systems_input_buf) 
    {
      rp->system_input_buffer_size = rp->buffer_size;
      rp->all_systems_input_buf = (mus_sample_t *)CALLOC(rp->system_input_buffer_size, sizeof(mus_sample_t));
    }
  for (i = 0; i < rp->systems; i++) 
    rp->input_ports[i] = -1;
  if (rp->mixer_settings_saved) 
    mus_audio_restore(); 
  else mus_audio_save();
  if (rp->srate <= 0) 
    rp->srate = DEFAULT_RECORDER_SRATE;
  rp->monitor_chans = 2;
  for (i = 0; i < rp->possible_input_chans; i++) 
    rp->input_channel_active[i] = true; 

  if (mus_audio_api() == ALSA_API) 
    {
      /* ALSA_API: Select first input device for each system */

      /* FIXME: there is one srate, format and so on for each system. The current code
       * is not checking for them being compatible. At least srate and buffer length
       * have to be shared by all enabled recording devices. The code should check for
       * that and somehow disable devices that are incompatible. The previous global
       * state variables are being set with the results of the first system scanned */

      in_count = 0;
      for (i = 0; i < rp->ordered_devices_size; i++)
	{
	  sys = rp->ordered_systems[i];
	  dev = rp->ordered_devices[i];
	  sysdev = MUS_AUDIO_PACK_SYSTEM(sys)|dev;
	  if ((err = mus_audio_mixer_read(sysdev, MUS_AUDIO_DIRECTION, 0, &direction)) == MUS_NO_ERROR) 
	    {
	      if (rp->input_channels[sys] == 0 && (int)direction == 1)
		{
		  if ((err = mus_audio_mixer_read(sysdev, MUS_AUDIO_CHANNEL, 2, val)) == MUS_NO_ERROR) 
		    {
		      rp->input_channels[sys] = (int)val[0];
		      if (rp->input_channels[sys] > MAX_IN_CHANS)
			rp->input_channels[sys] = MAX_IN_CHANS;
		    }
		  if ((err = mus_audio_mixer_read(sysdev, MUS_AUDIO_SRATE, 2, val)) == MUS_NO_ERROR)
		    {
		      rp->input_srates[sys] = (int)val[0];
		      if (i == 0) 
			set_recorder_srate(rp, rp->input_srates[sys]);
		    }
		  rp->input_formats[sys] = mus_audio_compatible_format(sysdev);
		  if (i == 0) 
		    rp->in_format = rp->input_formats[sys];
		  if ((err = mus_audio_mixer_read(sysdev, MUS_AUDIO_SAMPLES_PER_CHANNEL, 0, val)) == MUS_NO_ERROR)
		    {
		      rp->input_buffer_sizes[sys] = (int)(val[0]);
		      if (i == 0) 
			{
			  rp->buffer_size = rp->input_buffer_sizes[sys];
			  reflect_record_size(rp->buffer_size);
			}
		      if (!(rp->raw_input_bufs[sys]))
			rp->raw_input_bufs[sys] = (char *)CALLOC(rp->input_buffer_sizes[sys] * rp->input_channels[sys], sizeof(int));
		    }
		}
	      if (rp->input_channels[sys] > 0)
		in_count += rp->input_channels[sys];
	    }
	}
      if (in_count == 0) 
	{
	  recorder_error(_("no inputs?: "));
	  return;
	}

      /* open all input devices */
      for (i = 0; i < rp->ordered_devices_size; i++)
	{
	  sys = rp->ordered_systems[i];
	  dev = rp->ordered_devices[i];
	  sysdev = MUS_AUDIO_PACK_SYSTEM(sys)|dev;
	  if ((err = mus_audio_mixer_read(sysdev, MUS_AUDIO_DIRECTION, 0, &direction)) == MUS_NO_ERROR) 
	    {
	      if ((int)direction == 1)
		{
		  size = mus_bytes_per_sample(rp->input_formats[sys]);
		  rp->input_ports[sys] = mus_audio_open_input(sysdev,
							      rp->srate,
							      rp->input_channels[sys],
							      rp->input_formats[sys],
							      rp->input_buffer_sizes[sys] * rp->input_channels[sys] * size);
		  if (rp->input_ports[sys] == -1)
		    {
		      recorder_error(_("open device: "));
		      for (j = 0; j < rp->ordered_devices_size; j++)
			{
			  sys = rp->ordered_systems[j];
			  if (rp->input_ports[sys] != -1)
			    {
			      mus_audio_close(rp->input_ports[sys]);
			      rp->input_ports[sys] = -1;
			    }
			}
		      return;
		    }
		}
	    }
	}
      rp->taking_input = true;

      /* search for output devices for rp->monitoring, first one wins */

      for (i = 0; i < rp->ordered_devices_size; i++)
	{
	  sys = rp->ordered_systems[i];
	  dev = rp->ordered_devices[i];
	  sysdev = MUS_AUDIO_PACK_SYSTEM(sys)|dev;
	  if ((err = mus_audio_mixer_read(sysdev, MUS_AUDIO_DIRECTION, 0, &direction)) == MUS_NO_ERROR) 
	    {
	      if ((int)direction == 0)
		{
		  /* found the first pane that has an output device (must be the only one) */
		  if ((err = mus_audio_mixer_read(sysdev, MUS_AUDIO_CHANNEL, 2, val)) == MUS_NO_ERROR) 
		    {
		      rp->monitor_chans = (int)(val[0]);
		      /* FIXME: what would be the proper value for this? 
		       * the computer wedges if I don't initialize it, why? */
		      if (rp->monitor_chans > MAX_OUT_CHANS)
			rp->monitor_chans = MAX_OUT_CHANS;
		      rp->out_chans = rp->monitor_chans;
		    }
		  rp->monitor_data_format = mus_audio_compatible_format(sysdev);
		  size = mus_bytes_per_sample(rp->monitor_data_format);
		  rp->monitor_port = mus_audio_open_output(sysdev,
							   rp->srate,
							   rp->monitor_chans,
							   rp->monitor_data_format,
							   rp->buffer_size * rp->monitor_chans * size);
		  if (rp->monitor_port != -1) 
		    {
		      if (!rp->output_bufs)
			rp->output_bufs = (mus_sample_t **)CALLOC(MAX_OUT_CHANS, sizeof(mus_sample_t *));
		      for (i = 0; i < rp->monitor_chans; i++) 
			{
			  if (rp->output_bufs[i]) 
			    FREE(rp->output_bufs[i]);
			  rp->output_bufs[i] = (mus_sample_t *)CALLOC(rp->buffer_size, sizeof(mus_sample_t));
			}
		      if (!(rp->monitor_buf))
			rp->monitor_buf = (char *)CALLOC(rp->buffer_size * rp->monitor_chans * size, 1);
		      rp->monitoring = true;
		    }
		  else
		    {
		      recorder_error(_("open output: "));
		      rp->monitoring = false;
		    }
		  break;
		}
	    }
	}
    }
  else
    {
      /* OSS_API */
      for (i = 0; i < rp->systems; i++)
	{
	  if (!(rp->raw_input_bufs[i]))
	    rp->raw_input_bufs[i] = (char *)CALLOC(rp->buffer_size, sizeof(int)); /* 4 bytes per sample is probably enough?? */
	  rp->input_formats[i] = rp->in_format;
	  rp->input_buffer_sizes[i] = rp->buffer_size / rp->out_chans;
	}
      for (i = 0; i < rp->systems; i++)
	get_input_channels(i);
      err = 1;
      for (i = 0; i < rp->systems; i++) 
	if (rp->input_channels[i] > 0) 
	  {
	    err = 0; 
	    break;
	  }
      if (err)
	{
	  recorder_error(_("no inputs?: "));
	  return;
	}
      /* if adat, aes etc, make choices about default on/off state, open monitor separately (and write) */
      get_input_devices();
      if (rp->input_ports[0] == -1)
	{
	  recorder_error(_("open device: "));
	  return;
	}
      rp->taking_input = true;
      /* rp->monitor_port = 0; */

      /*
       * if (full_duplex(0))
       *   rp->monitor_port = mus_audio_open_output(MUS_AUDIO_DUPLEX_DEFAULT, rp->srate, rp->monitor_chans, rp->output_data_format, rp->buffer_size);
       */

      if (rp->monitor_port == -1)
	{
	  recorder_error(_("open output: "));
	  rp->monitoring = false;
	}
      else rp->monitoring = true;
    }
  set_read_in_progress();
}

#else /* not ALSA or OSS */

void fire_up_recorder(void)
{
  int i;
#if NEW_SGI_AL
  int j, n;
#endif
  float val[8];
  int err, new_srate = 0;
#ifdef SGI
  int cur_dev;
  long sb[8];
#endif

  for (i = 0; i < rp->systems; i++)
    if (!(rp->raw_input_bufs[i]))
      rp->raw_input_bufs[i] = (char *)CALLOC(rp->buffer_size, sizeof(int)); /* 4 bytes per sample is probably enough?? */
  if (!rp->all_systems_input_buf) 
    {
      rp->system_input_buffer_size = rp->buffer_size;
      rp->all_systems_input_buf = (mus_sample_t *)CALLOC(rp->system_input_buffer_size, sizeof(mus_sample_t));
    }
  for (i = 0; i < rp->systems; i++) rp->input_ports[i] = -1;
  mus_audio_save();

  /* the recorder srate sometimes depends purely on external devices */
  
#ifdef SGI
  cur_dev = MUS_AUDIO_MICROPHONE;
  #if OLD_SGI_AL
    sb[0] = AL_INPUT_SOURCE;
    err = ALgetparams(AL_DEFAULT_DEVICE, sb, 2);
    if (!err)
      {
	if (sb[1] == AL_INPUT_LINE)
	  cur_dev = MUS_AUDIO_LINE_IN;
	else
	  if (sb[1] == AL_INPUT_DIGITAL)
	    cur_dev = MUS_AUDIO_DIGITAL_IN;
	if (cur_dev == MUS_AUDIO_DIGITAL_IN)
	  sb[0] = AL_DIGITAL_INPUT_RATE;
	else sb[0] = AL_INPUT_RATE;
	err = ALgetparams(AL_DEFAULT_DEVICE, sb, 2);
	if (!err)
	  new_srate = sb[1];
	if (cur_dev == MUS_AUDIO_DIGITAL_IN) 
	  rp->input_channels[0] = 2;
	else rp->input_channels[0] = 4;
      }
    else
      {
	err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_MICROPHONE, MUS_AUDIO_SRATE, 0, val);
	if (!err) rp->srate = (int)val[0];
      }
    if ((new_srate > 0) && (new_srate != rp->srate)) set_recorder_srate(rp, new_srate);
    rp->monitor_chans = rp->input_channels[0];
    for (i = 0; i < 4; i++) 
      {
        rp->input_channel_active[i] = (cur_dev != MUS_AUDIO_DIGITAL_IN);
      }
    rp->input_channel_active[4] = (cur_dev == MUS_AUDIO_DIGITAL_IN);
    rp->input_channel_active[5] = (cur_dev == MUS_AUDIO_DIGITAL_IN);
    if (cur_dev != MUS_AUDIO_DIGITAL_IN) 
      mus_audio_mixer_write(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_MICROPHONE, MUS_AUDIO_CHANNEL, 2, NULL);
  #endif
  #if NEW_SGI_AL
    err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_MICROPHONE, MUS_AUDIO_SRATE, 0, val);
    new_srate = (int)val[0];
    if (err == MUS_NO_ERROR) 
      if ((new_srate > 0) && (rp->srate != new_srate)) 
	set_recorder_srate(rp, new_srate);
    rp->monitor_chans = 2;
  #endif
#else /* not SGI */
    if (rp->srate <= 0) rp->srate = 22050;
  #ifdef SUN
    /* turn on "monitor" */
    val[0] = 1.0;
    mus_audio_mixer_write(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_MICROPHONE, MUS_AUDIO_IGAIN, 0, val);
    rp->input_channel_active[0] = true;
    #if HAVE_SYS_MIXER_H
      rp->input_channel_active[1] = true;
    #else
      rp->input_channel_active[1] = false;
    #endif
  #else
    err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | rp->in_device, MUS_AUDIO_SRATE, 0, val);
    if (err == MUS_NO_ERROR) 
      {
	new_srate = (int)val[0];
	if ((new_srate > 0) && (rp->srate != new_srate)) 
	  set_recorder_srate(rp, new_srate);
      }
    rp->monitor_chans = 2;
    for (i = 0; i < rp->possible_input_chans; i++) 
      {
	rp->input_channel_active[i] = true; 
      }
  #endif
#endif

  for (i = 0; i < rp->systems; i++)
    {
      if (rp->input_channels[i] == 0)
	{
#if OLD_SGI_AL
	  rp->input_channels[i] = ((cur_dev == MUS_AUDIO_DIGITAL_IN) ? 2 : 4);
	  rp->monitor_chans = rp->input_channels[i];
#else
  #ifdef SUN
	  rp->input_channels[i] = device_channels(MUS_AUDIO_PACK_SYSTEM(i) | MUS_AUDIO_MICROPHONE);
  #else
	  get_input_channels(i);
  #endif
#endif
	}
    }

  err = 1;
  for (i = 0; i < rp->systems; i++) if (rp->input_channels[i] > 0) {err = 0; break;}
  if (err)
    {
      recorder_error(_("no inputs?: "));
      return;
    }

#if NEW_SGI_AL || MAC_OSX
  rp->input_ports[0] = mus_audio_open_input(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_MICROPHONE,
					    rp->srate,
					    rp->input_channels[0],
					    rp->in_format,
					    rp->buffer_size);
#else
  #if OLD_SGI_AL
    rp->input_ports[0] = mus_audio_open_input(MUS_AUDIO_PACK_SYSTEM(0) | cur_dev,
					      rp->srate,
					      rp->input_channels[0],
					      rp->in_format,
					      rp->buffer_size);
  #else
    #ifdef SUN
    rp->input_ports[0] = mus_audio_open_input(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_MICROPHONE,
					      rp->srate,
					      rp->input_channels[0],
					      rp->in_format,
					      rp->buffer_size);
    #else
    /* if adat, aes etc, make choices about default on/off state, open monitor separately (and write) */

    get_input_devices();
    #endif
  #endif
#endif
  if (rp->input_ports[0] == -1)
    {
      recorder_error(_("open device: "));
      return;
    }
  rp->taking_input = true;
#if HAVE_OSS || SUN || MAC_OSX
  /* rp->monitor_port = 0; */

  /*
   * if (full_duplex(0))
   *   rp->monitor_port = mus_audio_open_output(MUS_AUDIO_DUPLEX_DEFAULT, rp->srate, rp->monitor_chans, rp->output_data_format, rp->buffer_size);
   */

#else
  rp->monitor_port = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_DAC_OUT,
					   rp->srate,
					   rp->monitor_chans,
					   rp->output_data_format,
					   rp->buffer_size);
#endif
  if (rp->monitor_port == -1)
    {
      recorder_error(_("open output: "));
      rp->monitoring = false;
    }
  else rp->monitoring = true;
  set_read_in_progress();
}
#endif

void close_recorder_audio(void) 
{
  int i;
  if (rp->taking_input)
    {
      for (i = 0; i < rp->systems; i++)
	if (rp->input_ports[i] != -1)
	  {
	    mus_audio_close(rp->input_ports[i]);
	    rp->input_ports[i] = -1;
	  }
      rp->taking_input = false;
    }
  if (rp->recorder_reader) 
    {
      BACKGROUND_REMOVE(rp->recorder_reader);
      rp->recorder_reader = 0;
    }
  if (rp->monitoring)
    {
      mus_audio_close(rp->monitor_port);
      rp->monitoring = false;
    }
}


void recorder_characterize_devices(int devs, int output_devices)
{
#if (HAVE_ALSA || HAVE_OSS)
  float direction = 0.0;
  int err;
#endif
  int i, k, def_out, system, cur_devices, device, n;
  float audval[AUDVAL_SIZE];
  rp->ordered_devices_size = devs;
  rp->ordered_devices = (int *)CALLOC(rp->ordered_devices_size, sizeof(int));
  rp->ordered_systems = (int *)CALLOC(rp->ordered_devices_size, sizeof(int));
  rp->possible_input_chans = 0;
  def_out = 2;
  k = 0;
  for (system = 0; system < rp->systems; system++)
    {
      mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(system) | MUS_AUDIO_DEFAULT, MUS_AUDIO_PORT, AUDVAL_SIZE, audval);
      cur_devices = (int)(audval[0]);
      for (i = 0; i < cur_devices; i++)
	{
#if (HAVE_ALSA || HAVE_OSS)
	  device = (int)audval[i + 1];
	  /* FIXME: have not looked to see if oss sndlib supports MUS_AUDIO_DIRECTION */
	  if ((mus_audio_api() == ALSA_API && 
	       ((err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(system)|device, MUS_AUDIO_DIRECTION, 0, &direction)) == MUS_NO_ERROR) &&
	       (int)direction == 1) 
	      ||
	      (mus_audio_api() == OSS_API &&
	       recorder_input_device(device)))
#else
	    device = (int)audval[i + 1];
	  if (recorder_input_device(device))
#endif
	    {
	      n = device_channels(MUS_AUDIO_PACK_SYSTEM(system) | device);
	      rp->possible_input_chans += n;
	      if (n > def_out) def_out = n;
	      rp->ordered_devices[k] = device;
	      rp->ordered_systems[k] = system;
	      if (device == MUS_AUDIO_DIGITAL_IN) rp->digital_in_button = k;
	      else if (device == MUS_AUDIO_MICROPHONE) rp->microphone_button = k;
	      else if (device == MUS_AUDIO_LINE_IN) rp->line_in_button = k;
	      k++;
	    }
	}
    }
#if (HAVE_ALSA || HAVE_OSS)
  if (mus_audio_api() == ALSA_API) 
    {
      /* search for output devices, first one wins, previous code
       * was assuming one particular device existed */
      for (system = 0; system < rp->systems; system++)
	{
	  mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(system)|MUS_AUDIO_DEFAULT, MUS_AUDIO_PORT, AUDVAL_SIZE, audval);
	  cur_devices = (int)(audval[0]);
	  for (i = 0; i < cur_devices; i++)
	    {
	      direction = 0.0;
	      device = (int)audval[i + 1];
	      if ((err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(system)|device, MUS_AUDIO_DIRECTION, 0, &direction)) == MUS_NO_ERROR) 
		{
		  if ((int)direction == 0)
		    {
		      rp->ordered_devices[k] = device;
		      rp->ordered_systems[k] = system;
		      return;
		    }
		}
	    }
	}
    }
  else
    {
      if (output_devices) rp->ordered_devices[k] = MUS_AUDIO_DAC_OUT;
    }
#else
  if (output_devices) rp->ordered_devices[k] = MUS_AUDIO_DAC_OUT;
#endif
}

#if (HAVE_ALSA || HAVE_OSS)

static Cessate read_adc(void) 
{
  int in_chan, out_chan, i, k, m, n, out_frame, inchn, offset, active_in_chans, ochns, sr, buffer_size, in_datum_size;
  mus_sample_t val;
  mus_sample_t *input_bufs[1];
  out_frame = 0;
  ochns = rp->out_chans;
  sr = rp->srate;
  if (rp->systems == 1)
    {
      active_in_chans = rp->input_channels[0];
      buffer_size = rp->input_buffer_sizes[0] * rp->input_channels[0];
      if (ochns > active_in_chans) buffer_size /= ochns;
      if (rp->system_input_buffer_size < buffer_size)
	{
	  if (rp->all_systems_input_buf) FREE(rp->all_systems_input_buf);
	  rp->system_input_buffer_size = buffer_size;
	  rp->all_systems_input_buf = (mus_sample_t *)CALLOC(rp->system_input_buffer_size, sizeof(mus_sample_t));
	}
      in_datum_size = mus_bytes_per_sample(rp->input_formats[0]);
      mus_audio_read(rp->input_ports[0], rp->raw_input_bufs[0], buffer_size * in_datum_size);
      input_bufs[0] = rp->all_systems_input_buf;
      mus_file_read_buffer(rp->input_formats[0], 0, 1, buffer_size, input_bufs, rp->raw_input_bufs[0]);
    }
  else
    {
      active_in_chans = 0;
      /* rp->possible_input_chans is a count of all possible input channels, some of which may be incompatible */
      /* rp->input_channels[i] is how many of these channels can be active at once on a given system */
      for (i = 0; i < rp->systems; i++) active_in_chans += rp->input_channels[i];
      offset = 0;
      buffer_size = 0;
      for (i = 0; i < rp->systems; i++) 
	buffer_size += rp->input_buffer_sizes[i] * rp->input_channels[i];
      if (ochns > active_in_chans) buffer_size /= ochns;
      if (rp->system_input_buffer_size < buffer_size)
	{
	  rp->system_input_buffer_size = buffer_size;
	  if (rp->all_systems_input_buf) FREE(rp->all_systems_input_buf);
	  rp->all_systems_input_buf = (mus_sample_t *)CALLOC(rp->system_input_buffer_size, sizeof(mus_sample_t));
	  if (rp->one_system_input_buf) FREE(rp->one_system_input_buf);
	  rp->one_system_input_buf = (mus_sample_t *)CALLOC(rp->system_input_buffer_size, sizeof(mus_sample_t));
	}
      if (rp->one_system_input_buf == NULL) rp->one_system_input_buf = (mus_sample_t *)CALLOC(buffer_size, sizeof(mus_sample_t));
      input_bufs[0] = rp->one_system_input_buf;
      for (i = 0; i < rp->systems; i++)
	{
	  in_datum_size = mus_bytes_per_sample(rp->input_formats[i]);
	  mus_audio_read(rp->input_ports[i], 
			 rp->raw_input_bufs[i], 
			 rp->input_buffer_sizes[i] * rp->input_channels[i] * in_datum_size);
	  mus_file_read_buffer(rp->input_formats[i], 
			       0, 1, 
			       rp->input_buffer_sizes[i] * rp->input_channels[i], 
			       input_bufs, 
			       rp->raw_input_bufs[i]);
	  for (k = 0, m = offset; m < buffer_size; m += active_in_chans) 
	    for (n = 0; n < rp->input_channels[i]; n++) 
	      rp->all_systems_input_buf[m + n] = rp->one_system_input_buf[k++];
	  offset += rp->input_channels[i];
	}
    }
  for (i = 0; i < rp->possible_input_chans; i++)
    rp->input_vu_maxes[i] = MUS_SAMPLE_0;
  for (i = 0; i < ochns; i++) 
    rp->output_vu_maxes[i] = MUS_SAMPLE_0;

  /* run through input devices looking for any that are currently turned on */
  /* for each channel currently on, get its associated input channel */

  for (i = 0, out_frame = 0; i < buffer_size; i += active_in_chans, out_frame++)
    {
      for (out_chan = 0; out_chan < ochns; out_chan++) 
	rp->unscaled_output_bufs[out_chan] = MUS_SAMPLE_0;
      inchn = 0;
      for (in_chan = 0; in_chan < rp->possible_input_chans; in_chan++)
	{
	  if (rp->input_channel_active[in_chan])
	    {
	      val = rp->all_systems_input_buf[i + inchn];
	      if (rp->chan_in_active[in_chan])
		for (out_chan = 0; out_chan < ochns; out_chan++)
		  rp->unscaled_output_bufs[out_chan] += (mus_sample_t)(rp->in_amps[in_chan][out_chan] * val);
	      if (val < MUS_SAMPLE_0) val = -val; 
	      if (val > rp->input_vu_maxes[in_chan]) rp->input_vu_maxes[in_chan] = val;
	      inchn++;
	    }
	}
      for (out_chan = 0; out_chan < ochns; out_chan++)
	{
	  val = (mus_sample_t)(rp->unscaled_output_bufs[out_chan] * rp->out_amps[out_chan]);
	  if ((rp->recording) && 
	      (rp->chan_out_active[out_chan])) 
	    rp->output_bufs[out_chan][out_frame] = val;
	  if (val < MUS_SAMPLE_0) val = -val;
	  if (val > rp->output_vu_maxes[out_chan]) 
	    rp->output_vu_maxes[out_chan] = val;
	}
    }
  for (in_chan = 0; in_chan < rp->possible_input_chans; in_chan++)
    if (rp->input_channel_active[in_chan])
      recorder_set_vu_in_val(in_chan, rp->input_vu_maxes[in_chan]);
  for (out_chan = 0; out_chan < ochns; out_chan++)
    {
      recorder_set_vu_out_val(out_chan, rp->output_vu_maxes[out_chan]);
      if ((!rp->triggered) && 
	  (MUS_SAMPLE_TO_FLOAT(rp->output_vu_maxes[out_chan]) > rp->trigger)) 
	rp->triggered = true;
    }
  if ((rp->monitoring) && (rp->output_bufs) && (ochns <= rp->monitor_chans))
    {
      /* opened in rp->output_data_format and rp->monitor_chans */
      mus_file_write_buffer(rp->monitor_data_format, 0, out_frame - 1, rp->monitor_chans, rp->output_bufs, rp->monitor_buf, data_clipped(ss));
      mus_audio_write(rp->monitor_port, 
		      rp->monitor_buf, 
		      rp->buffer_size * rp->monitor_chans * mus_bytes_per_sample(rp->monitor_data_format));
    }
  if ((rp->recording) && (rp->triggered))
    {
      mus_file_write(rp->output_file_descriptor, 0, out_frame - 1, ochns, rp->output_bufs);
      rp->total_output_frames += out_frame;
      if (rp->total_output_frames > rp->duration_label_update_frames)
	{
	  reflect_recorder_duration((Float)((double)(rp->total_output_frames) / (Float)sr));
	  rp->duration_label_update_frames += (sr / 4);
	}
    }
  return(((rp->total_output_frames / sr) >= rp->max_duration) ? BACKGROUND_QUIT : BACKGROUND_CONTINUE);
}

#else
/* not ALSA or OSS */

static Cessate read_adc(void)
{
  int in_chan, out_chan, i, k, m, n, out_frame, inchn, offset, active_in_chans, cur_size, ochns, sr, sz, ifmt, in_datum_size;
  mus_sample_t val;
  mus_sample_t *input_bufs[1];
  input_bufs[0] = rp->all_systems_input_buf;
  out_frame = 0;
  ifmt = rp->in_format;
  ochns = rp->out_chans;
  sr = rp->srate;
  sz = rp->buffer_size;
  in_datum_size = mus_bytes_per_sample(ifmt);
  if (rp->systems == 1)
    {
      active_in_chans = rp->input_channels[0];
      if (ochns > active_in_chans) sz /= ochns;
      mus_audio_read(rp->input_ports[0], rp->raw_input_bufs[0], sz * in_datum_size);
      mus_file_read_buffer(ifmt, 0, 1, sz, input_bufs, rp->raw_input_bufs[0]);
    }
  else
    {
      active_in_chans = 0;
      /* rp->possible_input_chans is a count of all possible input channels, some of which may be incompatible */
      /* rp->input_channels[i] is how many of these channels can be active at once on a given system */
      for (i = 0; i < rp->systems; i++) active_in_chans += rp->input_channels[i];
      offset = 0;
      sz = (int)(((Float)sz / (Float)active_in_chans) + .5);
      sz *= active_in_chans; /* size has to be a multiple of incoming channels */
      if (ochns > active_in_chans) sz /= ochns;
      if (rp->system_input_buffer_size < sz)
	{
	  if (rp->all_systems_input_buf) FREE(rp->all_systems_input_buf);
	  rp->system_input_buffer_size = sz;
	  rp->all_systems_input_buf = (mus_sample_t *)CALLOC(rp->system_input_buffer_size, sizeof(mus_sample_t));
	}
      if (!rp->one_system_input_buf) rp->one_system_input_buf = (mus_sample_t *)CALLOC(sz, sizeof(mus_sample_t));
      input_bufs[0] = rp->one_system_input_buf;
      for (i = 0; i < rp->systems; i++)
	{
	  cur_size = sz * rp->input_channels[i] / active_in_chans;
	  mus_audio_read(rp->input_ports[i], 
			 rp->raw_input_bufs[i], 
			 cur_size * in_datum_size);
	  mus_file_read_buffer(ifmt, 0, 1, sz, input_bufs, rp->raw_input_bufs[i]);
	  for (k = 0, m = offset; m < sz; m += active_in_chans) 
	    for (n = 0; n < rp->input_channels[i]; n++) 
	      rp->all_systems_input_buf[m + n] = rp->one_system_input_buf[k++];
	  offset += rp->input_channels[i];
	}
    }
  for (i = 0; i < rp->possible_input_chans; i++) 
    rp->input_vu_maxes[i] = MUS_SAMPLE_0;
  for (i = 0; i < ochns; i++) 
    rp->output_vu_maxes[i] = MUS_SAMPLE_0;

  /* run through input devices looking for any that are currently turned on */
  /* for each channel currently on, get its associated input channel */

  for (i = 0, out_frame = 0; i < sz; i += active_in_chans, out_frame++)
    {
      for (out_chan = 0; out_chan < ochns; out_chan++) 
	rp->unscaled_output_bufs[out_chan] = MUS_SAMPLE_0;
      inchn = 0;
      for (in_chan = 0; in_chan < rp->possible_input_chans; in_chan++)
	{
	  if (rp->input_channel_active[in_chan])
	    {
	      val = rp->all_systems_input_buf[i+inchn];
	      if (rp->chan_in_active[in_chan])
		for (out_chan = 0; out_chan < ochns; out_chan++)
		  rp->unscaled_output_bufs[out_chan] += (mus_sample_t)(rp->in_amps[in_chan][out_chan] * val);
	      if (val < MUS_SAMPLE_0) val = -val; 
	      if (val > rp->input_vu_maxes[in_chan]) rp->input_vu_maxes[in_chan] = val;
	      inchn++;
	    }
	}
      for (out_chan = 0; out_chan < ochns; out_chan++)
	{
	  val = (mus_sample_t)(rp->unscaled_output_bufs[out_chan] * rp->out_amps[out_chan]);
	  if ((rp->recording) && 
	      (rp->chan_out_active[out_chan])) 
	    rp->output_bufs[out_chan][out_frame] = val;
	  if (val < MUS_SAMPLE_0) val = -val;
	  if (val > rp->output_vu_maxes[out_chan]) 
	    rp->output_vu_maxes[out_chan] = val;
	}
    }
  for (in_chan = 0; in_chan < rp->possible_input_chans; in_chan++)
    if (rp->input_channel_active[in_chan])
      recorder_set_vu_in_val(in_chan, rp->input_vu_maxes[in_chan]);
  for (out_chan = 0; out_chan < ochns; out_chan++)
    {
      recorder_set_vu_out_val(out_chan, rp->output_vu_maxes[out_chan]);
      if ((!rp->triggered) && 
	  (MUS_SAMPLE_TO_FLOAT(rp->output_vu_maxes[out_chan]) > rp->trigger)) 
	rp->triggered = true;
    }

  if ((rp->monitoring) && 
      (rp->output_bufs) && 
      (ochns == rp->monitor_chans))
    {
      /* opened in rp->output_data_format and rp->monitor_chans */
      mus_file_write_buffer(rp->output_data_format, 0, out_frame - 1, rp->monitor_chans, rp->output_bufs, rp->raw_input_bufs[0], data_clipped(ss));
      mus_audio_write(rp->monitor_port, 
		      rp->raw_input_bufs[0], 
		      out_frame * rp->monitor_chans * mus_bytes_per_sample(rp->output_data_format));
    }
  if ((rp->recording) && (rp->triggered))
    {
      mus_file_write(rp->output_file_descriptor, 0, out_frame - 1, ochns, rp->output_bufs);
      rp->total_output_frames += out_frame;
      if (rp->total_output_frames > rp->duration_label_update_frames)
	{
	  reflect_recorder_duration((Float)((double)(rp->total_output_frames) / (Float)sr));
	  rp->duration_label_update_frames += (sr / 4);
	}
    }
  return(((rp->total_output_frames / sr) >= rp->max_duration) ? BACKGROUND_QUIT : BACKGROUND_CONTINUE);
}
#endif

bool recorder_start_output_file(const char *comment)
{
  int comlen, err, i;
  char *msg;
  comlen = (int)(snd_strlen(comment) + 3) / 4;
  comlen *= 4;
  err = snd_write_header(rp->output_file, rp->output_header_type, rp->srate, rp->out_chans, 28 + comlen, 0,
			 rp->output_data_format, comment, snd_strlen(comment), NULL);
  if (err == -1)
    {
      msg = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      mus_snprintf(msg, PRINT_BUFFER_SIZE, "%s:\n %s", rp->output_file, strerror(errno));
      recorder_error(msg);
      FREE(msg);
      rp->recording = false;
      rp->triggered = (!rp->triggering);
      return(true);
    }
  unsensitize_control_buttons();

  if (rp->output_header_type != MUS_RAW) mus_header_read(rp->output_file);
  rp->output_file_descriptor = snd_reopen_write(rp->output_file);
  mus_file_open_descriptors(rp->output_file_descriptor, 
			    rp->output_file,
			    rp->output_data_format, 
			    mus_bytes_per_sample(rp->output_data_format), 
			    mus_header_data_location(),
			    rp->out_chans, 
			    rp->output_header_type);
  mus_file_set_data_clipped(rp->output_file_descriptor, data_clipped(ss));
  rp->total_output_frames = 0;
  rp->duration_label_update_frames = rp->srate / 4;
  if (!rp->output_bufs)
    rp->output_bufs = (mus_sample_t **)CALLOC(MAX_OUT_CHANS, sizeof(mus_sample_t *));
  for (i = 0; i < rp->out_chans; i++) 
    if (!rp->output_bufs[i])
      rp->output_bufs[i] = (mus_sample_t *)CALLOC(rp->buffer_size, sizeof(mus_sample_t));
  return(false);
}

int recorder_get_devices(recorder_info *rp, int *outs)
{
  int i, err, input_devices = 0, output_devices = 0, system, cur_devices, device;
  float audval[AUDVAL_SIZE];

  rp->systems = mus_audio_systems();
  for (system = 0; system < rp->systems; system++)
    {
      /* look for audio input devices -- if none, report problem and quit */
      err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(system) | MUS_AUDIO_DEFAULT, 
				 MUS_AUDIO_PORT, 
				 AUDVAL_SIZE, 
				 audval);
      if (err != MUS_NO_ERROR) 
	{
	  snd_error("%s[%d] %s", 
		    __FILE__, __LINE__, c__FUNCTION__);
	  return(-1);
	}
      cur_devices = (int)(audval[0]);
      if (cur_devices == 0) 
	{
	  snd_error(_("no audio devices available")); 
	  return(-1);
	}
      for (i = 0; i < cur_devices; i++) 
	{
	  device = (int)(audval[i + 1]);
	  if (recorder_input_device(device))
	    input_devices++;
	  else 
	    {
	      if ((system == 0) && (recorder_output_device(device)))
		output_devices++;
	    }
	  rp->num_mixer_gains += device_gains(MUS_AUDIO_PACK_SYSTEM(system) | device);
	}
    }
  if (input_devices == 0) 
    {
      snd_error(_("no audio input devices available")); 
      return(-1);
    }
  (*outs) = output_devices;
  return(input_devices);
}

void cleanup_recording (void)
{
  if (!rp) return;
  if (rp->taking_input) close_recorder_audio();
  if ((record_in_progress()) && (rp->output_file_descriptor > 0))
    {
      rp->recording = false;
      rp->triggered = (!rp->triggering);
      sensitize_control_buttons();
      mus_file_close(rp->output_file_descriptor);
    }
}

static Cessate run_adc(Indicium ignore)
{
  Cessate val;
  val = read_adc();
#if DEBUGGING
  if (!(with_background_processes(ss))) val = BACKGROUND_QUIT;
#endif
  if (val == BACKGROUND_QUIT)
    {
      rp->recording = false;
      finish_recording(rp);
    }
  return(val);
}

void set_read_in_progress (void)
{
#if DEBUGGING
  if (with_background_processes(ss))
#endif
    rp->recorder_reader = BACKGROUND_ADD(run_adc, NULL);
}

static XEN g_recorder_autoload(void) 
{
  init_recorder(); 
  return(C_TO_XEN_BOOLEAN(rp->autoload));
}
static XEN g_set_recorder_autoload(XEN val) 
{
  #define H_recorder_autoload "(" S_recorder_autoload "): #t if newly recorded sound should be loaded into Snd automatically"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_recorder_autoload, "a boolean");
  init_recorder(); 
  set_recorder_autoload(rp, XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(rp->autoload));
}

static XEN g_recorder_buffer_size(void) 
{
  init_recorder(); 
  return(C_TO_XEN_INT(rp->buffer_size));
}
static XEN g_set_recorder_buffer_size(XEN val) 
{
  int size;
  #define H_recorder_buffer_size "(" S_recorder_buffer_size "): ADC buffer size (4096)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_recorder_buffer_size, "an integer"); 
  size = XEN_TO_C_INT(val);
  if (size <= 0)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_recorder_buffer_size, 1, val, "size ~A <= 0?");
  init_recorder(); 
  rp->buffer_size = size;
  return(val);
}

static XEN g_recorder_file(void) 
{
  init_recorder(); 
  return(C_TO_XEN_STRING(rp->output_file));
}
static XEN g_set_recorder_file(XEN val) 
{
  #define H_recorder_file "(" S_recorder_file "): default recorder file name"
  XEN_ASSERT_TYPE(XEN_STRING_P(val) || XEN_FALSE_P(val), val, XEN_ONLY_ARG, S_setB S_recorder_file, "a string"); 
  init_recorder(); 
  if (rp->output_file) FREE(rp->output_file);
  if (XEN_FALSE_P(val))
    {
      if (DEFAULT_RECORDER_FILE != (char *)NULL)
	rp->output_file = copy_string(DEFAULT_RECORDER_FILE);
      else rp->output_file = NULL;
    }
  else rp->output_file = copy_string(XEN_TO_C_STRING(val));
  return(C_TO_XEN_STRING(rp->output_file));
}

static XEN g_recorder_in_format(void) 
{
  init_recorder(); 
  return(C_TO_XEN_INT(rp->in_format));
}
static XEN g_set_recorder_in_format(XEN val) 
{
  #define H_recorder_in_format "(" S_recorder_in_format "): default recorder incoming data format (16 bit linear usually)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_recorder_in_format, "an integer"); 
  init_recorder(); 
  rp->in_format = XEN_TO_C_INT(val);
  return(C_TO_XEN_INT(rp->in_format));
}

static XEN g_recorder_in_device(void) 
{
  init_recorder(); 
  return(C_TO_XEN_INT(rp->in_device));
}
static XEN g_set_recorder_in_device(XEN val) 
{
  #define H_recorder_in_device "(" S_recorder_in_device "): default recorder input device (mus-audio-line-in or microphone usually)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_recorder_in_device, "an integer"); 
  init_recorder(); 
  rp->in_device = XEN_TO_C_INT(val);
  return(C_TO_XEN_INT(rp->in_device));
}

static XEN g_recorder_out_chans(void) 
{
  init_recorder(); 
  return(C_TO_XEN_INT(rp->out_chans));
}
static XEN g_set_recorder_out_chans(XEN val) 
{
  #define H_recorder_out_chans "(" S_recorder_out_chans "): default recorder output channels (1 or 2 usually)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_recorder_out_chans, "an integer"); 
  init_recorder(); 
  rp->out_chans = XEN_TO_C_INT(val);
  return(C_TO_XEN_INT(rp->out_chans));
}

static XEN g_recorder_out_format(void) 
{
  init_recorder(); 
  return(C_TO_XEN_INT(rp->output_data_format));
}
static XEN g_set_recorder_out_format(XEN val) 
{
  int df;
  #define H_recorder_out_format "(" S_recorder_out_format "): recorder output data format (16-bit linear usually)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_recorder_out_format, "a data format"); 
  init_recorder(); 
  df = XEN_TO_C_INT(val);
  if (MUS_DATA_FORMAT_OK(df))
    rp->output_data_format = df;
  else XEN_OUT_OF_RANGE_ERROR(S_setB S_recorder_out_format, XEN_ONLY_ARG, val, "a data format");
  return(C_TO_XEN_INT(rp->output_data_format));
}

static XEN g_recorder_out_type(void) 
{
  init_recorder(); 
  return(C_TO_XEN_INT(rp->output_header_type));
}
static XEN g_set_recorder_out_type(XEN val) 
{
  int ht;
  #define H_recorder_out_type "(" S_recorder_out_type "): recorder output header type"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_recorder_out_type, "a header type");
  init_recorder(); 
  ht = XEN_TO_C_INT(val);
  if (MUS_HEADER_TYPE_OK(ht))
    rp->output_header_type = ht;
  else XEN_OUT_OF_RANGE_ERROR(S_setB S_recorder_out_type, XEN_ONLY_ARG, val, "a header type");
  return(C_TO_XEN_INT(rp->output_header_type));
}

static XEN g_recorder_srate(void) 
{
  init_recorder(); 
  return(C_TO_XEN_INT(rp->srate));
}
static XEN g_set_recorder_srate(XEN val) 
{
  #define H_recorder_srate "(" S_recorder_srate "): default recorder sampling rate (22050 or 44100 usually)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_recorder_srate, "a number"); 
  init_recorder();
  set_recorder_srate(rp, XEN_TO_C_INT_OR_ELSE(val, 0));
  return(C_TO_XEN_INT(rp->srate));
}

static XEN g_recorder_trigger(void) 
{
  init_recorder(); 
  return(C_TO_XEN_DOUBLE(rp->trigger));
}
static XEN g_set_recorder_trigger(XEN val) 
{
  Float trigger;
  #define H_recorder_trigger "(" S_recorder_trigger "): the min amp that can trigger recording (if triggering)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_recorder_trigger, "a number"); 
  init_recorder(); 
  trigger = XEN_TO_C_DOUBLE(val);
  if (trigger > 1.0) trigger = 1.0;
  if (trigger < 0.0) trigger = 0.0;
  set_recorder_trigger(rp, trigger);
  return(C_TO_XEN_DOUBLE(rp->trigger));
}

static XEN g_recorder_max_duration(void) 
{
  init_recorder(); 
  return(C_TO_XEN_DOUBLE(rp->max_duration));
}
static XEN g_set_recorder_max_duration(XEN val) 
{
  #define H_recorder_max_duration "(" S_recorder_max_duration "): max recorder output file length"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_recorder_max_duration, "a number"); 
  init_recorder(); 
  rp->max_duration = XEN_TO_C_DOUBLE(val);
  return(C_TO_XEN_DOUBLE(rp->max_duration));
}

static XEN g_recorder_gain (XEN num) 
{
  #define H_recorder_gain "(" S_recorder_gain " gain): recorder input (soundcard) gain"
  int g;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(num), num, XEN_ONLY_ARG, S_recorder_gain, "an integer");
  init_recorder(); 
  g = XEN_TO_C_INT(num);
  if ((g >= 0) && (g < MAX_MIXER_GAINS))
    return(C_TO_XEN_DOUBLE(rp->mixer_gains[g]));
  return(C_TO_XEN_DOUBLE(0.0));
}

static XEN g_recorder_in_amp (XEN in, XEN out) 
{
  #define H_recorder_in_amp "(" S_recorder_in_amp " in out): recorder scaler on input to output"
  int ic, oc;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(in), in, XEN_ARG_1, S_recorder_in_amp, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(out), out, XEN_ARG_2, S_recorder_in_amp, "an integer");
  init_recorder(); 
  ic = XEN_TO_C_INT(in);
  oc = XEN_TO_C_INT(out);
  if ((ic >= 0) && (ic < MAX_IN_CHANS) && 
      (oc >= 0) && (oc < MAX_OUT_CHANS))
    return(C_TO_XEN_DOUBLE(rp->in_amps[ic][oc]));
  return(C_TO_XEN_DOUBLE(0.0));
}

static XEN g_recorder_out_amp (XEN num) 
{
  #define H_recorder_out_amp "(" S_recorder_out_amp " out): recorder output scaler"
  int oc;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(num), num, XEN_ONLY_ARG, S_recorder_out_amp, "an integer");
  init_recorder(); 
  oc = XEN_TO_C_INT(num);
  if ((oc >= 0) && (oc < MAX_OUT_CHANS))
    return(C_TO_XEN_DOUBLE(rp->out_amps[oc]));
  return(C_TO_XEN_DOUBLE(0.0));
}

static XEN g_set_recorder_gain (XEN num, XEN amp) 
{
  int ind;
  Float gain;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(num), num, XEN_ARG_1, S_setB S_recorder_gain, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(amp), amp, XEN_ARG_2, S_setB S_recorder_gain, "a number"); 
  init_recorder(); 
  ind = XEN_TO_C_INT(num);
  if ((ind >= 0) && (ind < MAX_MIXER_GAINS))
    {
      gain = XEN_TO_C_DOUBLE(amp);
      if ((gain >= 0.0) && (gain <= 1.0))
	{
	  rp->mixer_gains[ind] = gain;
	  reflect_recorder_mixer_gain(ind, rp->mixer_gains[ind]);
	}
    }
  return(amp);
}

static XEN g_set_recorder_in_amp (XEN in, XEN out, XEN amp) 
{
  int in_ind, out_ind;
  Float gain;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(in), in, XEN_ARG_1, S_setB S_recorder_in_amp, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(out), out, XEN_ARG_2, S_setB S_recorder_in_amp, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(amp), amp, XEN_ARG_3, S_setB S_recorder_in_amp, "a number");
  init_recorder(); 
  in_ind = XEN_TO_C_INT(in);
  out_ind = XEN_TO_C_INT(out);
  if ((in_ind >= 0) && (in_ind < MAX_IN_CHANS) && 
      (out_ind >= 0) && (out_ind < MAX_OUT_CHANS))
    {
      gain = XEN_TO_C_DOUBLE(amp);
      if ((gain >= amp_control_min(ss)) && (gain <= amp_control_max(ss)))
	{
	  rp->in_amps[in_ind][out_ind] = gain;
	  reflect_recorder_in_amp(in_ind, 
				  out_ind, 
				  rp->in_amps[in_ind][out_ind]);
	}
    }
  return(amp);
}

static XEN g_set_recorder_out_amp (XEN num, XEN amp) 
{
  int ind;
  Float gain;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(num), num, XEN_ARG_1, S_setB S_recorder_out_amp, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(amp), amp, XEN_ARG_2, S_setB S_recorder_out_amp, "a number"); 
  init_recorder(); 
  ind = XEN_TO_C_INT(num);
  if ((ind >= 0) && (ind < MAX_OUT_CHANS))
    {
      gain = XEN_TO_C_DOUBLE(amp); 
      if ((gain >= amp_control_min(ss)) && (gain <= amp_control_max(ss)))
	{
	  rp->out_amps[ind] = gain;
	  reflect_recorder_out_amp(ind,
				   rp->out_amps[ind]);
	}
    }
  return(amp);
}

static XEN g_recorder_dialog(void) 
{
  widget_t w;
  #define H_recorder_dialog "(" S_recorder_dialog "): start the Recorder"
  init_recorder(); 
  w = snd_record_file(); 
  return(XEN_WRAP_WIDGET(w));
}

static XEN g_vu_font(void) {return(C_TO_XEN_STRING(vu_font(ss)));}
static XEN g_set_vu_font(XEN val) 
{
  #define H_vu_font "(" S_vu_font "): name of font used to make VU meter labels (courier)"
  XEN_ASSERT_TYPE(XEN_STRING_P(val) || XEN_FALSE_P(val), val, XEN_ONLY_ARG, S_setB S_vu_font, "a string"); 
  if (vu_font(ss)) FREE(vu_font(ss));
  if (XEN_FALSE_P(val))
    set_vu_font((DEFAULT_VU_FONT) ? copy_string(DEFAULT_VU_FONT) : NULL);
  else set_vu_font(copy_string(XEN_TO_C_STRING(val)));
  return(C_TO_XEN_STRING(vu_font(ss)));
}

static XEN g_vu_font_size(void) {return(C_TO_XEN_DOUBLE(vu_font_size(ss)));}
static XEN g_set_vu_font_size(XEN val) 
{
  #define H_vu_font_size "(" S_vu_font_size "): size of VU font meter labels (1.0)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_vu_font_size, "a number"); 
  set_vu_font_size(XEN_TO_C_DOUBLE(val));
  return(C_TO_XEN_DOUBLE(vu_font_size(ss)));
}

static XEN g_vu_size(void) {return(C_TO_XEN_DOUBLE(vu_size(ss)));}
static XEN g_set_vu_size(XEN val) 
{
  #define H_vu_size "(" S_vu_size "): size of VU meters (1.0)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_vu_size, "a number"); 
  set_vu_size(XEN_TO_C_DOUBLE(val));
  return(C_TO_XEN_DOUBLE(vu_size(ss)));
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_recorder_autoload_w, g_recorder_autoload)
XEN_NARGIFY_1(g_set_recorder_autoload_w, g_set_recorder_autoload)
XEN_NARGIFY_0(g_recorder_buffer_size_w, g_recorder_buffer_size)
XEN_NARGIFY_1(g_set_recorder_buffer_size_w, g_set_recorder_buffer_size)
XEN_NARGIFY_0(g_recorder_file_w, g_recorder_file)
XEN_NARGIFY_1(g_set_recorder_file_w, g_set_recorder_file)
XEN_NARGIFY_0(g_recorder_in_format_w, g_recorder_in_format)
XEN_NARGIFY_1(g_set_recorder_in_format_w, g_set_recorder_in_format)
XEN_NARGIFY_0(g_recorder_in_device_w, g_recorder_in_device)
XEN_NARGIFY_1(g_set_recorder_in_device_w, g_set_recorder_in_device)
XEN_NARGIFY_0(g_recorder_out_chans_w, g_recorder_out_chans)
XEN_NARGIFY_1(g_set_recorder_out_chans_w, g_set_recorder_out_chans)
XEN_NARGIFY_0(g_recorder_out_format_w, g_recorder_out_format)
XEN_NARGIFY_1(g_set_recorder_out_format_w, g_set_recorder_out_format)
XEN_NARGIFY_0(g_recorder_out_type_w, g_recorder_out_type)
XEN_NARGIFY_1(g_set_recorder_out_type_w, g_set_recorder_out_type)
XEN_NARGIFY_0(g_recorder_srate_w, g_recorder_srate)
XEN_NARGIFY_1(g_set_recorder_srate_w, g_set_recorder_srate)
XEN_NARGIFY_0(g_recorder_trigger_w, g_recorder_trigger)
XEN_NARGIFY_1(g_set_recorder_trigger_w, g_set_recorder_trigger)
XEN_NARGIFY_0(g_recorder_max_duration_w, g_recorder_max_duration)
XEN_NARGIFY_1(g_set_recorder_max_duration_w, g_set_recorder_max_duration)
XEN_ARGIFY_1(g_recorder_gain_w, g_recorder_gain)
XEN_NARGIFY_2(g_set_recorder_gain_w, g_set_recorder_gain)
XEN_NARGIFY_2(g_recorder_in_amp_w, g_recorder_in_amp)
XEN_NARGIFY_3(g_set_recorder_in_amp_w, g_set_recorder_in_amp)
XEN_NARGIFY_1(g_recorder_out_amp_w, g_recorder_out_amp)
XEN_NARGIFY_2(g_set_recorder_out_amp_w, g_set_recorder_out_amp)
XEN_NARGIFY_0(g_recorder_dialog_w, g_recorder_dialog)
XEN_NARGIFY_0(g_vu_font_w, g_vu_font)
XEN_NARGIFY_1(g_set_vu_font_w, g_set_vu_font)
XEN_NARGIFY_0(g_vu_font_size_w, g_vu_font_size)
XEN_NARGIFY_1(g_set_vu_font_size_w, g_set_vu_font_size)
XEN_NARGIFY_0(g_vu_size_w, g_vu_size)
XEN_NARGIFY_1(g_set_vu_size_w, g_set_vu_size)
#else
#define g_recorder_autoload_w g_recorder_autoload
#define g_set_recorder_autoload_w g_set_recorder_autoload
#define g_recorder_buffer_size_w g_recorder_buffer_size
#define g_set_recorder_buffer_size_w g_set_recorder_buffer_size
#define g_recorder_file_w g_recorder_file
#define g_set_recorder_file_w g_set_recorder_file
#define g_recorder_in_format_w g_recorder_in_format
#define g_set_recorder_in_format_w g_set_recorder_in_format
#define g_recorder_in_device_w g_recorder_in_device
#define g_set_recorder_in_device_w g_set_recorder_in_device
#define g_recorder_out_chans_w g_recorder_out_chans
#define g_set_recorder_out_chans_w g_set_recorder_out_chans
#define g_recorder_out_format_w g_recorder_out_format
#define g_set_recorder_out_format_w g_set_recorder_out_format
#define g_recorder_out_type_w g_recorder_out_type
#define g_set_recorder_out_type_w g_set_recorder_out_type
#define g_recorder_srate_w g_recorder_srate
#define g_set_recorder_srate_w g_set_recorder_srate
#define g_recorder_trigger_w g_recorder_trigger
#define g_set_recorder_trigger_w g_set_recorder_trigger
#define g_recorder_max_duration_w g_recorder_max_duration
#define g_set_recorder_max_duration_w g_set_recorder_max_duration
#define g_recorder_gain_w g_recorder_gain
#define g_set_recorder_gain_w g_set_recorder_gain
#define g_recorder_in_amp_w g_recorder_in_amp
#define g_set_recorder_in_amp_w g_set_recorder_in_amp
#define g_recorder_out_amp_w g_recorder_out_amp
#define g_set_recorder_out_amp_w g_set_recorder_out_amp
#define g_recorder_dialog_w g_recorder_dialog
#define g_vu_font_w g_vu_font
#define g_set_vu_font_w g_set_vu_font
#define g_vu_font_size_w g_vu_font_size
#define g_set_vu_font_size_w g_set_vu_font_size
#define g_vu_size_w g_vu_size
#define g_set_vu_size_w g_set_vu_size
#endif

void g_init_recorder(void)
{
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_autoload, g_recorder_autoload_w, H_recorder_autoload,
				   S_setB S_recorder_autoload, g_set_recorder_autoload_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_buffer_size, g_recorder_buffer_size_w, H_recorder_buffer_size,
				   S_setB S_recorder_buffer_size, g_set_recorder_buffer_size_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_file, g_recorder_file_w, H_recorder_file,
				   S_setB S_recorder_file, g_set_recorder_file_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_in_format, g_recorder_in_format_w, H_recorder_in_format,
				   S_setB S_recorder_in_format, g_set_recorder_in_format_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_in_device, g_recorder_in_device_w, H_recorder_in_device,
				   S_setB S_recorder_in_device, g_set_recorder_in_device_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_out_chans, g_recorder_out_chans_w, H_recorder_out_chans,
				   S_setB S_recorder_out_chans, g_set_recorder_out_chans_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_out_format, g_recorder_out_format_w, H_recorder_out_format,
				   S_setB S_recorder_out_format, g_set_recorder_out_format_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_out_type, g_recorder_out_type_w, H_recorder_out_type,
				   S_setB S_recorder_out_type, g_set_recorder_out_type_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_srate, g_recorder_srate_w, H_recorder_srate,
				   S_setB S_recorder_srate, g_set_recorder_srate_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_trigger, g_recorder_trigger_w, H_recorder_trigger,
				   S_setB S_recorder_trigger, g_set_recorder_trigger_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_max_duration, g_recorder_max_duration_w, H_recorder_max_duration,
				   S_setB S_recorder_max_duration, g_set_recorder_max_duration_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_gain, g_recorder_gain_w, H_recorder_gain,
				   S_setB S_recorder_gain, g_set_recorder_gain_w,  0, 1, 2, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_in_amp, g_recorder_in_amp_w, H_recorder_in_amp,
				   S_setB S_recorder_in_amp, g_set_recorder_in_amp_w,  2, 0, 3, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_out_amp, g_recorder_out_amp_w, H_recorder_out_amp,
				   S_setB S_recorder_out_amp, g_set_recorder_out_amp_w,  1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE(S_recorder_dialog, g_recorder_dialog_w, 0, 0, 0, H_recorder_dialog);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_vu_font, g_vu_font_w, H_vu_font,
				   S_setB S_vu_font, g_set_vu_font_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_vu_font_size, g_vu_font_size_w, H_vu_font_size,
				   S_setB S_vu_font_size, g_set_vu_font_size_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_vu_size, g_vu_size_w, H_vu_size,
				   S_setB S_vu_size, g_set_vu_size_w,  0, 0, 1, 0);

}

#if USE_NO_GUI
  void set_recorder_trigger(recorder_info *rp, Float val) {}
  void set_recorder_srate(recorder_info *rp, int val) {}
  void set_recorder_autoload(recorder_info *rp, bool val) {}
  void recorder_set_vu_in_val(int chan, mus_sample_t val) {}
  void recorder_set_vu_out_val(int chan, mus_sample_t val) {}
  void finish_recording(recorder_info *rp) {}
  void sensitize_control_buttons(void) {}
  void recorder_fill_wd(void *uwd, int chan, int field, int device) {}
#endif
