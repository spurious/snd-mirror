/* snd-xrec and snd-grec shared code */

#include "snd.h"
#include "snd-rec.h"

FONT_TYPE *get_vu_font(snd_state *ss, Float size)
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
    case MUS_AUDIO_DIGITAL_OUT: return("Digital Out"); break;
    case MUS_AUDIO_LINE_OUT:    return("Line Out");    break;
    case MUS_AUDIO_DEFAULT: 
    case MUS_AUDIO_DAC_OUT:     return("Output");      break; /* default here means that linuxppc reports "Output" as analog-in pane name */
    case MUS_AUDIO_DUPLEX_DEFAULT: 
    case MUS_AUDIO_SPEAKERS:    return("Speakers");    break;
    case MUS_AUDIO_ADAT_IN:     return("Adat In");     break;
    case MUS_AUDIO_AES_IN:      return("Aes In");      break;
#if (HAVE_OSS || HAVE_ALSA)
    case MUS_AUDIO_LINE_IN:     return("Analog In");   break;
#else
    case MUS_AUDIO_LINE_IN:     return("Line In");     break;
#endif
    case MUS_AUDIO_MICROPHONE:  return("Microphone");  break;
    case MUS_AUDIO_DIGITAL_IN:  return("Digital In");  break;
    case MUS_AUDIO_ADAT_OUT:    return("Adat Out");    break;
    case MUS_AUDIO_AES_OUT:     return("Aes Out");     break;
    case MUS_AUDIO_DAC_FILTER:  return("Tone");          break;
    case MUS_AUDIO_MIXER:       return("Mixer");         break;
    case MUS_AUDIO_AUX_INPUT:   return("Aux Input");     break;
    case MUS_AUDIO_CD:          return("CD");            break;
    case MUS_AUDIO_AUX_OUTPUT:  return("Aux Output");    break;
    case MUS_AUDIO_SPDIF_IN:    return("S/PDIF In");     break;
    case MUS_AUDIO_SPDIF_OUT:   return("S/PDIF Out");    break;
    default: 
      snd_error("%s[%d] %s: unknown device: %d", 
		__FILE__, __LINE__, __FUNCTION__, 
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

int recorder_input_device(int dev)
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
    case MUS_AUDIO_DAC_OUT: return(0); break;
    case MUS_AUDIO_DUPLEX_DEFAULT: 
    case MUS_AUDIO_ADAT_IN: 
    case MUS_AUDIO_AES_IN:
    case MUS_AUDIO_SPDIF_IN:
    case MUS_AUDIO_LINE_IN: 
    case MUS_AUDIO_MICROPHONE: 
    case MUS_AUDIO_DIGITAL_IN: 
    case MUS_AUDIO_CD:
    default: return(1); break;
    }
  snd_error("%s[%d] %s: uncategorized device: %d", 
	    __FILE__, __LINE__, __FUNCTION__, 
	    dev);
  return(0);
}

int recorder_output_device(int dev)
{
  return((dev != MUS_AUDIO_DAC_FILTER) && 
	 (dev != MUS_AUDIO_MIXER) &&
	 (!(recorder_input_device(dev))));
}

char *pane_device_name(int dev)
{
  /* informal aw shucks reference in help window */
  switch (dev)
    {
    case MUS_AUDIO_DIGITAL_OUT:
    case MUS_AUDIO_LINE_OUT:
    case MUS_AUDIO_DEFAULT:
    case MUS_AUDIO_DAC_OUT:
    case MUS_AUDIO_DUPLEX_DEFAULT: return("the output");                break;
    case MUS_AUDIO_SPEAKERS:   return("the speakers");                  break;
    case MUS_AUDIO_ADAT_OUT: 
    case MUS_AUDIO_ADAT_IN:    return("the Adat");                      break;
    case MUS_AUDIO_AES_OUT: 
    case MUS_AUDIO_AES_IN:     return("the Aes");                       break;
    case MUS_AUDIO_SPDIF_IN:
    case MUS_AUDIO_SPDIF_OUT:  return("the S/PDIF");                    break;
    case MUS_AUDIO_LINE_IN:    return("line in");                       break;
    case MUS_AUDIO_MICROPHONE: return("the microphone");                break;
    case MUS_AUDIO_DIGITAL_IN: return("digital in");                    break;
    case MUS_AUDIO_DAC_FILTER: return("the analog tone control");       break;
    case MUS_AUDIO_MIXER:      return("various analog volume controls"); break;
    case MUS_AUDIO_CD:         return("the internal CD");               break;
    default:                   return("the input");                     break;
    }
}

#if (HAVE_OSS || HAVE_ALSA)
char *recorder_field_abbreviation(int fld)
{
  switch (fld)
    {
    case MUS_AUDIO_IMIX:   return("imx"); break;
    case MUS_AUDIO_IGAIN:  return("ign"); break;
    case MUS_AUDIO_RECLEV: return("rec"); break;
    case MUS_AUDIO_PCM:    return("pcm"); break;
    case MUS_AUDIO_PCM2:   return("pc2"); break;
    case MUS_AUDIO_OGAIN:  return("ogn"); break;
    case MUS_AUDIO_LINE:   return("lin"); break;
    case MUS_AUDIO_MICROPHONE:    return("mic"); break;
    case MUS_AUDIO_LINE1:  return("l1");  break;
    case MUS_AUDIO_LINE2:  return("l2");  break;
    case MUS_AUDIO_LINE3:  return("l3");  break;
    case MUS_AUDIO_SYNTH:  return("syn"); break;
    case MUS_AUDIO_BASS:   return("ton"); break;
    case MUS_AUDIO_TREBLE: return("ton"); break;
    case MUS_AUDIO_CD:     return("cd");  break;
    }
  return("oops");
}

char *recorder_field_name(int fld)
{
  switch (fld)
    {
    case MUS_AUDIO_IMIX:   return("imix");    break;
    case MUS_AUDIO_IGAIN:  return("igain");   break;
    case MUS_AUDIO_RECLEV: return("reclev");  break;
    case MUS_AUDIO_PCM:    return("pcm");     break;
    case MUS_AUDIO_PCM2:   return("pcm2");    break;
    case MUS_AUDIO_OGAIN:  return("ogain");   break;
    case MUS_AUDIO_LINE:   return("line-in"); break;
    case MUS_AUDIO_MICROPHONE: return("mic"); break;
    case MUS_AUDIO_LINE1:  return("line1");   break;
    case MUS_AUDIO_LINE2:  return("line2");   break; 
    case MUS_AUDIO_LINE3:  return("line3");   break;
    case MUS_AUDIO_SYNTH:  return("synth");   break;
    case MUS_AUDIO_CD:     return("cd");      break;
    default:               return("?");       break;
    }
}

char *recorder_field_function(int fld)
{
  switch (fld)
    {
    case MUS_AUDIO_IMIX:   return("the pre-adc mix of mic and line-in"); break;
    case MUS_AUDIO_IGAIN:  return("input gain");                         break;
    case MUS_AUDIO_RECLEV: return("recording level");                    break;
    case MUS_AUDIO_PCM:    return("the speaker level, perhaps");         break;
    case MUS_AUDIO_PCM2:   return("nothing in particular");              break;
    case MUS_AUDIO_OGAIN:  return("output gain");                        break;
    case MUS_AUDIO_LINE:   return("analog line-in");                     break;
    case MUS_AUDIO_MICROPHONE: return("the microphone");                 break;
    case MUS_AUDIO_LINE1:  
    case MUS_AUDIO_LINE2:  
    case MUS_AUDIO_LINE3:  return("extra line inputs");                  break;
    case MUS_AUDIO_SYNTH:  return("the on-card synthesizer, if any");    break;
    case MUS_AUDIO_CD:     return("the cd gain");                        break;
    default:               return("?");                                  break;
    }
}
#endif

int recorder_sort_mixer_device(void *wd, int i, int chan, int input, int device, int *mixflds)
{
  int k;
#if (HAVE_OSS || HAVE_ALSA)
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

int recorder_check_device(int system, int device, int *mixer_gains_posted, int *tone_controls_posted, int *mixflds, int *gains, int *inp)
{
  int vu_meters = 0, input, num_gains, k;
#if (HAVE_OSS || HAVE_ALSA)
  float mixer_field_chans[MAX_AUDIO_FIELD];
#endif
  vu_meters = device_channels(MUS_AUDIO_PACK_SYSTEM(system) | device);
  input = (recorder_input_device(device));
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

recorder_info *get_recorder_info(void) {return(rp);}

void init_recorder(void)
{
  int i;
  rp = (recorder_info *)CALLOC(1, sizeof(recorder_info));
  rp->recording = 0;
  rp->autoload = DEFAULT_RECORDER_AUTOLOAD;
  rp->buffer_size = DEFAULT_RECORDER_BUFFER_SIZE;
  rp->out_chans = DEFAULT_RECORDER_OUT_CHANS;
  rp->out_format = DEFAULT_RECORDER_OUT_FORMAT;
  rp->in_format = DEFAULT_RECORDER_IN_FORMAT;
  rp->srate = DEFAULT_RECORDER_SRATE;
  rp->trigger = DEFAULT_RECORDER_TRIGGER;
  rp->max_duration = DEFAULT_RECORDER_MAX_DURATION;
  if (DEFAULT_RECORDER_FILE != (char *)NULL) rp->output_file = copy_string(DEFAULT_RECORDER_FILE); else rp->output_file = NULL;
  rp->in_device = MUS_AUDIO_DEFAULT;
  rp->triggering = 0;
  rp->triggered = 1;
  rp->monitor_chans = 2;
  rp->monitor_port = -1;
  rp->monitoring = 0;
  rp->taking_input = 0;
  rp->systems = 1;
  rp->mixer_settings_saved = 0;
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
  rp->chan_in_active = (int *)CALLOC(MAX_IN_CHANS, sizeof(int));
  rp->chan_out_active = (int *)CALLOC(MAX_OUT_CHANS, sizeof(int));
}

int record_in_progress(void)
{
  return((record_dialog_is_active()) && (rp->recording));
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

static int fneq(Float a, Float b) {return(fabs(a - b) > .00001);}
static char *b2s(int val) {if (val) return("#t"); else return("#f");}

void save_recorder_state(FILE *fd)
{
#if HAVE_GUILE
  if (rp->autoload != DEFAULT_RECORDER_AUTOLOAD) fprintf(fd, "(set! (%s) %s)\n", S_recorder_autoload, b2s(rp->autoload));
  if (rp->buffer_size != DEFAULT_RECORDER_BUFFER_SIZE) fprintf(fd, "(set! (%s) %d)\n", S_recorder_buffer_size, rp->buffer_size);
  if (rp->out_chans != DEFAULT_RECORDER_OUT_CHANS) fprintf(fd, "(set! (%s) %d)\n", S_recorder_out_chans, rp->out_chans);
  if (rp->out_format != DEFAULT_RECORDER_OUT_FORMAT) fprintf(fd, "(set! (%s) %d)\n", S_recorder_out_format, rp->out_format);
  if (rp->in_format != DEFAULT_RECORDER_IN_FORMAT) fprintf(fd, "(set! (%s) %d)\n", S_recorder_in_format, rp->in_format);
  if (rp->in_device != MUS_AUDIO_DEFAULT) fprintf(fd, "(set! (%s) %d)\n", S_recorder_in_device, rp->in_device);
  if (rp->srate != DEFAULT_RECORDER_SRATE) fprintf(fd, "(set! (%s) %d)\n", S_recorder_srate, rp->srate);
  if (rp->output_file != NULL) fprintf(fd, "(set! (%s) \"%s\")\n", S_recorder_file, rp->output_file);
  if (fneq(rp->trigger, DEFAULT_RECORDER_TRIGGER)) fprintf(fd, "(set! (%s) %.4f)\n", S_recorder_trigger, rp->trigger);
  if (fneq(rp->max_duration, DEFAULT_RECORDER_MAX_DURATION)) fprintf(fd, "(set! (%s) %.4f)\n", S_recorder_max_duration, rp->max_duration);
#endif
#if HAVE_RUBY
  if (rp->autoload != DEFAULT_RECORDER_AUTOLOAD) fprintf(fd, "set_%s %s\n", S_recorder_autoload, b2s(rp->autoload));
  if (rp->buffer_size != DEFAULT_RECORDER_BUFFER_SIZE) fprintf(fd, "set_%s %d\n", S_recorder_buffer_size, rp->buffer_size);
  if (rp->out_chans != DEFAULT_RECORDER_OUT_CHANS) fprintf(fd, "set_%s %d\n", S_recorder_out_chans, rp->out_chans);
  if (rp->out_format != DEFAULT_RECORDER_OUT_FORMAT) fprintf(fd, "set_%s %d\n", S_recorder_out_format, rp->out_format);
  if (rp->in_format != DEFAULT_RECORDER_IN_FORMAT) fprintf(fd, "set_%s %d\n", S_recorder_in_format, rp->in_format);
  if (rp->in_device != MUS_AUDIO_DEFAULT) fprintf(fd, "set_%s %d\n", S_recorder_in_device, rp->in_device);
  if (rp->srate != DEFAULT_RECORDER_SRATE) fprintf(fd, "set_%s %d\n", S_recorder_srate, rp->srate);
  if (rp->output_file != NULL) fprintf(fd, "set_%s \"%s\"\n", S_recorder_file, rp->output_file);
  if (fneq(rp->trigger, DEFAULT_RECORDER_TRIGGER)) fprintf(fd, "set_%s %.4f\n", S_recorder_trigger, rp->trigger);
  if (fneq(rp->max_duration, DEFAULT_RECORDER_MAX_DURATION)) fprintf(fd, "set_%s %.4f\n", S_recorder_max_duration, rp->max_duration);
#endif
}

static char numbuf[LABEL_BUFFER_SIZE];
char *channel_name(int in_chans, int out_chans, int chan)
{
  int use_numbers;
  use_numbers = ((out_chans > 4) || (in_chans > 4));
  if (use_numbers)
    mus_snprintf(numbuf, LABEL_BUFFER_SIZE, "%d", chan + 1);
  else mus_snprintf(numbuf, LABEL_BUFFER_SIZE, "%c", (char)('A' + chan));
  return(numbuf);
}

char *out_channel_name(int chan)
{
  int use_numbers;
  use_numbers = (rp->out_chans > 4);
  if (use_numbers)
    mus_snprintf(numbuf, LABEL_BUFFER_SIZE, "%d", chan + 1);
  else mus_snprintf(numbuf, LABEL_BUFFER_SIZE, "%c", (char)('A' + chan));
  return(numbuf);
}

char *gain_channel_name(int in_chans, int out_chans, int input, int dev_in, int out)
{
  int use_numbers;
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
    snd_error("%s[%d] %s: overflow %d > %d",
	      __FILE__, __LINE__, __FUNCTION__, 
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
    snd_error("%s[%d] %s: overflow %d > %d",
	      __FILE__, __LINE__, __FUNCTION__, 
	      gain, rp->num_mixer_gains);
  else rp->mixer_gains[gain] = amp;
}


void recorder_set_audio_srate(snd_state *ss, int device, int srate, int system, int aud)
{
  float g[1];
#if (!NEW_SGI_AL)
  if (aud) close_recorder_audio();
  g[0] = (Float)srate;
  mus_audio_mixer_write(MUS_AUDIO_PACK_SYSTEM(system) | device, MUS_AUDIO_SRATE, 0, g);
  if (aud) fire_up_recorder(ss);
#endif
}

#if OLD_SGI_AL
void set_line_source(snd_state *ss, int in_digital)
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
  if (err == -1) 
    recorder_error("set input source: ");
  rp->input_channel_active[0] = (!in_digital); 
  rp->input_channel_active[1] = (!in_digital);
  rp->input_channel_active[2] = (!in_digital); 
  rp->input_channel_active[3] = (!in_digital);
  rp->input_channel_active[4] = in_digital; 
  rp->input_channel_active[5] = in_digital;
  if (aud) fire_up_recorder(ss);
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

static void oss_get_input_channels(int i)
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
		rp->input_channels[i] = device_channels(MUS_AUDIO_PACK_SYSTEM(i) | MUS_AUDIO_ADAT_IN);
	    }
	}
    }
}

static void oss_get_input_devices(void)
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

#if (HAVE_ALSA || HAVE_OSS)
void fire_up_recorder(snd_state *ss)
{
  int i, j;
  float val[64];
  float direction = 0.0;
  int size, sys, dev, sysdev, in_count;
  int err;

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
    rp->input_channel_active[i] = 1; 

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
	  if ((err = mus_audio_mixer_read(sysdev, MUS_AUDIO_DIRECTION, 0, &direction)) == 0) 
	    {
	      if (rp->input_channels[sys] == 0 && (int)direction == 1)
		{
		  if ((err = mus_audio_mixer_read(sysdev, MUS_AUDIO_CHANNEL, 2, val)) == 0) 
		    rp->input_channels[sys] = (int)val[0];
		  if ((err = mus_audio_mixer_read(sysdev, MUS_AUDIO_SRATE, 2, val)) == 0)
		    {
		      rp->input_srates[sys] = (int)val[0];
		      if (i == 0) 
			set_recorder_srate(rp, rp->input_srates[sys]);
		    }
		  rp->input_formats[sys] = mus_audio_compatible_format(sysdev);
		  if (i == 0) 
		    rp->in_format = rp->input_formats[sys];
		  if ((err = mus_audio_mixer_read(sysdev, MUS_AUDIO_SAMPLES_PER_CHANNEL, 0, val)) == 0)
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
	  recorder_error("no inputs?: ");
	  return;
	}

      /* open all input devices */
      for (i = 0; i < rp->ordered_devices_size; i++)
	{
	  sys = rp->ordered_systems[i];
	  dev = rp->ordered_devices[i];
	  sysdev = MUS_AUDIO_PACK_SYSTEM(sys)|dev;
	  if ((err = mus_audio_mixer_read(sysdev, MUS_AUDIO_DIRECTION, 0, &direction)) == 0) 
	    {
	      if ((int)direction == 1)
		{
		  size = mus_data_format_to_bytes_per_sample(rp->input_formats[sys]);
		  rp->input_ports[sys] = mus_audio_open_input(sysdev,
							      rp->srate,
							      rp->input_channels[sys],
							      rp->input_formats[sys],
							      rp->input_buffer_sizes[sys] * rp->input_channels[sys] * size);
		  if (rp->input_ports[sys] == -1)
		    {
		      recorder_error("open device: ");
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
      rp->taking_input = 1;

      /* search for output devices for rp->monitoring, first one wins */

      for (i = 0; i < rp->ordered_devices_size; i++)
	{
	  sys = rp->ordered_systems[i];
	  dev = rp->ordered_devices[i];
	  sysdev = MUS_AUDIO_PACK_SYSTEM(sys)|dev;
	  if ((err = mus_audio_mixer_read(sysdev, MUS_AUDIO_DIRECTION, 0, &direction)) == 0) 
	    {
	      if ((int)direction == 0)
		{
		  /* found the first pane that has an output device (must be the only one) */
		  if ((err = mus_audio_mixer_read(sysdev, MUS_AUDIO_CHANNEL, 2, val)) == 0) 
		    {
		      rp->monitor_chans = (int)(val[0]);
		      /* FIXME: what would be the proper value for this? 
		       * the computer wedges if I don't initialize it, why? */
		      rp->out_chans = rp->monitor_chans;
		    }
		  rp->monitor_data_format = mus_audio_compatible_format(sysdev);
		  size = mus_data_format_to_bytes_per_sample(rp->monitor_data_format);
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
		      rp->monitoring = 1;
		    }
		  else
		    {
		      recorder_error("open output: ");
		      rp->monitoring = 0;
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
	oss_get_input_channels(i);
      err = 1;
      for (i = 0; i < rp->systems; i++) 
	if (rp->input_channels[i] > 0) 
	  {
	    err = 0; 
	    break;
	  }
      if (err)
	{
	  recorder_error("no inputs?: ");
	  return;
	}
      /* if adat, aes etc, make choices about default on/off state, open monitor separately (and write) */
      oss_get_input_devices();
      if (rp->input_ports[0] == -1)
	{
	  recorder_error("open device: ");
	  return;
	}
      rp->taking_input = 1;
      /* rp->monitor_port = 0; */

      /*
       * if (full_duplex(0))
       *   rp->monitor_port = mus_audio_open_output(MUS_AUDIO_DUPLEX_DEFAULT, rp->srate, rp->monitor_chans, rp->out_format, rp->buffer_size);
       */

      if (rp->monitor_port == -1)
	{
	  recorder_error("open output: ");
	  rp->monitoring = 0;
	}
      else rp->monitoring = 1;
    }
  set_read_in_progress(ss);
}

#else /* not ALSA or OSS */

void fire_up_recorder(snd_state *ss)
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
#if HAVE_OSS
  if (rp->mixer_settings_saved) mus_audio_restore(); else mus_audio_save();
#else
  mus_audio_save();
#endif

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
    if (!err) 
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
    rp->input_channel_active[0] = 1;
    rp->input_channel_active[1] = 0;
  #else
    err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(0) | rp->in_device, MUS_AUDIO_SRATE, 0, val);
    if (!err) 
      {
	new_srate = (int)val[0];
	if ((new_srate > 0) && (rp->srate != new_srate)) 
	  set_recorder_srate(rp, new_srate);
      }
    rp->monitor_chans = 2;
    for (i = 0; i < rp->possible_input_chans; i++) 
      {
	rp->input_channel_active[i] = 1; 
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
	  oss_get_input_channels(i);
  #endif
#endif
	}
    }

  err = 1;
  for (i = 0; i < rp->systems; i++) if (rp->input_channels[i] > 0) {err = 0; break;}
  if (err)
    {
      recorder_error("no inputs?: ");
      return;
    }

#if NEW_SGI_AL
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

    oss_get_input_devices();
    #endif
  #endif
#endif
  if (rp->input_ports[0] == -1)
    {
      recorder_error("open device: ");
      return;
    }
  rp->taking_input = 1;
#if HAVE_OSS || SUN
  /* rp->monitor_port = 0; */

  /*
   * if (full_duplex(0))
   *   rp->monitor_port = mus_audio_open_output(MUS_AUDIO_DUPLEX_DEFAULT, rp->srate, rp->monitor_chans, rp->out_format, rp->buffer_size);
   */

#else
  rp->monitor_port = mus_audio_open_output(MUS_AUDIO_PACK_SYSTEM(0) | MUS_AUDIO_DAC_OUT,
					   rp->srate,
					   rp->monitor_chans,
					   rp->out_format,
					   rp->buffer_size);
#endif
  if (rp->monitor_port == -1)
    {
      recorder_error("open output: ");
      rp->monitoring = 0;
    }
  else rp->monitoring = 1;
  set_read_in_progress(ss);
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
      rp->taking_input = 0;
    }
  if (rp->recorder_reader) 
    {
      BACKGROUND_REMOVE(rp->recorder_reader);
      rp->recorder_reader = 0;
    }
  if (rp->monitoring)
    {
      mus_audio_close(rp->monitor_port);
      rp->monitoring = 0;
    }
}


void recorder_characterize_devices(int devs, int output_devices)
{
  float direction = 0.0;
  int i, k, def_out, system, cur_devices, device, err, n;
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
	       ((err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(system)|device, MUS_AUDIO_DIRECTION, 0, &direction)) == 0) &&
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
	      if ((err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(system)|device, MUS_AUDIO_DIRECTION, 0, &direction)) == 0) 
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

static BACKGROUND_TYPE read_adc(snd_state *ss) 
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
      buffer_size = rp->input_buffer_sizes[0]*rp->input_channels[0];
      if (ochns > active_in_chans) buffer_size /= ochns;
      if (rp->system_input_buffer_size < buffer_size)
	{
	  if (rp->all_systems_input_buf) FREE(rp->all_systems_input_buf);
	  rp->system_input_buffer_size = buffer_size;
	  rp->all_systems_input_buf = (mus_sample_t *)CALLOC(rp->system_input_buffer_size, sizeof(mus_sample_t));
	}
      in_datum_size = mus_data_format_to_bytes_per_sample(rp->input_formats[0]);
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
	  in_datum_size = mus_data_format_to_bytes_per_sample(rp->input_formats[i]);
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
	rp->triggered = 1;
    }
  if ((rp->monitoring) && (rp->output_bufs) && (ochns <= rp->monitor_chans))
    {
      /* opened in rp->out_format and rp->monitor_chans */
      mus_file_write_buffer(rp->monitor_data_format, 0, out_frame - 1, rp->monitor_chans, rp->output_bufs, rp->monitor_buf, data_clipped(ss));
      mus_audio_write(rp->monitor_port, 
		      rp->monitor_buf, 
		      rp->buffer_size * rp->monitor_chans * mus_data_format_to_bytes_per_sample(rp->monitor_data_format));
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

static BACKGROUND_TYPE read_adc(snd_state *ss)
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
  in_datum_size = mus_data_format_to_bytes_per_sample(ifmt);
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
	rp->triggered = 1;
    }

  if ((rp->monitoring) && 
      (rp->output_bufs) && 
      (ochns == rp->monitor_chans))
    {
      /* opened in rp->out_format and rp->monitor_chans */
      mus_file_write_buffer(rp->out_format, 0, out_frame - 1, rp->monitor_chans, rp->output_bufs, rp->raw_input_bufs[0], data_clipped(ss));
      mus_audio_write(rp->monitor_port, 
		      rp->raw_input_bufs[0], 
		      out_frame * rp->monitor_chans * mus_data_format_to_bytes_per_sample(rp->out_format));
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

int recorder_start_output_file(snd_state *ss, char *comment)
{
  int comlen, err, i;
  char *msg;
  comlen = (int)(snd_strlen(comment) + 3) / 4;
  comlen *= 4;
  err = snd_write_header(ss, rp->output_file, rp->output_header_type, rp->srate, rp->out_chans, 28 + comlen, 0,
			 rp->out_format, comment, snd_strlen(comment), NULL);
  if (err)
    {
      msg = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      mus_snprintf(msg, PRINT_BUFFER_SIZE, "%s:\n %s", rp->output_file, strerror(errno));
      recorder_error(msg);
      FREE(msg);
      rp->recording = 0;
      rp->triggered = (!rp->triggering);
      return(TRUE);
    }

  unsensitize_control_buttons();

  rp->output_file_descriptor = snd_reopen_write(ss, rp->output_file);
  mus_header_read_with_fd(rp->output_file_descriptor);
  mus_file_open_descriptors(rp->output_file_descriptor, 
			    rp->output_file,
			    rp->out_format, 
			    mus_data_format_to_bytes_per_sample(rp->out_format), 
			    mus_header_data_location(),
			    rp->out_chans, 
			    rp->output_header_type);
  mus_file_set_data_clipped(rp->output_file_descriptor, data_clipped(ss));
  rp->total_output_frames = 0;
  rp->duration_label_update_frames = rp->srate / 4;
  if (!rp->output_bufs)
    rp->output_bufs = (mus_sample_t **)CALLOC(MAX_OUT_CHANS, sizeof(mus_sample_t *));
  for (i = 0; i < rp->out_chans; i++) 
    {
      if (!rp->output_bufs[i])
	rp->output_bufs[i] = (mus_sample_t *)CALLOC(rp->buffer_size, sizeof(mus_sample_t));
    }
  return(FALSE);
}

int recorder_get_devices(recorder_info *rp, int *outs)
{
  int i, err, input_devices = 0, output_devices = 0, system, cur_devices, device;
  float audval[AUDVAL_SIZE];

  rp->systems = mus_audio_systems();
#if (HAVE_OSS || HAVE_ALSA)
  mus_audio_mixer_restore(DEFAULT_AUDIO_STATE_FILE);
#endif
  for (system = 0; system < rp->systems; system++)
    {
      /* look for audio input devices -- if none, report problem and quit */
      err = mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(system) | MUS_AUDIO_DEFAULT, 
				 MUS_AUDIO_PORT, 
				 AUDVAL_SIZE, 
				 audval);
      if (err != 0) 
	{
	  snd_error("%s[%d] %s", 
		    __FILE__, __LINE__, __FUNCTION__);
	  return(-1);
	}
      cur_devices = (int)(audval[0]);
      if (cur_devices == 0) 
	{
	  snd_error("no audio devices available"); 
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
      snd_error("no audio input devices available"); 
      return(-1);
    }
  (*outs) = output_devices;
  return(input_devices);
}

void cleanup_recording (void)
{
  if (rp->taking_input) close_recorder_audio();
  if ((record_in_progress()) && (rp->output_file_descriptor > 0))
    {
      rp->recording = 0;
      rp->triggered = (!rp->triggering);
      sensitize_control_buttons();
      mus_file_close(rp->output_file_descriptor);
    }
}

static BACKGROUND_TYPE run_adc(GUI_POINTER uss)
{
  BACKGROUND_TYPE val;
  snd_state *ss = (snd_state *)uss;
  val = read_adc(ss);
#if DEBUGGING
  if (!(with_background_processes(ss))) val = BACKGROUND_QUIT;
#endif
  if (val == BACKGROUND_QUIT)
    {
      rp->recording = 0;
      finish_recording(ss, rp);
    }
  return(val);
}

void set_read_in_progress (snd_state *ss)
{
  if (with_background_processes(ss) != 1234)
    rp->recorder_reader = BACKGROUND_ADD(ss, run_adc, ss);
}



static XEN g_recorder_autoload(void) {return(C_TO_XEN_BOOLEAN(rp->autoload));}
static XEN g_set_recorder_autoload(XEN val) 
{
  #define H_recorder_autoload "(" S_recorder_autoload ") -> #t if newly recorded sound should be loaded into Snd automatically"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(val), val, XEN_ONLY_ARG, "set! " S_recorder_autoload, "a boolean");
  set_recorder_autoload(rp, XEN_TO_C_BOOLEAN_OR_TRUE(val));
  return(C_TO_XEN_BOOLEAN(rp->autoload));
}

static XEN g_recorder_buffer_size(void) {return(C_TO_XEN_INT(rp->buffer_size));}
static XEN g_set_recorder_buffer_size(XEN val) 
{
  int size;
  #define H_recorder_buffer_size "(" S_recorder_buffer_size ") -> ADC buffer size (4096)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, "set! " S_recorder_buffer_size, "an integer"); 
  size = XEN_TO_C_INT(val);
  if (size <= 0)
    mus_misc_error("set! " S_recorder_buffer_size, "can't set buffer size <= 0", val);
  rp->buffer_size = size;
  return(val);
}

static XEN g_recorder_file(void) {return(C_TO_XEN_STRING(rp->output_file));}
static XEN g_set_recorder_file(XEN val) 
{
  #define H_recorder_file "(" S_recorder_file ") -> default recorder file name"
  XEN_ASSERT_TYPE(XEN_STRING_P(val) || XEN_FALSE_P(val), val, XEN_ONLY_ARG, "set! " S_recorder_file, "a string"); 
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

static XEN g_recorder_in_format(void) {return(C_TO_XEN_INT(rp->in_format));}
static XEN g_set_recorder_in_format(XEN val) 
{
  #define H_recorder_in_format "(" S_recorder_in_format ") -> default recorder incoming data format (16 bit linear)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, "set! " S_recorder_in_format, "an integer"); 
  rp->in_format = XEN_TO_C_INT(val);
  return(C_TO_XEN_INT(rp->in_format));
}

static XEN g_recorder_in_device(void) {return(C_TO_XEN_INT(rp->in_device));}
static XEN g_set_recorder_in_device(XEN val) 
{
  #define H_recorder_in_device "(" S_recorder_in_device ") -> default recorder input device (mus-audio-line-in)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, "set! " S_recorder_in_device, "an integer"); 
  rp->in_device = XEN_TO_C_INT(val);
  return(C_TO_XEN_INT(rp->in_device));
}

static XEN g_recorder_out_chans(void) {return(C_TO_XEN_INT(rp->out_chans));}
static XEN g_set_recorder_out_chans(XEN val) 
{
  #define H_recorder_out_chans "(" S_recorder_out_chans ") -> default recorder output channels (2)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, "set! " S_recorder_out_chans, "an integer"); 
  rp->out_chans = XEN_TO_C_INT(val);
  return(C_TO_XEN_INT(rp->out_chans));
}

static XEN g_recorder_out_format(void) {return(C_TO_XEN_INT(rp->out_format));}
static XEN g_set_recorder_out_format(XEN val) 
{
  #define H_recorder_out_format "(" S_recorder_out_format ") -> default recorder output data format (16-bit linear)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, "set! " S_recorder_out_format, "an integer"); 
  rp->out_format = XEN_TO_C_INT(val);
  return(C_TO_XEN_INT(rp->out_format));
}

static XEN g_recorder_srate(void) {return(C_TO_XEN_INT(rp->srate));}
static XEN g_set_recorder_srate(XEN val) 
{
  #define H_recorder_srate "(" S_recorder_srate ") -> default recorder sampling rate (22050)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, "set! " S_recorder_srate, "a number"); 
  set_recorder_srate(rp, XEN_TO_C_INT_OR_ELSE(val, 0));
  return(C_TO_XEN_INT(rp->srate));
}

static XEN g_recorder_trigger(void) {return(C_TO_XEN_DOUBLE(rp->trigger));}
static XEN g_set_recorder_trigger(XEN val) 
{
  Float trigger;
  #define H_recorder_trigger "(" S_recorder_trigger ") -> if doing triggered record, min amp that can trigger recording"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, "set! " S_recorder_trigger, "a number"); 
  trigger = XEN_TO_C_DOUBLE(val);
  if (trigger > 1.0) trigger = 1.0;
  if (trigger < 0.0) trigger = 0.0;
  set_recorder_trigger(rp, trigger);
  return(C_TO_XEN_DOUBLE(rp->trigger));
}

static XEN g_recorder_max_duration(void) {return(C_TO_XEN_DOUBLE(rp->max_duration));}
static XEN g_set_recorder_max_duration(XEN val) 
{
  #define H_recorder_max_duration "(" S_recorder_max_duration ") -> max recorder output file length"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, "set! " S_recorder_max_duration, "a number"); 
  rp->max_duration = XEN_TO_C_DOUBLE(val);
  return(C_TO_XEN_DOUBLE(rp->max_duration));
}

static XEN g_recorder_gain (XEN num) 
{
  #define H_recorder_gain "(" S_recorder_gain " gain) -> recorder input (soundcard) gain"
  int g;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(num), num, XEN_ONLY_ARG, S_recorder_gain, "an integer");
  g = XEN_TO_C_INT(num);
  if ((g >= 0) && (g < MAX_MIXER_GAINS))
    return(C_TO_XEN_DOUBLE(rp->mixer_gains[g]));
  return(C_TO_XEN_DOUBLE(0.0));
}

static XEN g_recorder_in_amp (XEN in, XEN out) 
{
  #define H_recorder_in_amp "(" S_recorder_in_amp " in out) -> recorder scaler on input in to output out"
  int ic, oc;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(in), in, XEN_ARG_1, S_recorder_in_amp, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(out), out, XEN_ARG_2, S_recorder_in_amp, "an integer");
  ic = XEN_TO_C_INT(in);
  oc = XEN_TO_C_INT(out);
  if ((ic >= 0) && (ic < MAX_IN_CHANS) && 
      (oc >= 0) && (oc < MAX_OUT_CHANS))
    return(C_TO_XEN_DOUBLE(rp->in_amps[ic][oc]));
  return(C_TO_XEN_DOUBLE(0.0));
}

static XEN g_recorder_out_amp (XEN num) 
{
  #define H_recorder_out_amp "(" S_recorder_out_amp " out) -> recorder output out scaler"
  int oc;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(num), num, XEN_ONLY_ARG, S_recorder_out_amp, "an integer");
  oc = XEN_TO_C_INT(num);
  if ((oc >= 0) && (oc < MAX_OUT_CHANS))
    return(C_TO_XEN_DOUBLE(rp->out_amps[oc]));
  return(C_TO_XEN_DOUBLE(0.0));
}

static XEN g_set_recorder_gain (XEN num, XEN amp) 
{
  int ind;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(num), num, XEN_ARG_1, "set! " S_recorder_gain, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(amp), amp, XEN_ARG_2, "set! " S_recorder_gain, "a number"); 
  ind = XEN_TO_C_INT(num);
  if ((ind >= 0) && (ind < MAX_MIXER_GAINS))
    {
      rp->mixer_gains[ind] = XEN_TO_C_DOUBLE(amp);
      reflect_recorder_mixer_gain(ind, rp->mixer_gains[ind]);
    }
  return(amp);
}

static XEN g_set_recorder_in_amp (XEN in, XEN out, XEN amp) 
{
  int in_ind, out_ind;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(in), in, XEN_ARG_1, "set! " S_recorder_in_amp, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(out), out, XEN_ARG_2, "set! " S_recorder_in_amp, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(amp), amp, XEN_ARG_3, "set! " S_recorder_in_amp, "a number");
  in_ind = XEN_TO_C_INT(in);
  out_ind = XEN_TO_C_INT(out);
  if ((in_ind >= 0) && (in_ind < MAX_IN_CHANS) && 
      (out_ind >= 0) && (out_ind < MAX_OUT_CHANS))
    {
      rp->in_amps[in_ind][out_ind] = XEN_TO_C_DOUBLE(amp);
      reflect_recorder_in_amp(in_ind, 
			      out_ind, 
			      rp->in_amps[in_ind][out_ind]);
    }
  return(amp);
}

static XEN g_set_recorder_out_amp (XEN num, XEN amp) 
{
  int ind;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(num), num, XEN_ARG_1, "set! " S_recorder_out_amp, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(amp), amp, XEN_ARG_2, "set! " S_recorder_out_amp, "a number"); 
  ind = XEN_TO_C_INT(num);
  if ((ind >= 0) && (ind < MAX_OUT_CHANS))
    {
      rp->out_amps[ind] = XEN_TO_C_DOUBLE(amp); 
      reflect_recorder_out_amp(ind,
			       rp->out_amps[ind]);
    }
  return(amp);
}

static XEN g_recorder_dialog(void) 
{
  #define H_recorder_dialog "(" S_recorder_dialog ") fires up the Recorder"
  snd_record_file(get_global_state()); 
  return(XEN_FALSE);
}

static XEN g_vu_font(void) {return(C_TO_XEN_STRING(vu_font(get_global_state())));}
static XEN g_set_vu_font(XEN val) 
{
  #define H_vu_font "(" S_vu_font ") -> name of font used to make VU meter labels (courier)"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_STRING_P(val) || XEN_FALSE_P(val), val, XEN_ONLY_ARG, "set! " S_vu_font, "a string"); 
  if (vu_font(ss)) FREE(vu_font(ss));
  if (XEN_FALSE_P(val))
    set_vu_font(ss, (DEFAULT_VU_FONT) ? copy_string(DEFAULT_VU_FONT) : NULL);
  else set_vu_font(ss, copy_string(XEN_TO_C_STRING(val)));
  return(C_TO_XEN_STRING(vu_font(ss)));
}

static XEN g_vu_font_size(void) {return(C_TO_XEN_DOUBLE(vu_font_size(get_global_state())));}
static XEN g_set_vu_font_size(XEN val) 
{
  #define H_vu_font_size "(" S_vu_font_size ") -> size of VU font meter labels (1.0)"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, "set! " S_vu_font_size, "a number"); 
  set_vu_font_size(ss, XEN_TO_C_DOUBLE(val));
  return(C_TO_XEN_DOUBLE(vu_font_size(ss)));
}

static XEN g_vu_size(void) {return(C_TO_XEN_DOUBLE(vu_size(get_global_state())));}
static XEN g_set_vu_size(XEN val) 
{
  #define H_vu_size "(" S_vu_size ") -> size of VU meters (1.0)"
  snd_state *ss;
  ss = get_global_state();
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, "set! " S_vu_size, "a number"); 
  set_vu_size(ss, XEN_TO_C_DOUBLE(val));
  return(C_TO_XEN_DOUBLE(vu_size(ss)));
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_recorder_autoload_w, g_recorder_autoload)
XEN_ARGIFY_1(g_set_recorder_autoload_w, g_set_recorder_autoload)
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
XEN_ARGIFY_1(g_set_vu_font_w, g_set_vu_font)
XEN_NARGIFY_0(g_vu_font_size_w, g_vu_font_size)
XEN_ARGIFY_1(g_set_vu_font_size_w, g_set_vu_font_size)
XEN_NARGIFY_0(g_vu_size_w, g_vu_size)
XEN_ARGIFY_1(g_set_vu_size_w, g_set_vu_size)
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
				   "set-" S_recorder_autoload, g_set_recorder_autoload_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_buffer_size, g_recorder_buffer_size_w, H_recorder_buffer_size,
				   "set-" S_recorder_buffer_size, g_set_recorder_buffer_size_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_file, g_recorder_file_w, H_recorder_file,
				   "set-" S_recorder_file, g_set_recorder_file_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_in_format, g_recorder_in_format_w, H_recorder_in_format,
				   "set-" S_recorder_in_format, g_set_recorder_in_format_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_in_device, g_recorder_in_device_w, H_recorder_in_device,
				   "set-" S_recorder_in_device, g_set_recorder_in_device_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_out_chans, g_recorder_out_chans_w, H_recorder_out_chans,
				   "set-" S_recorder_out_chans, g_set_recorder_out_chans_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_out_format, g_recorder_out_format_w, H_recorder_out_format,
				   "set-" S_recorder_out_format, g_set_recorder_out_format_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_srate, g_recorder_srate_w, H_recorder_srate,
				   "set-" S_recorder_srate, g_set_recorder_srate_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_trigger, g_recorder_trigger_w, H_recorder_trigger,
				   "set-" S_recorder_trigger, g_set_recorder_trigger_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_max_duration, g_recorder_max_duration_w, H_recorder_max_duration,
				   "set-" S_recorder_max_duration, g_set_recorder_max_duration_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_gain, g_recorder_gain_w, H_recorder_gain,
				   "set-" S_recorder_gain, g_set_recorder_gain_w,  0, 1, 2, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_in_amp, g_recorder_in_amp_w, H_recorder_in_amp,
				   "set-" S_recorder_in_amp, g_set_recorder_in_amp_w,  2, 0, 3, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_recorder_out_amp, g_recorder_out_amp_w, H_recorder_out_amp,
				   "set-" S_recorder_out_amp, g_set_recorder_out_amp_w,  1, 0, 2, 0);

  XEN_DEFINE_PROCEDURE(S_recorder_dialog, g_recorder_dialog_w, 0, 0, 0, H_recorder_dialog);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_vu_font, g_vu_font_w, H_vu_font,
				   "set-" S_vu_font, g_set_vu_font_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_vu_font_size, g_vu_font_size_w, H_vu_font_size,
				   "set-" S_vu_font_size, g_set_vu_font_size_w,  0, 0, 0, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_vu_size, g_vu_size_w, H_vu_size,
				   "set-" S_vu_size, g_set_vu_size_w,  0, 0, 0, 1);

}

#if USE_NO_GUI
  void set_recorder_trigger(recorder_info *rp, Float val) {}
  void set_recorder_srate(recorder_info *rp, int val) {}
  void set_recorder_autoload(recorder_info *rp, int val) {}
  void recorder_set_vu_in_val(int chan, mus_sample_t val) {}
  void recorder_set_vu_out_val(int chan, mus_sample_t val) {}
  void finish_recording(snd_state *ss, recorder_info *rp) {}
  void sensitize_control_buttons(void) {}
  void recorder_fill_wd(void *uwd, int chan, int field, int device) {}
#endif
