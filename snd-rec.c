/* snd-xrec and snd-grec shared code */

#include "snd.h"
#include "snd-rec.h"

void recorder_set_audio_srate(snd_state *ss, int device, int srate, int system, int aud)
{
  float g[1];
#if (!NEW_SGI_AL)
  if (aud) close_recorder_audio();
  g[0] = (Float)srate;
  mus_audio_mixer_write(MUS_AUDIO_PACK_SYSTEM(system) | device,MUS_AUDIO_SRATE,0,g);
  if (aud) fire_up_recorder(ss);
#endif
}

char *recorder_device_name(int dev)
{
  /* format label at top of pane */
  switch (dev)
    {
    case MUS_AUDIO_DIGITAL_OUT: return(STR_Digital_Out); break;
    case MUS_AUDIO_LINE_OUT:    return(STR_Line_Out); break;
    case MUS_AUDIO_DEFAULT: 
    case MUS_AUDIO_DAC_OUT:     return(STR_Output); break; /* default here means that linuxppc reports "Output" as analog-in pane name */
    case MUS_AUDIO_DUPLEX_DEFAULT: 
    case MUS_AUDIO_SPEAKERS:    return(STR_Speakers); break;
    case MUS_AUDIO_ADAT_IN:     return(STR_Adat_In); break;
    case MUS_AUDIO_AES_IN:      return(STR_Aes_In); break;
#if (HAVE_OSS || HAVE_ALSA)
    case MUS_AUDIO_LINE_IN:     return(STR_Analog_In); break;
#else
    case MUS_AUDIO_LINE_IN:     return(STR_Line_In); break;
#endif
    case MUS_AUDIO_MICROPHONE:  return(STR_Microphone); break;
    case MUS_AUDIO_DIGITAL_IN:  return(STR_Digital_In); break;
    case MUS_AUDIO_ADAT_OUT:    return(STR_Adat_Out); break;
    case MUS_AUDIO_AES_OUT:     return(STR_Aes_Out); break;
    case MUS_AUDIO_DAC_FILTER:  return("Tone"); break;
    case MUS_AUDIO_MIXER:       return("Mixer"); break;
    case MUS_AUDIO_AUX_INPUT:   return("Aux Input"); break;
    case MUS_AUDIO_CD:          return("CD"); break;
    case MUS_AUDIO_AUX_OUTPUT:  return("Aux Output"); break;
    case MUS_AUDIO_SPDIF_IN:    return("S/PDIF In"); break;
    case MUS_AUDIO_SPDIF_OUT:   return("S/PDIF Out"); break;
    default: snd_error("%s[%d] %s: unknown device: %d",__FILE__,__LINE__,__FUNCTION__,dev); return(STR_Input); break;
    }
}

static char sysdevstr[32];
char *recorder_system_and_device_name(int sys, int dev)
{
  if (strcmp("OSS",mus_audio_system_name(sys)) == 0) return(recorder_device_name(dev));
  sprintf(sysdevstr,"%s: %s",mus_audio_system_name(sys),recorder_device_name(dev));
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
  snd_error("%s[%d] %s: uncategorized device: %d",__FILE__,__LINE__,__FUNCTION__,dev);
  return(0);
}

int recorder_output_device(int dev)
{
  return((dev != MUS_AUDIO_DAC_FILTER) && (dev != MUS_AUDIO_MIXER) && (!(recorder_input_device(dev))));
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
    case MUS_AUDIO_MIXER:      return("various analog volume controls");break;
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
    case MUS_AUDIO_CD:     return("cd"); break;
    }
  return("oops");
}

char *recorder_field_name(int fld)
{
  switch (fld)
    {
    case MUS_AUDIO_IMIX:   return("imix"); break;
    case MUS_AUDIO_IGAIN:  return("igain"); break;
    case MUS_AUDIO_RECLEV: return("reclev"); break;
    case MUS_AUDIO_PCM:    return("pcm"); break;
    case MUS_AUDIO_PCM2:   return("pcm2"); break;
    case MUS_AUDIO_OGAIN:  return("ogain"); break;
    case MUS_AUDIO_LINE:   return("line-in"); break;
    case MUS_AUDIO_MICROPHONE:    return("mic"); break;
    case MUS_AUDIO_LINE1:  return("line1"); break;
    case MUS_AUDIO_LINE2:  return("line2"); break; 
    case MUS_AUDIO_LINE3:  return("line3"); break;
    case MUS_AUDIO_SYNTH:  return("synth"); break;
    case MUS_AUDIO_CD:     return("cd"); break;
    default: return("?"); break;
    }
}

char *recorder_field_function(int fld)
{
  switch (fld)
    {
    case MUS_AUDIO_IMIX:   return("the pre-adc mix of mic and line-in"); break;
    case MUS_AUDIO_IGAIN:  return("input gain"); break;
    case MUS_AUDIO_RECLEV: return("recording level"); break;
    case MUS_AUDIO_PCM:    return("the speaker level, perhaps"); break;
    case MUS_AUDIO_PCM2:   return("nothing in particular"); break;
    case MUS_AUDIO_OGAIN:  return("output gain"); break;
    case MUS_AUDIO_LINE:   return("analog line-in"); break;
    case MUS_AUDIO_MICROPHONE:    return("the microphone"); break;
    case MUS_AUDIO_LINE1:  
    case MUS_AUDIO_LINE2:  
    case MUS_AUDIO_LINE3:  return("extra line inputs"); break;
    case MUS_AUDIO_SYNTH:  return("the on-card synthesizer, if any"); break;
    case MUS_AUDIO_CD:     return("the cd gain"); break;
    default: return("?"); break;
    }
}
#endif

