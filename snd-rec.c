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

/* now recorder_info dependent code */

static recorder_info *rp = NULL;

recorder_info *get_recorder_info(void) {return(rp);}

void init_recorder(snd_state *ss)
{
  int i;
  rp = (recorder_info *)CALLOC(1,sizeof(recorder_info));
  /* rp->ss = ss; */
  rp->autoload = DEFAULT_RECORDER_AUTOLOAD;
  rp->buffer_size = DEFAULT_RECORDER_BUFFER_SIZE;
  rp->out_chans = DEFAULT_RECORDER_OUT_CHANS;
  rp->out_format = DEFAULT_RECORDER_OUT_FORMAT;
  rp->in_format = DEFAULT_RECORDER_IN_FORMAT;
  rp->srate = DEFAULT_RECORDER_SRATE;
  rp->trigger = DEFAULT_RECORDER_TRIGGER;
  rp->max_duration = DEFAULT_RECORDER_MAX_DURATION;
  rp->output_file = DEFAULT_RECORDER_FILE;
  rp->triggering = 0;
  rp->triggered = 1;
  rp->monitor_out_chans = 2;
  rp->taking_input = 0;
#ifdef LINUX
  rp->output_header_type = MUS_RIFF;
#else
  rp->output_header_type = MUS_AIFC;
#endif

  rp->out_amps = (Float *)CALLOC(MAX_OUT_CHANS,sizeof(Float));
  rp->mixer_gains = (Float *)CALLOC(MAX_MIXER_GAINS,sizeof(Float));
  rp->in_amps = (Float **)CALLOC(MAX_IN_CHANS,sizeof(Float));
  for (i=0;i<MAX_IN_CHANS;i++) rp->in_amps[i] = (Float *)CALLOC(MAX_OUT_CHANS,sizeof(Float));
  rp->chan_in_active = (int *)CALLOC(MAX_IN_CHANS,sizeof(int));
  rp->chan_out_active = (int *)CALLOC(MAX_OUT_CHANS,sizeof(int));
}

static int fneq(Float a, Float b) {return(fabs(a - b) > .00001);}
#if HAVE_GUILE
static char *b2s(int val) {if (val) return("#t"); else return("#f");}
#else
static char *b2s(int val) {if (val) return("1"); else return("0");}
#endif

void save_recorder_state(FILE *fd)
{
  if (rp->autoload != DEFAULT_RECORDER_AUTOLOAD) fprintf(fd,"(%s %s)\n",S_set_recorder_autoload,b2s(rp->autoload));
  if (rp->buffer_size != DEFAULT_RECORDER_BUFFER_SIZE) fprintf(fd,"(%s %d)\n",S_set_recorder_buffer_size,rp->buffer_size);
  if (rp->out_chans != DEFAULT_RECORDER_OUT_CHANS) fprintf(fd,"(%s %d)\n",S_set_recorder_out_chans,rp->out_chans);
  if (rp->out_format != DEFAULT_RECORDER_OUT_FORMAT) fprintf(fd,"(%s %d)\n",S_set_recorder_out_format,rp->out_format);
  if (rp->in_format != DEFAULT_RECORDER_IN_FORMAT) fprintf(fd,"(%s %d)\n",S_set_recorder_in_format,rp->in_format);
  if (rp->srate != DEFAULT_RECORDER_SRATE) fprintf(fd,"(%s %d)\n",S_set_recorder_srate,rp->srate);
  if (rp->output_file != DEFAULT_RECORDER_FILE) fprintf(fd,"(%s \"%s\")\n",S_set_recorder_file,rp->output_file);
  if (fneq(rp->trigger,DEFAULT_RECORDER_TRIGGER)) fprintf(fd,"(%s %.4f)\n",S_set_recorder_trigger,rp->trigger);
  if (fneq(rp->max_duration,DEFAULT_RECORDER_MAX_DURATION)) fprintf(fd,"(%s %.4f)\n",S_set_recorder_max_duration,rp->max_duration);
}

static char numbuf[8];
char *channel_name(int in_chans, int out_chans, int chan)
{
  int use_numbers;
  use_numbers = ((out_chans>4) || (in_chans>4));
  if (use_numbers)
    sprintf(numbuf,"%d",chan+1);
  else sprintf(numbuf,"%c",(char)('A' + chan));
  return(numbuf);
}

char *out_channel_name(snd_state *ss, int chan)
{
  int use_numbers;
  recorder_info *rp;
  rp = get_recorder_info();
  use_numbers = (rp->out_chans>4);
  if (use_numbers)
    sprintf(numbuf,"%d",chan+1);
  else sprintf(numbuf,"%c",(char)('A' + chan));
  return(numbuf);
}

char *gain_channel_name(int in_chans, int out_chans, int input, int dev_in, int out)
{
  int use_numbers;
  if (input)
    {
      use_numbers = ((out_chans>4) || (in_chans>4));
      if (use_numbers)
	sprintf(numbuf,"%d->%d:",dev_in+1,out+1);
      else sprintf(numbuf,"%c->%c:",(char)('A' + dev_in),(char)('A'+out));
    }
  else
    {
      use_numbers = (out_chans > 4);
      if (use_numbers)
	sprintf(numbuf,"%d:",out+1);
      else sprintf(numbuf,"%c:",(char)('A'+out));
    }
  return(numbuf);
}

Float mixer_gain(int system, int device, int chan, int gain, int field)
{
  float g[1];
  recorder_info *rp;
  rp = get_recorder_info();
  mus_audio_mixer_read(MUS_AUDIO_PACK_SYSTEM(system) | (device),field,chan,g);
  if (gain > rp->num_mixer_gains) snd_error("%s[%d] %s: overflow %d > %d",__FILE__,__LINE__,__FUNCTION__,gain,rp->num_mixer_gains);
  rp->mixer_gains[gain]=g[0];
  return(g[0]);
}

void set_mixer_gain(int system, int device, int chan, int gain, int field, Float amp) 
{
  float g[1];
  recorder_info *rp;
  rp = get_recorder_info();
  g[0] = amp;
  if (device == MUS_AUDIO_DAC_FILTER) /* bass or treble control affects both channels at once */
    {
      mus_audio_mixer_write(MUS_AUDIO_PACK_SYSTEM(system) | (device),field,0,g);
      mus_audio_mixer_write(MUS_AUDIO_PACK_SYSTEM(system) | (device),field,1,g);
    }
  else 
    mus_audio_mixer_write(MUS_AUDIO_PACK_SYSTEM(system) | (device),field,chan,g);
  if (gain > rp->num_mixer_gains) snd_error("%s[%d] %s: overflow %d > %d",__FILE__,__LINE__,__FUNCTION__,gain,rp->num_mixer_gains);
  rp->mixer_gains[gain] = amp;
}



#if HAVE_GUILE
#include "sg.h"

static SCM g_recorder_autoload(void) {RTNBOOL(rp->autoload);}
static SCM g_set_recorder_autoload(SCM val) 
{
  #define H_recorder_autoload "(" S_recorder_autoload ") -> #t if newly recorded sound should be loaded into Snd automatically"
  #define H_set_recorder_autoload "(" S_set_recorder_autoload " &optional (val #t)) sets " S_recorder_autoload
  ERRB1(val,S_set_recorder_autoload); 
  set_recorder_autoload(bool_int_or_one(val));
  RTNBOOL(rp->autoload);
}

static SCM g_recorder_buffer_size(void) {RTNINT(rp->buffer_size);}
static SCM g_set_recorder_buffer_size(SCM val) 
{
  #define H_recorder_buffer_size "(" S_recorder_buffer_size ") -> ADC buffer size (4096)"
  #define H_set_recorder_buffer_size "(" S_set_recorder_buffer_size " val) sets " S_recorder_buffer_size
  ERRN1(val,S_set_recorder_buffer_size); 
  rp->buffer_size = g_scm2int(val);
  RTNINT(rp->buffer_size);
}

static SCM g_recorder_file(void) {RTNSTR(rp->output_file);}
static SCM g_set_recorder_file(SCM val) 
{
  #define H_recorder_file "(" S_recorder_file ") -> default recorder file name"
  #define H_set_recorder_file "(" S_set_recorder_file " val) sets " S_recorder_file
  ERRS1(val,S_set_recorder_file); 
  rp->output_file = gh_scm2newstr(val,0);
  RTNSTR(rp->output_file);
}

static SCM g_recorder_in_format(void) {RTNINT(rp->in_format);}
static SCM g_set_recorder_in_format(SCM val) 
{
  #define H_recorder_in_format "(" S_recorder_in_format ") -> default recorder incoming data format (16 bit linear)"
  #define H_set_recorder_in_format "(" S_set_recorder_in_format " val) sets " S_recorder_in_format
  ERRN1(val,S_set_recorder_in_format); 
  rp->in_format = g_scm2int(val);
  RTNINT(rp->in_format);
}

static SCM g_recorder_out_chans(void) {RTNINT(rp->out_chans);}
static SCM g_set_recorder_out_chans(SCM val) 
{
  #define H_recorder_out_chans "(" S_recorder_out_chans ") -> default recorder output channels (2)"
  #define H_set_recorder_out_chans "(" S_set_recorder_out_chans " val) sets " S_recorder_out_chans
  ERRN1(val,S_set_recorder_out_chans); 
  rp->out_chans = g_scm2int(val);
  RTNINT(rp->out_chans);
}

static SCM g_recorder_out_format(void) {RTNINT(rp->out_format);}
static SCM g_set_recorder_out_format(SCM val) 
{
  #define H_recorder_out_format "(" S_recorder_out_format ") -> default recorder output data format (16-bit linear)"
  #define H_set_recorder_out_format "(" S_set_recorder_out_format " val) sets " S_recorder_out_format
  ERRN1(val,S_set_recorder_out_format); 
  rp->out_format = g_scm2int(val);
  RTNINT(rp->out_format);
}

static SCM g_recorder_srate(void) {RTNINT(rp->srate);}
static SCM g_set_recorder_srate(SCM val) 
{
  #define H_recorder_srate "(" S_recorder_srate ") -> default recorder sampling rate (22050)"
  #define H_set_recorder_srate "(" S_set_recorder_srate " val) sets " S_recorder_srate
  ERRN1(val,S_set_recorder_srate); 
  set_recorder_srate(g_scm2int(val));
  RTNINT(rp->srate);
}

static SCM g_recorder_trigger(void) {RTNFLT(rp->trigger);}
static SCM g_set_recorder_trigger(SCM val) 
{
  #define H_recorder_trigger "(" S_recorder_trigger ") -> if doing triggered record, min amp that can trigger recording"
  #define H_set_recorder_trigger "(" S_set_recorder_trigger " val) sets " S_recorder_trigger
  ERRN1(val,S_set_recorder_trigger); 
  set_recorder_trigger(gh_scm2double(val));
  RTNFLT(rp->trigger);
}

static SCM g_recorder_max_duration(void) {RTNFLT(rp->max_duration);}
static SCM g_set_recorder_max_duration(SCM val) 
{
  #define H_recorder_max_duration "(" S_recorder_max_duration ") -> max recorder output file length"
  #define H_set_recorder_max_duration "(" S_set_recorder_max_duration " val) sets " S_recorder_max_duration
  ERRN1(val,S_set_recorder_max_duration); 
  rp->max_duration = gh_scm2double(val);
  RTNFLT(rp->max_duration);
}

static SCM g_recorder_gain (SCM num) 
{
  #define H_recorder_gain "(" S_recorder_gain " gain) -> recorder input (soundcard) gain"
  RTNFLT(rp->mixer_gains[g_scm2int(num)]);
}

static SCM g_recorder_in_amp (SCM in, SCM out) 
{
  #define H_recorder_in_amp "(" S_recorder_in_amp " in out) -> recorder scaler on input in to output out"
  RTNFLT(rp->in_amps[g_scm2int(in)][g_scm2int(out)]);
}

static SCM g_recorder_out_amp (SCM num) 
{
  #define H_recorder_out_amp "(" S_recorder_out_amp " out) -> recorder output out scaler"
  RTNFLT(rp->out_amps[g_scm2int(num)]);
}

static SCM g_set_recorder_gain (SCM num, SCM amp) 
{
  #define H_set_recorder_gain "(" S_set_recorder_gain " num amp) sets recorder input gain num to amp"
  int ind;
  ERRN1(num,S_set_recorder_gain);
  ERRN2(amp,S_set_recorder_gain); 
  ind = g_scm2int(num);
  rp->mixer_gains[ind] = gh_scm2double(amp);
  reflect_recorder_mixer_gain(ind,rp->mixer_gains[ind]);
  return(amp);
}

static SCM g_set_recorder_in_amp (SCM in, SCM out, SCM amp) 
{
  #define H_set_recorder_in_amp "(" S_set_recorder_in_amp " in out amp) sets recorder scaler on input in to output out to amp"
  int in_ind,out_ind;
  ERRN1(in,S_set_recorder_in_amp);
  ERRN2(out,S_set_recorder_in_amp);
  ERRN3(amp,S_set_recorder_in_amp);
  in_ind = g_scm2int(in);
  out_ind = g_scm2int(out);
  rp->in_amps[in_ind][out_ind] = gh_scm2double(amp);
  reflect_recorder_in_amp(in_ind,out_ind,rp->in_amps[in_ind][out_ind]);
  return(amp);
}

static SCM g_set_recorder_out_amp (SCM num, SCM amp) 
{
  #define H_set_recorder_out_amp "(" S_set_recorder_out_amp " num amp) sets recorder output gain num to amp"
  int ind;
  ERRN1(num,S_set_recorder_out_amp);
  ERRN2(amp,S_set_recorder_out_amp); 
  ind = g_scm2int(num);
  rp->out_amps[ind] = gh_scm2double(amp); 
  reflect_recorder_out_amp(ind,rp->out_amps[ind]);
  return(amp);
}

static SCM g_recorder_dialog(void) 
{
  #define H_recorder_dialog "(" S_recorder_dialog ") fires up the Recorder"
  snd_record_file(get_global_state()); 
  return(SCM_BOOL_F);
}

void g_init_recorder(SCM local_doc)
{
  DEFINE_PROC(gh_new_procedure0_0(S_recorder_autoload,g_recorder_autoload),H_recorder_autoload);
  DEFINE_PROC(gh_new_procedure0_1(S_set_recorder_autoload,g_set_recorder_autoload),H_set_recorder_autoload);
  DEFINE_PROC(gh_new_procedure0_0(S_recorder_buffer_size,g_recorder_buffer_size),H_recorder_buffer_size);
  DEFINE_PROC(gh_new_procedure1_0(S_set_recorder_buffer_size,g_set_recorder_buffer_size),H_set_recorder_buffer_size);
  DEFINE_PROC(gh_new_procedure0_0(S_recorder_file,g_recorder_file),H_recorder_file);
  DEFINE_PROC(gh_new_procedure1_0(S_set_recorder_file,g_set_recorder_file),H_set_recorder_file);
  DEFINE_PROC(gh_new_procedure0_0(S_recorder_in_format,g_recorder_in_format),H_recorder_in_format);
  DEFINE_PROC(gh_new_procedure1_0(S_set_recorder_in_format,g_set_recorder_in_format),H_set_recorder_in_format);
  DEFINE_PROC(gh_new_procedure0_0(S_recorder_out_chans,g_recorder_out_chans),H_recorder_out_chans);
  DEFINE_PROC(gh_new_procedure1_0(S_set_recorder_out_chans,g_set_recorder_out_chans),H_set_recorder_out_chans);
  DEFINE_PROC(gh_new_procedure0_0(S_recorder_out_format,g_recorder_out_format),H_recorder_out_format);
  DEFINE_PROC(gh_new_procedure1_0(S_set_recorder_out_format,g_set_recorder_out_format),H_set_recorder_out_format);
  DEFINE_PROC(gh_new_procedure0_0(S_recorder_srate,g_recorder_srate),H_recorder_srate);
  DEFINE_PROC(gh_new_procedure1_0(S_set_recorder_srate,g_set_recorder_srate),H_set_recorder_srate);
  DEFINE_PROC(gh_new_procedure0_0(S_recorder_trigger,g_recorder_trigger),H_recorder_trigger);
  DEFINE_PROC(gh_new_procedure1_0(S_set_recorder_trigger,g_set_recorder_trigger),H_set_recorder_trigger);
  DEFINE_PROC(gh_new_procedure0_0(S_recorder_max_duration,g_recorder_max_duration),H_recorder_max_duration);
  DEFINE_PROC(gh_new_procedure1_0(S_set_recorder_max_duration,g_set_recorder_max_duration),H_set_recorder_max_duration);
  DEFINE_PROC(gh_new_procedure1_0(S_recorder_gain,g_recorder_gain),H_recorder_gain);
  DEFINE_PROC(gh_new_procedure2_0(S_recorder_in_amp,g_recorder_in_amp),H_recorder_in_amp);
  DEFINE_PROC(gh_new_procedure1_0(S_recorder_out_amp,g_recorder_out_amp),H_recorder_out_amp);
  DEFINE_PROC(gh_new_procedure2_0(S_set_recorder_gain,g_set_recorder_gain),H_set_recorder_gain);
  DEFINE_PROC(gh_new_procedure3_0(S_set_recorder_in_amp,g_set_recorder_in_amp),H_set_recorder_in_amp);
  DEFINE_PROC(gh_new_procedure2_0(S_set_recorder_out_amp,g_set_recorder_out_amp),H_set_recorder_out_amp);
  DEFINE_PROC(gh_new_procedure0_0(S_recorder_dialog,g_recorder_dialog),H_recorder_dialog);
}

#endif
