/* this file will gradually wither away... */
/* it could be omitted now with no great loss */

#include "snd.h"

#if (!HAVE_GUILE)

#include "sndlib-strings.h"

#define NOSCM_BUFFER_SIZE 16384
static char *noscm_buffer = NULL;
static int noscm_buffer_size = 0;

static int eval_forever(snd_state *ss, int fd)
{
  int bytes,i,j,k,m,n,start,commenting,len,parens,quoting,loop_start;
  char *tmp;
  if (!noscm_buffer)
    {
      noscm_buffer = (char *)CALLOC(NOSCM_BUFFER_SIZE,sizeof(char));
      noscm_buffer_size = NOSCM_BUFFER_SIZE;
    }
  bytes=read(fd,noscm_buffer,noscm_buffer_size);
  if (bytes == 0) return(0);
  noscm_buffer[bytes] = '\0';
  commenting = 0;
  parens = 0;
  quoting = FALSE;
  start = 0;
  loop_start = 0;
READ_BUFFER:
  for (i=loop_start;i<bytes;i++)
    {
      if (!quoting)
	{
	  if ((commenting == 0) && (noscm_buffer[i] == ';')) commenting = 1;
	  else if ((commenting == 1) && (noscm_buffer[i] == '\n')) commenting = 0;
	  else if ((commenting == 0) && (noscm_buffer[i] == '#') && (noscm_buffer[i+1] == '|')) commenting = 2;
	  else if ((commenting == 2) && (noscm_buffer[i] == '|') && (noscm_buffer[i+1] == '#')) commenting = 0;
	}
      if (noscm_buffer[i] == '\"') quoting = (!quoting);
      if (commenting == 0)
	{
	  if (noscm_buffer[i] == '(') {if (parens == 0) start = i; parens++;}
	  else if (noscm_buffer[i] == ')') 
	    {
	      parens--; 
	      if (parens == 0) 
		{
		  tmp = (char *)CALLOC(i-start+2,sizeof(char));
		  for (m=0,n=start;n<=i;m++,n++) tmp[m] = noscm_buffer[n];
		  tmp[m]='\0';
		  snd_eval_str(ss,tmp,1);
		  FREE(tmp);
		}
	    }
	}
    }
  if (parens)
    {
      if (start != 0)
	for (i=start,j=0;i<bytes;i++,j++) noscm_buffer[j] = noscm_buffer[i];
      len = bytes - start;
      if (len == noscm_buffer_size)
	{
	  noscm_buffer_size *= 2;
	  noscm_buffer = (char *)REALLOC(noscm_buffer,noscm_buffer_size*sizeof(char));
	}
      noscm_buffer[len] = '\0';
      loop_start = len;
      k = read(fd,(char *)(noscm_buffer+len),noscm_buffer_size - len);
      if (k <= 0) 
	{
	  snd_error("error reading file: extra ')'?\ncurrent expression started at byte %d\n",start);
	  return(0); /* this ends the read loops below */
	}
      bytes = len + k;
      goto READ_BUFFER;
    }
  return(bytes);
}

static char noscm_white_space[4]={' ','\t','\n',')'};
static char noscm_null_string[1] = {'\0'};
static int no_guile_errors = 0;

#define RES_SIZE 512
static char res_buf[RES_SIZE];

static void display_results(snd_state *ss, char *msg)
{
  if (ss->mx_sp) 
    report_in_minibuffer(ss->mx_sp,msg);
  if (ss->listening != LISTENER_CLOSED)
    {
      ss->result_printout = MESSAGE_WITH_CARET;
      snd_append_command(ss,msg);
    }
}

static void fsym(snd_state *ss, Float val)
{
  sprintf(res_buf,"%f",val);
  display_results(ss,res_buf);
}

static void isym(snd_state *ss, int val)
{
  sprintf(res_buf,"%d",val);
  display_results(ss,res_buf);
}

static void ssym(snd_state *ss, char *val)
{
  if (val)
    sprintf(res_buf,"\"%s\"",val); /* sun gets a segfault here if val is null! */
  else sprintf(res_buf,"\"\"");
  display_results(ss,res_buf);
}

static Float fstr(char *str)
{
  Float val = 0.0;
  if ((str) && (*str)) sscanf(str,"%f",&val);
  return(val);
}

static int istr(char *str)
{
  int val = 0;
  if ((str) && (*str)) sscanf(str,"%d",&val);
  return(val);
}

static char *sstr(char *val)
{
  /* strip off double quotes, if any */
  int len;
  if ((val) && (*val))
    {
      if (val[0] == '\"') val++;
      len = strlen(val);
      if (val[len-1] == '\"') val[len-1] = 0;
    }
  return(val);
}

typedef struct {
  char *name;
  env *val;
} lv;

static int env_table_size = 0;
static int env_table_top = 0;
static lv **env_table = NULL;

static void add_symbol(char *name, env *val)
{
  lv *v;
  if (env_table_top == env_table_size)
    {
      env_table_size += 16;
      if (env_table_top == 0) 
	env_table = (lv **)CALLOC(env_table_size,sizeof(lv));
      else env_table = (lv **)REALLOC(env_table,env_table_size * sizeof(lv));
    }
  v = (lv *)CALLOC(1,sizeof(lv));
  v->name = copy_string(name);
  v->val = copy_env(val);
  env_table[env_table_top] = v;
  env_table_top++;
}

static lv *find_symbol(char *name) 
{
  int i;
  for (i=0;i<env_table_top;i++) 
    if (strcmp(name,env_table[i]->name) == 0) return(env_table[i]);
  return(NULL);
}

static snd_info *get_sp(snd_state *state, char *str)
{
  int snd_n;
  if ((str) && (*str))
    sscanf(str,"%d",&snd_n);
  else 
    if ((state->selected_sound != NO_SELECTION) && (snd_ok(state->sounds[state->selected_sound])))
      snd_n = state->selected_sound;
    else snd_n = 0;
  if ((snd_n >= 0) && (snd_n < state->max_sounds) && (snd_ok(state->sounds[snd_n])))
    return(state->sounds[snd_n]);
  else return(any_selected_sound(state));
}

static chan_info *get_cp(snd_state *state, char *sstr, char *cstr)
{
  snd_info *sp;
  int chn_n;
  sp = get_sp(state,sstr);
  if (sp) 
    {
      if ((cstr) && (*cstr))
	sscanf(cstr,"%d",&chn_n);
      else
	if (sp->selected_channel != NO_SELECTION) 
	  chn_n = sp->selected_channel;
	else chn_n = 0;
      if ((chn_n >= 0) && (chn_n < sp->nchans)) return(sp->chans[chn_n]);
    }
  display_results(state,"no such channel");
  return(NULL);
}

static void g_save_envelopes(char *name)
{
  FILE *fd;
  if (name == NULL) name = "envs.save";
  fd = fopen(name,"w");
  if (fd)
    {
      save_envelope_editor_state(fd);
      fclose(fd);
    }
}


void g_snd_callback(int callb) {}

env *name_to_env(char *str) 
{
  lv *e;
  e = find_symbol(str);
  if (e) return(copy_env(e->val));
  return(NULL);
}

#define ENV_BUFFER_SIZE 128
static int env_buffer_size = 0;
static Float *env_buffer = NULL;
static char env_white_space[5]={' ','(',')','\t','\''};

static env *scan_envelope(char *str)
{
  char *tok;
  int i;
  float f;
  if ((str) && (*str))
    {
      i = 0;
      if (env_buffer_size == 0)
	{
	  env_buffer_size = ENV_BUFFER_SIZE;
	  env_buffer = (Float *)CALLOC(ENV_BUFFER_SIZE,sizeof(Float));
	}
      if ((*str) == '\'') str++;
      if ((*str) == '(') str++;
      tok = strtok(str,env_white_space);
      while (tok)
	{
	  sscanf(tok,"%f",&f);
	  env_buffer[i]=(Float)f;
	  i++;
	  if (i == env_buffer_size)
	    {
	      env_buffer_size *= 2;
	      env_buffer = (Float *)REALLOC(env_buffer,env_buffer_size * sizeof(Float));
	    }
	  tok = strtok(NULL,env_white_space);
	}
      if ((i==0) || (i&1)) return(NULL); /* no data or odd length ? */
      return(make_envelope(env_buffer,i));
    }
  return(NULL);
}

env *string2env(char *str) {return(scan_envelope(str));}
Float string2Float(char *str) {return(fstr(str));}
int string2int(char *str) {return(istr(str));}
/* char *string2string(char *str) {return(copy_string(str));} */

static void pass_name_to_envelope_editor(snd_state *ss, char *fullstr) 
{
  /* fullstr is the incoming "defvar ...)" */
  char *name,*val,*defv;
  env *e;
  defv = strtok(fullstr,noscm_white_space);
  name = strtok(NULL,noscm_white_space);
  val = strtok(NULL,noscm_null_string);
  e = scan_envelope(val);
  if (e)
    {
      alert_envelope_editor(ss,name,e);
      add_symbol(name,e);
    }
}

void add_or_edit_symbol(char *name, env *val)
{
  lv *v;
  v = find_symbol(name);
  if (v)
    v->val = copy_env(val);
  else add_symbol(name,val);
}

static int symit(snd_state *ss,char **str);

void snd_eval_listener_str(snd_state *ss, char *buf) 
{
  int val;
  /* this is called from the lisp listener when the user has typed in a form (so don't echo it) */
  ss->result_printout = MESSAGE_WITH_CARET;
  val = snd_eval_str(ss,buf,1);
  ss->result_printout = PLAIN_MESSAGE;
}

void snd_eval_stdin_str(snd_state *ss, char *buf)
{
  snd_eval_str(ss,buf,1);
}

static int handle_set(snd_state *ss, char *tok, char **str)
{
  snd_info *sp;
  chan_info *cp;
  int ival;
  MUS_SAMPLE_TYPE samp_vals[2];
  if (strcmp(tok,"set-" S_ask_before_overwrite) == 0) {set_ask_before_overwrite(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_audio_input_device) == 0) {set_audio_input_device(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_audio_output_device) == 0) {set_audio_output_device(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_audio_state_file) == 0) {set_audio_state_file(ss,sstr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_auto_resize) == 0) {set_auto_resize(ss,istr(str[1])); reflect_resize(ss); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_auto_update) == 0) {set_auto_update(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_channel_style) == 0) {set_channel_style(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_color_cutoff) == 0) {set_color_cutoff(ss,fstr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_color_inverted) == 0) {set_color_inverted(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_color_scale) == 0) {set_color_scale(ss,fstr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_colormap) == 0) {set_color_map(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_corruption_time) == 0) {set_corruption_time(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_cursor) == 0) 
    {
      cp = get_cp(ss,str[2],str[3]); 
      if (cp) 
	{
	  cp->cursor_on = 1;
	  handle_cursor(cp,cursor_moveto(cp,istr(str[1]))); 
	}
      isym(ss,0); 
      return(0);
    }
  if (strcmp(tok,"set-" S_cursor_follows_play) == 0) 
    {sp = get_sp(ss,str[2]); if (sp) sp->cursor_follows_play = istr(str[1]); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_dac_size) == 0) {set_dac_size(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_dac_folding) == 0) {set_dac_folding(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_data_clipped) == 0) {set_data_clipped(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_default_output_chans) == 0) {set_default_output_chans(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_default_output_srate) == 0) {set_default_output_srate(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_default_output_type) == 0) {set_default_output_type(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_default_output_format) == 0) {set_default_output_format(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_enved_base) == 0) {in_set_enved_base(ss,fstr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_enved_clipping) == 0) {in_set_enved_clipping(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_enved_exping) == 0) {in_set_enved_exping(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_enved_target) == 0) {in_set_enved_target(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_enved_waving) == 0) {in_set_enved_waving(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_enved_dBing) == 0) {in_set_enved_dBing(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_eps_file) == 0) {set_eps_file(ss,sstr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_expand) == 0) 
    {
      sp = get_sp(ss,str[2]); 
      if (sp) set_snd_expand(sp,fstr(str[1])); 
      isym(ss,0); 
      return(0);
    }
  if (strcmp(tok,"set-" S_expand_hop) == 0) {sp = get_sp(ss,str[2]); if (sp) sp->expand_hop = fstr(str[1]); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_expand_length) == 0) {sp = get_sp(ss,str[2]); if (sp) sp->expand_length = fstr(str[1]); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_expand_ramp) == 0) {sp = get_sp(ss,str[2]); if (sp) sp->expand_ramp = fstr(str[1]); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_expanding) == 0) {sp = get_sp(ss,str[2]); if (sp) toggle_expand_button(sp,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_fft_beta) == 0) {set_fft_beta(ss,fstr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_fft_log_frequency) == 0) {set_fft_log_frequency(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_fft_log_magnitude) == 0) {set_fft_log_magnitude(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_fft_size) == 0) {set_fft_size(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_fft_style) == 0) {set_fft_style(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_fft_window) == 0) {set_fft_window(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_ffting) == 0) {cp = get_cp(ss,str[2],str[3]); if (cp) fftb(cp,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_filter_dBing) == 0) {sp = get_sp(ss,str[2]); if (sp) set_filter_dBing(sp,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_filter_env_order) == 0) {set_filter_env_order(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_filter_order) == 0) {sp = get_sp(ss,str[2]); if (sp) set_snd_filter_order(sp,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_filter_env) == 0) 
    {sp = get_sp(ss,str[2]); if (sp) {sp->filter_env = scan_envelope(str[1]); filter_env_changed(sp,sp->filter_env); isym(ss,0); return(0);}}
  if (strcmp(tok,"set-" S_filtering) == 0) {sp = get_sp(ss,str[2]); if (sp) toggle_filter_button(sp,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_fit_data_on_open) == 0) {set_fit_data_on_open(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_graph_style) == 0) {in_set_graph_style(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_graph_style) == 0) {set_graph_style(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_graphing) == 0) {cp = get_cp(ss,str[2],str[3]); if (cp) cp->lisp_graphing = istr(str[1]); isym(ss,0); return(0);}
#if HAVE_HTML
  if (strcmp(tok,"set-" S_html_dir) == 0) {set_html_dir(ss,sstr(str[1])); isym(ss,0); return(0);}
#endif
  if (strcmp(tok,"set-" S_initial_x0) == 0) {set_initial_x0(ss,fstr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_initial_x1) == 0) {set_initial_x1(ss,fstr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_initial_y0) == 0) {set_initial_y0(ss,fstr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_initial_y1) == 0) {set_initial_y1(ss,fstr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_movies) == 0) {set_movies(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_normalize_fft) == 0) {set_normalize_fft(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_normalize_on_open) == 0) {set_normalize_on_open(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_oss_buffers) == 0)
    {
#if (HAVE_OSS || HAVE_ALSA)
      mus_audio_set_oss_buffers(istr(str[1]),istr(str[2]));
#endif
      isym(ss,0);
      return(0);
    }
  if (strcmp(tok,"set-" S_previous_files_sort) == 0) {set_previous_files_sort(ss,istr(str[1])); update_prevfiles(ss); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_print_length) == 0) {set_print_length(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_raw_chans) == 0) {set_raw_chans(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_raw_format) == 0) {set_raw_format(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_raw_srate) == 0) {set_raw_srate(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_read_only) == 0) 
    {sp = get_sp(ss,str[2]); if (sp) {ival = istr(str[1]); sp->read_only = ival; snd_file_lock_icon(sp,ival);} isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_reverb_feedback) == 0) {sp = get_sp(ss,str[2]); if (sp) sp->revfb = fstr(str[1]); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_reverb_length) == 0) 
    {
      sp = get_sp(ss,str[2]); 
      if (sp) set_snd_revlen(sp,fstr(str[1])); 
      isym(ss,0); 
      return(0);
    }
  if (strcmp(tok,"set-" S_reverb_lowpass) == 0) {sp = get_sp(ss,str[2]); if (sp) sp->revlp = fstr(str[1]); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_reverb_scale) == 0) 
    {
      sp = get_sp(ss,str[2]); 
      if (sp) set_snd_revscl(sp,fstr(str[1])); 
      isym(ss,0); return(0);
    }
  if (strcmp(tok,"set-" S_reverbing) == 0) {sp = get_sp(ss,str[2]); if (sp) toggle_reverb_button(sp,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_show_fft_peaks) == 0) {set_show_fft_peaks(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_show_marks) == 0) {set_show_marks(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_show_selection_transform) == 0) {set_show_selection_transform(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_show_usage_stats) == 0) {set_show_usage_stats(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_show_y_zero) == 0) {set_show_y_zero(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_show_controls) == 0) 
    {sp = get_sp(ss,str[2]); if (sp) {if (istr(str[1])) sound_show_ctrls(sp); else sound_hide_ctrls(sp);} isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_sinc_width) == 0) {set_sinc_width(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_spectro_cutoff) == 0) {set_spectro_cutoff_and_redisplay(ss,fstr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_spectro_hop) == 0) {set_spectro_hop(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_spectro_x_angle) == 0) {set_spectro_x_angle(ss,fstr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_spectro_x_scale) == 0) {set_spectro_x_scale(ss,fstr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_spectro_y_angle) == 0) {set_spectro_y_angle(ss,fstr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_spectro_y_scale) == 0) {set_spectro_y_scale(ss,fstr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_spectro_z_angle) == 0) {set_spectro_z_angle(ss,fstr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_spectro_z_scale) == 0) {set_spectro_z_scale(ss,fstr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_speed) == 0) 
    {
      sp = get_sp(ss,str[2]); 
      if (sp) set_snd_srate(sp,fstr(str[1])); 
      isym(ss,0); 
      return(0);
    }
  if (strcmp(tok,"set-" S_speed_style) == 0) {activate_speed_in_menu(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_squelch_update) == 0) {cp = get_cp(ss,str[2],str[3]); if (cp) cp->squelch_update = istr(str[1]); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_sync) == 0) {sp = get_sp(ss,str[2]); if (sp) syncb(sp,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_temp_dir) == 0) {set_temp_dir(ss,sstr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_transform_type) == 0) {set_transform_type(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_trap_segfault) == 0) {set_trap_segfault(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_uniting) == 0) {sp = get_sp(ss,str[2]); if (sp) combineb(sp,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_use_raw_defaults) == 0) {set_use_raw_defaults(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_use_sinc_interp) == 0) {set_use_sinc_interp(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_verbose_cursor) == 0) {set_verbose_cursor(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_vu_font) == 0) {set_vu_font(ss,copy_string(sstr(str[1]))); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_vu_font_size) == 0) {set_vu_font_size(ss,fstr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_vu_size) == 0) {set_vu_size(ss,fstr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_wavelet_type) == 0) {set_wavelet_type(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_waving) == 0) {cp = get_cp(ss,str[2],str[3]); if (cp) waveb(cp,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_window_height) == 0) 
    {
      ival = istr(str[1]);
      set_widget_height(MAIN_SHELL(ss),ival);
      ss->init_window_height = ival;
      isym(ss,0); 
      return(0);
    }
  if (strcmp(tok,"set-" S_window_width) == 0) 
    {
      ival = istr(str[1]);
      set_widget_width(MAIN_SHELL(ss),ival);
      ss->init_window_width = ival;
      isym(ss,0); 
      return(0);
    }
  if (strcmp(tok,"set-" S_window_x) == 0) 
    {
      ival = istr(str[1]);
      set_widget_x(MAIN_SHELL(ss),istr(str[1])); 
      ss->init_window_x = ival;
      isym(ss,0); 
      return(0);
    }
  if (strcmp(tok,"set-" S_window_y) == 0) 
    {
      ival = istr(str[1]);
      set_widget_y(MAIN_SHELL(ss),istr(str[1])); 
      ss->init_window_y = ival;
      isym(ss,0); 
      return(0);
    }
  if (strcmp(tok,"set-" S_with_mix_tags) == 0) {set_with_mix_tags(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_x_axis_style) == 0) {in_set_x_axis_style(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_zoom_focus_style) == 0) {activate_focus_menu(ss,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_amp) == 0) 
    {
      sp = get_sp(ss,str[2]); 
      if (sp) set_snd_amp(sp,fstr(str[1])); 
      isym(ss,0); 
      return(0);
    }
  if (strcmp(tok,"set-" S_contrast) == 0) 
    {
      sp = get_sp(ss,str[2]); 
      if (sp) set_snd_contrast(sp,fstr(str[1])); 
      isym(ss,0); 
      return(0);
    }
  if (strcmp(tok,"set-" S_contrast_amp) == 0) {sp = get_sp(ss,str[2]); if (sp) sp->contrast_amp = fstr(str[1]); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_contrasting) == 0) {sp = get_sp(ss,str[2]); if (sp) toggle_contrast_button(sp,istr(str[1])); isym(ss,0); return(0);}
  if (strcmp(tok,"set-" S_sample) == 0) 
    {
      cp = get_cp(ss,str[3],str[4]); 
      if (cp) 
	{
	  samp_vals[0] = MUS_FLOAT_TO_SAMPLE(fstr(str[2])); 
	  change_samples(istr(str[1]),1,samp_vals,cp,LOCK_MIXES,"set-" S_sample);
	  update_graph(cp,NULL); 
	} 
      isym(ss,0);
      return(0);
    }
  isym(ss,-1); 
  return(-1);
}

int snd_eval_str(snd_state *ss, char *buf, int count)
{
  int i,err = 0;
  char *tmp;
  char *tok[10];
  snd_info *sp;
  chan_info *cp;
  if (buf == NULL) return(0);
  if (ss->result_printout == PLAIN_MESSAGE) snd_append_command(ss,buf);
  cp = NULL;
  sp = any_selected_sound(ss);
  if (sp) cp = any_selected_channel(sp);
  for (i=0;i<10;i++) tok[i] = NULL;
  tmp = buf;
  if (tmp[0] == '(') tmp++;
  /* now a kludge to handle the special defvar case */
  if ((snd_strlen(tmp) > 7) && (tmp[0]=='d') && (tmp[1]=='e') && (tmp[2]=='f') && (tmp[3]=='v') && (tmp[4]=='a') && (tmp[5]=='r'))
    {
      pass_name_to_envelope_editor(ss,tmp);
      isym(ss,0);
    }
  else
    {
      tok[0] = strtok(tmp,noscm_white_space);
      for (i=1;i<10;i++) 
	{
	  tok[i] = strtok(NULL,noscm_white_space);
	  if (tok[i] == NULL) break;
	}
      if (tok[0] == NULL) return(-1); /* probably white space with a <cr> */
      err = symit(ss,tok);
    }
  return(err);
}

static int symit(snd_state *ss,char **str)
{
  char *tok,*filename = NULL;
  int ival,i;
  chan_info *cp;
  snd_info *sp;
  Float scls[1];
  MUS_SAMPLE_TYPE samp_vals[2];
  tok = str[0];
  switch (*tok)
    {
    case 'a':
      if (strcmp(tok,S_activate_listener) == 0)
	{
	  handle_listener(ss,LISTENER_LISTENING); 
	  ss->listening = LISTENER_LISTENING; 
	  isym(ss,0);
	  return(0);
	}
      if (strcmp(tok,S_abort) == 0) abort();
      if (strcmp(tok,S_add_mark) == 0) {cp = get_cp(ss,str[2],str[3]); if (cp) isym(ss,mark_id(add_mark(istr(str[1]),NULL,cp))); return(0);}
      if (strcmp(tok,S_amp) == 0) {sp = get_sp(ss,str[1]); if (sp) fsym(ss,sp->amp); else isym(ss,0); return(0);}
      if (strcmp(tok,S_ask_before_overwrite) == 0) {isym(ss,ask_before_overwrite(ss)); return(0);}
      if (strcmp(tok,S_audio_input_device) == 0) {isym(ss,audio_input_device(ss)); return(0);}
      if (strcmp(tok,S_audio_output_device) == 0) {isym(ss,audio_output_device(ss)); return(0);}
      if (strcmp(tok,S_audio_state_file) == 0) {ssym(ss,audio_state_file(ss)); return(0);}
      if (strcmp(tok,S_auto_resize) == 0) {isym(ss,auto_resize(ss)); return(0);}
      if (strcmp(tok,S_auto_update) == 0) {isym(ss,auto_update(ss)); return(0);}
      break;
    case 'c':
#if HAVE_OSS
      if (strcmp(tok,S_clear_audio_inputs) == 0) {mus_audio_clear_soundcard_inputs(); isym(ss,0); return(0);}
#endif
      if (strcmp(tok,S_channel_style) == 0) {isym(ss,channel_style(ss)); return(0);}
      if (strcmp(tok,S_channels) == 0) {sp = get_sp(ss,str[1]); if (sp) isym(ss,sp->nchans); else isym(ss,0); return(0);}
      if (strcmp(tok,S_chans) == 0) {sp = get_sp(ss,str[1]); if (sp) isym(ss,sp->nchans); else isym(ss,0); return(0);}
      if (strcmp(tok,S_close_sound) == 0) {sp = get_sp(ss,str[1]); if (sp) snd_close_file(sp,ss); isym(ss,0); return(0);}
      if (strcmp(tok,S_color_cutoff) == 0) {fsym(ss,color_cutoff(ss)); return(0);}
      if (strcmp(tok,S_color_dialog) == 0) {start_color_dialog(ss,0,0); isym(ss,0); return(0);}
      if (strcmp(tok,S_color_inverted) == 0) {isym(ss,color_inverted(ss)); return(0);}
      if (strcmp(tok,S_color_scale) == 0) {fsym(ss,color_scale(ss)); return(0);}
      if (strcmp(tok,S_colormap) == 0) {isym(ss,color_map(ss)); return(0);}
      if (strcmp(tok,S_comment) == 0) {sp = get_sp(ss,str[1]); if (sp) ssym(ss,mus_sound_comment(sp->fullname)); else isym(ss,0); return(0);}
      if (strcmp(tok,S_contrast) == 0) {sp = get_sp(ss,str[1]); if (sp) fsym(ss,sp->contrast); else isym(ss,0); return(0);}
      if (strcmp(tok,S_contrast_amp) == 0) {sp = get_sp(ss,str[1]); if (sp) fsym(ss,sp->contrast_amp); else isym(ss,0); return(0);}
      if (strcmp(tok,S_contrasting) == 0) {sp = get_sp(ss,str[1]); if (sp) isym(ss,sp->contrasting); else isym(ss,0); return(0);}
      if (strcmp(tok,S_convolve_with) == 0) 
	{
	  cp = get_cp(ss,str[3],str[4]); 
	  if (cp) 
	    {
	      filename = mus_file_full_name(sstr(str[1]));
	      if (mus_file_probe(filename))
		convolve_with(filename,fstr(str[2]),cp); 
	      if (filename) FREE(filename);
	    }
	  isym(ss,0); 
	  return(0);
	}
      if (strcmp(tok,S_convolve_selection_with) == 0) 
	{
	  filename = mus_file_full_name(sstr(str[1]));
	  if (mus_file_probe(filename))
	    convolve_with(filename,fstr(str[2]),NULL); 
	  if (filename) FREE(filename);
	  isym(ss,0); 
	  return(0);
	}
      if (strcmp(tok,S_count_matches) == 0) {isym(ss,snd_find_1(get_cp(ss,str[3],str[4]),copy_string(str[1]),istr(str[2]),TRUE)); return(0);}
      if (strcmp(tok,S_corruption_time) == 0) {isym(ss,(int)(corruption_time(ss))); return(0);}
      if (strcmp(tok,S_cursor) == 0) {cp = get_cp(ss,str[1],str[2]); if (cp) isym(ss,cp->cursor); else isym(ss,0); return(0);}
      if (strcmp(tok,S_cursor_follows_play) == 0) {sp = get_sp(ss,str[1]); if (sp) isym(ss,sp->cursor_follows_play); else isym(ss,0); return(0);}
      if (strcmp(tok,S_cut) == 0) {delete_selection(S_cut,UPDATE_DISPLAY); isym(ss,0); return(0);}
      break;
    case 'd':
      if (strcmp(tok,S_dac_size) == 0) {isym(ss,dac_size(ss)); return(0);}
      if (strcmp(tok,S_dac_folding) == 0) {isym(ss,dac_folding(ss)); return(0);}
      if (strcmp(tok,S_data_clipped) == 0) {isym(ss,data_clipped(ss)); return(0);}
      if (strcmp(tok,S_data_format) == 0) {sp = get_sp(ss,str[1]); if (sp) isym(ss,(sp->hdr)->format); else isym(ss,0); return(0);}
      if (strcmp(tok,S_data_location) == 0) {sp = get_sp(ss,str[1]); if (sp) isym(ss,(sp->hdr)->data_location); else isym(ss,0); return(0);}
      if (strcmp(tok,S_default_output_chans) == 0) {isym(ss,default_output_chans(ss)); return(0);}
      if (strcmp(tok,S_default_output_srate) == 0) {isym(ss,default_output_srate(ss)); return(0);}
      if (strcmp(tok,S_default_output_type) == 0) {isym(ss,default_output_type(ss)); return(0);}
      if (strcmp(tok,S_default_output_format) == 0) {isym(ss,default_output_format(ss)); return(0);}
      if (strcmp(tok,S_delete_region) == 0) 
	{
	  ival = istr(str[1]);
	  if (region_ok(ival))
	    delete_region_and_update_browser(ss,ival);
	  else display_results(ss,"no such region");
	  isym(ss,0); 
	  return(0);
	}
      if (strcmp(tok,S_delete_sample) == 0) 
	{
	  cp = get_cp(ss,str[2],str[3]); 
	  if (cp) 
	    {
	      ival = istr(str[1]);
	      if ((ival <= current_ed_samples(cp)) && (ival >= 0))
		{
		  delete_samples(istr(str[1]),1,cp,S_delete_sample); 
		  update_graph(cp,NULL);
		}
	      else display_results(ss,"no such sample");
	    } 
	  isym(ss,0);
	  return(0);
	}
      if (strcmp(tok,S_delete_samples) == 0) 
	{
	  cp = get_cp(ss,str[3],str[4]);
	  if (cp) 
	    {
	      delete_samples(istr(str[1]),istr(str[2]),cp,S_delete_samples); 
	      update_graph(cp,NULL);
	    } 
	  isym(ss,0);
	  return(0);
	}
      if (strcmp(tok,S_describe_audio) == 0) {snd_help(ss,"Audio State",mus_audio_report()); isym(ss,0); return(0);}
      if (strcmp(tok,S_dismiss_all_dialogs) == 0) {dismiss_all_dialogs(ss); isym(ss,0); return(0);}
      break;
    case 'e':
      if (strcmp(tok,S_edit_header_dialog) == 0) {edit_header(get_sp(ss,str[1])); isym(ss,0); return(0);}
      if (strcmp(tok,S_edit_position) == 0) {cp = get_cp(ss,str[1],str[2]); if (cp) isym(ss,cp->edit_ctr); return(0);}
      if (strcmp(tok,S_enved_base) == 0) {fsym(ss,enved_base(ss)); return(0);}
      if (strcmp(tok,S_enved_power) == 0) {fsym(ss,enved_power(ss)); return(0);}
      if (strcmp(tok,S_enved_clipping) == 0) {isym(ss,enved_clipping(ss)); return(0);}
      if (strcmp(tok,S_enved_dialog) == 0) {create_envelope_editor(ss); isym(ss,0); return(0);}
      if (strcmp(tok,S_enved_exping) == 0) {isym(ss,enved_exping(ss)); return(0);}
      if (strcmp(tok,S_enved_target) == 0) {isym(ss,enved_target(ss)); return(0);}
      if (strcmp(tok,S_enved_waving) == 0) {isym(ss,enved_waving(ss)); return(0);}
      if (strcmp(tok,S_enved_dBing) == 0) {isym(ss,enved_dBing(ss)); return(0);}
      if (strcmp(tok,S_eps_file) == 0) {ssym(ss,eps_file(ss)); return(0);}
      if (strcmp(tok,S_exit) == 0) {snd_exit_cleanly(ss); snd_exit(1);}
      if (strcmp(tok,S_expand) == 0) {sp = get_sp(ss,str[1]); if (sp) fsym(ss,sp->expand); else isym(ss,0); return(0);}
      if (strcmp(tok,S_expand_hop) == 0) {sp = get_sp(ss,str[1]); if (sp) fsym(ss,sp->expand_hop); else isym(ss,0); return(0);}
      if (strcmp(tok,S_expand_length) == 0) {sp = get_sp(ss,str[1]); if (sp) fsym(ss,sp->expand_length); else isym(ss,0); return(0);}
      if (strcmp(tok,S_expand_ramp) == 0) {sp = get_sp(ss,str[1]); if (sp) fsym(ss,sp->expand_ramp); else isym(ss,0); return(0);}
      if (strcmp(tok,S_expanding) == 0) {sp = get_sp(ss,str[1]); if (sp) isym(ss,sp->expanding); else isym(ss,0); return(0);}
      if (strcmp(tok,S_env_selection) == 0)
	{
	  cp = get_cp(ss,str[3],str[4]);
	  if (cp) apply_env(cp,scan_envelope(str[1]),0,0,1.0,TRUE,NOT_FROM_ENVED,S_env_selection);
	  isym(ss,0);
	  return(0);
	}
      if (strcmp(tok,S_env_sound) == 0) 
	{
	  int dur,samp;
	  cp = get_cp(ss,str[5],str[6]);
	  if (cp)
	    {
	      if ((str[2]) && (*(str[2]))) samp = istr(str[2]); else samp = 0;
	      if ((str[3]) && (*(str[3]))) dur = istr(str[3]); else dur = current_ed_samples(cp);
	      apply_env(cp,scan_envelope(str[1]),samp,dur,1.0,FALSE,NOT_FROM_ENVED,S_env_sound);
	    }
	  isym(ss,0);
	  return(0);
	}
      break;
    case 'f':
      if (strcmp(tok,S_fft_beta) == 0) {fsym(ss,fft_beta(ss)); return(0);}
      if (strcmp(tok,S_fft_log_frequency) == 0) {isym(ss,fft_log_frequency(ss)); return(0);}
      if (strcmp(tok,S_fft_log_magnitude) == 0) {isym(ss,fft_log_magnitude(ss)); return(0);}
      if (strcmp(tok,S_fft_size) == 0) {isym(ss,fft_size(ss)); return(0);}
      if (strcmp(tok,S_fft_style) == 0) {isym(ss,fft_style(ss)); return(0);}
      if (strcmp(tok,S_fft_window) == 0) {isym(ss,fft_window(ss)); return(0);}
      if (strcmp(tok,S_ffting) == 0) {cp = get_cp(ss,str[1],str[2]); if (cp) isym(ss,cp->ffting); else isym(ss,0); return(0);}
      if (strcmp(tok,S_file_dialog) == 0) {start_file_dialog(ss,0,0); isym(ss,0); return(0);}
      if (strcmp(tok,S_file_name) == 0) {sp = get_sp(ss,str[1]); if (sp) ssym(ss,sp->fullname); else isym(ss,0); return(0);}
      if (strcmp(tok,S_filter_sound) == 0) 
	{
	  cp = get_cp(ss,str[3],str[4]); 
	  if (cp) apply_filter(cp,istr(str[2]),scan_envelope(str[1]),NOT_FROM_ENVED,S_filter_sound,FALSE,NULL); 
	  isym(ss,0); 
	  return(0);
	}
      if (strcmp(tok,S_filter_dBing) == 0) {sp = get_sp(ss,str[1]); if (sp) isym(ss,sp->filter_dBing); else isym(ss,0); return(0);}
      if (strcmp(tok,S_filter_env) == 0) {sp = get_sp(ss,str[1]); if (sp) ssym(ss,env_to_string(sp->filter_env)); else isym(ss,0); return(0);}
      if (strcmp(tok,S_filter_env_order) == 0) {isym(ss,filter_env_order(ss)); return(0);}
      if (strcmp(tok,S_filter_order) == 0) {sp = get_sp(ss,str[1]); if (sp) isym(ss,sp->filter_order); else isym(ss,0); return(0);}
      if (strcmp(tok,S_filtering) == 0) {sp = get_sp(ss,str[1]); if (sp) isym(ss,sp->filtering); else isym(ss,0); return(0);}
      if (strcmp(tok,S_filter_selection) == 0) 
	{
	  cp = get_cp(ss,str[3],str[4]); 
	  if (cp) apply_filter(cp,istr(str[2]),scan_envelope(str[1]),NOT_FROM_ENVED,S_filter_selection,TRUE,NULL); 
	  isym(ss,0); 
	  return(0);
	}
      if (strcmp(tok,S_find) == 0) {isym(ss,snd_find_1(get_cp(ss,str[3],str[4]),copy_string(str[1]),istr(str[2]),FALSE)); return(0);}
      if (strcmp(tok,S_find_sound) == 0) 
	{
	  filename = sstr(str[1]);
	  for (i=0;i<ss->max_sounds;i++)
	    {
	      sp = ss->sounds[i];
	      if ((snd_ok(sp)) && ((strcmp(filename,sp->fullname) == 0) || (strcmp(filename,sp->shortname) == 0)))
		{
		  isym(ss,i);
		  return(0);
		}
	    }
	  isym(ss,-1); 
	  return(0);
	}
      if (strcmp(tok,S_fit_data_on_open) == 0) {isym(ss,fit_data_on_open(ss)); return(0);}
      if (strcmp(tok,S_frames) == 0) {cp = get_cp(ss,str[1],str[2]); if (cp) isym(ss,current_ed_samples(cp)); else isym(ss,0); return(0);}
      break;
    case 'g':
      if (strcmp(tok,S_graph_style) == 0) {isym(ss,graph_style(ss)); return(0);}
      if (strcmp(tok,S_graphs_horizontal) == 0) {isym(ss,graphs_horizontal(ss)); return(0);}
      if (strcmp(tok,S_graphing) == 0) {cp = get_cp(ss,str[1],str[2]); if (cp) isym(ss,cp->lisp_graphing); else isym(ss,0); return(0);}
      if (strcmp(tok,S_graph_ps) == 0) {snd_print(ss,eps_file(ss)); isym(ss,0); return(0);}
      break;
    case 'h':
      if (strcmp(tok,S_header_type) == 0) {sp = get_sp(ss,str[1]); if (sp) isym(ss,(sp->hdr)->type); else isym(ss,0); return(0);}
      if (strcmp(tok,S_help_dialog) == 0) {snd_help(ss,sstr(str[1]),sstr(str[2])); isym(ss,0); return(0);}
      if (strcmp(tok,S_hide_listener) == 0) {if (ss->listening == LISTENER_OPEN) handle_listener(ss,LISTENER_LISTENING); isym(ss,0); return(0);}
#if HAVE_HTML
      if (strcmp(tok,S_html_dir) == 0) {ssym(ss,html_dir(ss)); return(0);}
#endif
      break;
    case 'i':
      if (strcmp(tok,S_initial_x0) == 0) {fsym(ss,initial_x0(ss)); return(0);}
      if (strcmp(tok,S_initial_x1) == 0) {fsym(ss,initial_x1(ss)); return(0);}
      if (strcmp(tok,S_initial_y0) == 0) {fsym(ss,initial_y0(ss)); return(0);}
      if (strcmp(tok,S_initial_y1) == 0) {fsym(ss,initial_y1(ss)); return(0);}
      if (strcmp(tok,S_insert_sample) == 0) 
	{
	  cp = get_cp(ss,str[3],str[4]); 
	  if (cp) 
	    {
	      ival = istr(str[1]);
	      if (ival >= 0) 
		{
		  samp_vals[0] = MUS_FLOAT_TO_SAMPLE(fstr(str[2])); 
		  insert_samples(ival,1,samp_vals,cp,S_insert_sample); 
		  update_graph(cp,NULL);
		}
	      else display_results(ss,"no such sample"); /* should we insert and move begin back? */
	    } 
	  isym(ss,0);
	  return(0);
	}
      if (strcmp(tok,S_insert_sound) == 0) 
	{
	  cp = get_cp(ss,str[3],str[4]);
	  if (cp)
	    {
	      filename = mus_file_full_name(sstr(str[1]));
	      ival = mus_sound_chans(filename);
	      if (ival > 0)
		{
		  file_insert_samples(0,mus_sound_samples(filename)/ival,filename,cp,istr(str[2]),DONT_DELETE_ME,S_insert_sound);
		  update_graph(cp,NULL);
		}
	      else display_results(ss,"no such file");
	      if (filename) FREE(filename);
	    }
	  isym(ss,0);
	  return(0);
	}
      break;
    case 'j':
      break;
    case 'k':
      if (strcmp(tok,S_key) == 0) {keyboard_command(current_channel(ss),istr(str[1]),istr(str[2])); isym(ss,0); return(0);}
      break;
    case 'l':
      if (strcmp(tok,S_left_sample) == 0) 
	{cp = get_cp(ss,str[1],str[2]); if ((cp) && (cp->axis)) isym(ss,(cp->axis)->losamp); else isym(ss,0); return(0);}
      if (strcmp(tok,S_line_size) == 0) {isym(ss,line_size(ss)); return(0);}
      if (strcmp(tok,"load") == 0) {snd_load_file(filename = mus_file_full_name(sstr(str[1]))); if (filename) FREE(filename); return(0);}
      break;
    case 'm':
      if (strcmp(tok,S_max_fft_peaks) == 0) {isym(ss,max_fft_peaks(ss)); return(0);}
      if (strcmp(tok,S_max_regions) == 0) {isym(ss,max_regions(ss)); return(0);}
      if (strcmp(tok,S_max_sounds) == 0) {isym(ss,ss->max_sounds); return(0);}
      if (strcmp(tok,S_memo_sound) == 0) {isym(ss,0); return(0);}
      if (strcmp(tok,S_maxamp) == 0) {cp = get_cp(ss,str[1],str[2]); if (cp) fsym(ss,get_maxamp(cp->sound,cp)); else isym(ss,0); return(0);}
      if (strcmp(tok,S_min_dB) == 0) {fsym(ss,ss->min_dB); return(0);}
      if (strcmp(tok,S_movies) == 0) {isym(ss,movies(ss)); return(0);}
      break;
    case 'n':
      if (strcmp(tok,S_new_sound) == 0) 
	{
	  if (str[2] == NULL)
	    snd_new_file(ss,filename = mus_file_full_name(sstr(str[1])),MUS_UNSUPPORTED,MUS_UNSUPPORTED,0,0,NULL,WITH_DIALOG);
	  else snd_new_file(ss,filename = mus_file_full_name(sstr(str[1])),istr(str[2]),istr(str[3]),istr(str[4]),istr(str[5]),sstr(str[6]),WITHOUT_DIALOG);
	  if (filename) FREE(filename);
	  isym(ss,0); 
	  return(0);
	}
      if (strcmp(tok,S_normalize_fft) == 0) {isym(ss,normalize_fft(ss)); return(0);}
      if (strcmp(tok,S_normalize_on_open) == 0) {isym(ss,normalize_on_open(ss)); return(0);}
      if (strcmp(tok,S_normalize_view) == 0) {normalize_all_sounds(ss); isym(ss,0); return(0);}
      break;
    case 'o': 
      if (strcmp(tok,S_open_sound) == 0) {sp = snd_open_file(sstr(str[1]),ss); if (sp) isym(ss,sp->index); else isym(ss,0); return(0);}
      if (strcmp(tok,S_open_alternate_sound) == 0) 
	{
	  sp = any_selected_sound(ss);
	  if (sp) snd_close_file(sp,ss);
	  sp = snd_open_file(sstr(str[1]),ss);
	  if (sp) isym(ss,sp->index); else isym(ss,0);
	  return(0);
	}
      if (strcmp(tok,S_orientation_dialog) == 0) {start_orientation_dialog(ss,0,0); isym(ss,0); return(0);}
      break;
    case 'p':
      if (strcmp(tok,S_play_region) == 0) 
	{
	  ival = istr(str[1]);
	  if (region_ok(ival))
	    play_region(ss,istr(str[1]),IN_BACKGROUND); 
	  else display_results(ss,"no such region");
	  isym(ss,0); 
	  return(0);
	}
      if (strcmp(tok,S_preload_directory) == 0) {add_directory_to_prevlist(ss,sstr(str[1])); isym(ss,0); return(0);}
      if (strcmp(tok,S_previous_files_sort) == 0) {isym(ss,previous_files_sort(ss)); return(0);}
      if (strcmp(tok,S_print_length) == 0) {isym(ss,print_length(ss)); return(0);}
      if (strcmp(tok,S_protect_region) == 0) 
	{if (str[2]) ival = istr(str[2]); else ival = 1; set_region_protect(istr(str[1]),ival); isym(ss,0); return(0);}
      break;
    case 'r':
      if (strcmp(tok,S_raw_chans) == 0) {isym(ss,raw_chans(ss)); return(0);}
      if (strcmp(tok,S_raw_format) == 0) {isym(ss,raw_format(ss)); return(0);}
      if (strcmp(tok,S_raw_srate) == 0) {isym(ss,raw_srate(ss)); return(0);}
      if (strcmp(tok,S_read_only) == 0) {sp = get_sp(ss,str[1]); if (sp) isym(ss,sp->read_only); else isym(ss,0); return(0);}
      if (strcmp(tok,S_region_chans) == 0) 
	{ival = istr(str[1]); if (region_ok(ival)) isym(ss,region_chans(ival)); else display_results(ss,"no such region"); return(0);}
      if (strcmp(tok,S_region_length) == 0) 
	{ival = istr(str[1]); if (region_ok(ival)) isym(ss,region_len(ival)); else display_results(ss,"no such region"); return(0);}
      if (strcmp(tok,S_region_maxamp) == 0) 
	{ival = istr(str[1]); if (region_ok(ival)) fsym(ss,region_maxamp(ival)); else display_results(ss,"no such region"); return(0);}
      if (strcmp(tok,S_region_srate) == 0) 
	{ival = istr(str[1]); if (region_ok(ival)) isym(ss,region_srate(ival)); else display_results(ss,"no such region"); return(0);}
      if (strcmp(tok,S_regions) == 0) {isym(ss,snd_regions()); return(0);}
      if (strcmp(tok,S_report_in_minibuffer) == 0) {sp = get_sp(ss,str[2]); if (sp) report_in_minibuffer(sp,str[1]); isym(ss,0); return(0);}
      if (strcmp(tok,S_restore_control_panel) == 0) {sp = get_sp(ss,str[1]); if (sp) restore_control_panel(sp); isym(ss,0); return(0);}
      if (strcmp(tok,S_reverb_decay) == 0) {fsym(ss,reverb_decay(ss)); return(0);}
      if (strcmp(tok,S_reverb_feedback) == 0) {sp = get_sp(ss,str[1]); if (sp) fsym(ss,sp->revfb); else isym(ss,0); return(0);}
      if (strcmp(tok,S_reverb_length) == 0) {sp = get_sp(ss,str[1]); if (sp) fsym(ss,sp->revlen); else isym(ss,0); return(0);}
      if (strcmp(tok,S_reverb_lowpass) == 0) {sp = get_sp(ss,str[1]); if (sp) fsym(ss,sp->revlp); else isym(ss,0); return(0);}
      if (strcmp(tok,S_reverb_scale) == 0) {sp = get_sp(ss,str[1]); if (sp) fsym(ss,sp->revscl); else isym(ss,0); return(0);}
      if (strcmp(tok,S_reverbing) == 0) {sp = get_sp(ss,str[1]); if (sp) isym(ss,sp->reverbing); else isym(ss,0); return(0);}
      if (strcmp(tok,S_right_sample) == 0) 
	{cp = get_cp(ss,str[1],str[2]); if ((cp) && (cp->axis)) isym(ss,(cp->axis)->hisamp); else isym(ss,0); return(0);}
      if (strcmp(tok,S_redo) == 0) 
	{
	  ival = istr(str[1]); 
	  if (ival == 0) ival = 1; 
	  cp = get_cp(ss,str[2],str[3]); 
	  if (cp) 
	    {
	      redo_edit_with_sync(cp,ival); 
	      update_graph(cp,NULL);
	    }
	  isym(ss,0);
	  return(0);
	}
      if (strcmp(tok,S_revert_sound) == 0) 
	{
	  sp = get_sp(ss,str[1]);
	  if (sp)
	    {
	      for (i=0;i<sp->nchans;i++) 
		{
		  revert_edits(sp->chans[i],NULL); 
		  update_graph(sp->chans[i],NULL);
		}
	      reflect_file_revert_in_label(sp);
	      reflect_file_revert_in_menu(ss);
	    }
	  isym(ss,0);
	  return(0);
	}
      break;
    case 's':
      if ((tok[1] == 'e') && (tok[2] == 't') && (tok[3] == '-'))
	{
	  return(handle_set(ss,tok,str));
	}
      if (strcmp(tok,S_sample) == 0) {cp = get_cp(ss,str[2],str[3]); if (cp) fsym(ss,sample(istr(str[1]),cp)); return(0);}
      if (strcmp(tok,S_save_control_panel) == 0) {sp = get_sp(ss,str[1]); if (sp) save_control_panel(sp); isym(ss,0); return(0);}
      if (strcmp(tok,S_save_dir) == 0) {ssym(ss,save_dir(ss)); return(0);}
      if (strcmp(tok,S_save_envelopes) == 0) {g_save_envelopes(str[1]); isym(ss,0); return(0);}
      if (strcmp(tok,S_save_region) == 0) 
	{
	  ival = istr(str[1]);
	  if (region_ok(ival))
	    save_region(ss,ival,filename = mus_file_full_name(sstr(str[2])),istr(str[3])); 
	  else display_results(ss,"no such region");
	  if (filename) FREE(filename);
	  isym(ss,0); 
	  return(0);
	}
      if (strcmp(tok,S_save_selection) == 0)
	{
	  save_selection(ss,filename = mus_file_full_name(sstr(str[1])),istr(str[2]),istr(str[3]),istr(str[4]),sstr(str[5]));
	  if (filename) FREE(filename);
	  isym(ss,0); return(0);
	}
      if (strcmp(tok,S_scale_selection_to) == 0) 
	{
	  if (selection_is_active())
	    {
	      scls[0] = fstr(str[1]);
	      scale_to(ss,NULL,NULL,scls,1,TRUE);
	      fsym(ss,scls[0]);
	    }
	  else 
	    {
	      display_results(ss,"no active selection");
	      isym(ss,-1);
	    }
	  return(0);
	}
      if (strcmp(tok,S_scale_selection_by) == 0) 
	{
	  if (selection_is_active())
	    {
	      scls[0] = fstr(str[1]);
	      scale_by(NULL,scls,1,TRUE);
	      fsym(ss,scls[0]);
	    }
	  else 
	    {
	      display_results(ss,"no active selection");
	      isym(ss,-1);
	    }
	  return(0);
	}
      if (strcmp(tok,S_scale_to) == 0) 
	{
	  cp = get_cp(ss,str[2],str[3]);
	  if (cp)
	    {
	      scls[0] = fstr(str[1]);
	      scale_to(ss,cp->sound,cp,scls,1,FALSE); 
	    }
	  fsym(ss,scls[0]);
	  return(0);
	}
      if (strcmp(tok,S_scale_by) == 0) 
	{
	  cp = get_cp(ss,str[2],str[3]);
	  if (cp)
	    {
	      scls[0] = fstr(str[1]);
	      scale_by(cp,scls,1,FALSE); 
	    }
	  fsym(ss,scls[0]);
	  return(0);
	}
      if (strcmp(tok,S_select_channel) == 0) 
	{
	  sp = any_selected_sound(ss);
	  ival = istr(str[1]);
	  if ((sp) && (ival >= 0) && (ival < sp->nchans))
	    select_channel(sp,ival); 
	  else display_results(ss,"no such channel");
	  isym(ss,0); 
	  return(0);
	}
      if (strcmp(tok,S_select_region) == 0) 
	{
	  ival = istr(str[1]);
	  if (region_ok(ival))
	    select_region_and_update_browser(ss,ival); 
	  else display_results(ss,"no such region");
	  isym(ss,0); 
	  return(0);
	}
      if (strcmp(tok,S_select_sound) == 0) 
	{
	  ival = istr(str[1]); 
	  if ((ival >= 0) && (ival < ss->max_sounds) && (snd_ok(ss->sounds[ival])))
	    select_channel(ss->sounds[ival],0);
	  else display_results(ss,"no such sound");
	  isym(ss,0); 
	  return(0);
	}
      if (strcmp(tok,S_selected_channel) == 0) {sp = get_sp(ss,str[1]); if (sp) isym(ss,sp->selected_channel); else isym(ss,0); return(0);}
      if (strcmp(tok,S_selected_mix) == 0) {isym(ss,ss->selected_mix); return(0);}
      if (strcmp(tok,S_selection_length) == 0) {if (selection_is_active()) isym(ss,selection_len()); else isym(ss,0); return(0);}
      if (strcmp(tok,S_selection_position) == 0) {if (selection_is_active()) isym(ss,selection_beg(NULL)); else isym(ss,0); return(0);}
      if (strcmp(tok,S_short_file_name) == 0) {sp = get_sp(ss,str[1]); if (sp) ssym(ss,sp->shortname); else isym(ss,0); return(0);}
      if (strcmp(tok,S_show_axes) == 0) {isym(ss,show_axes(ss)); return(0);}
      if (strcmp(tok,S_show_fft_peaks) == 0) {isym(ss,show_fft_peaks(ss)); return(0);}
      if (strcmp(tok,S_show_listener) == 0) {if (ss->listening != LISTENER_OPEN) handle_listener(ss,LISTENER_OPEN); isym(ss,0); return(0);}
      if (strcmp(tok,S_show_marks) == 0) {isym(ss,show_marks(ss)); return(0);}
      if (strcmp(tok,S_show_mix_waveforms) == 0) {isym(ss,show_mix_waveforms(ss)); return(0);}
      if (strcmp(tok,S_show_selection_transform) == 0) {isym(ss,show_selection_transform(ss)); return(0);}
      if (strcmp(tok,S_show_usage_stats) == 0) {isym(ss,show_usage_stats(ss)); return(0);}
      if (strcmp(tok,S_show_y_zero) == 0) {isym(ss,show_y_zero(ss)); return(0);}
      if (strcmp(tok,S_show_controls) == 0) {sp = get_sp(ss,str[1]); if (sp) isym(ss,control_panel_open(sp)); else isym(ss,0); return(0);}
      if (strcmp(tok,S_sinc_width) == 0) {isym(ss,sinc_width(ss)); return(0);}
      if (strcmp(tok,S_snd_print) == 0) 
	{
	  ss->result_printout = MESSAGE_WITHOUT_CARET;
	  snd_append_command(ss,sstr(str[1]));
	  return(0);
	}
      if (strcmp(tok,S_snd_version) == 0) {ssym(ss,SND_VERSION); return(0);}
      if (strcmp(tok,S_spectro_cutoff) == 0) {fsym(ss,spectro_cutoff(ss)); return(0);}
      if (strcmp(tok,S_spectro_start) == 0) {fsym(ss,spectro_cutoff(ss)); return(0);}
      if (strcmp(tok,S_spectro_hop) == 0) {isym(ss,spectro_hop(ss));  return(0);}
      if (strcmp(tok,S_spectro_x_angle) == 0) {fsym(ss,spectro_x_angle(ss)); return(0);}
      if (strcmp(tok,S_spectro_x_scale) == 0) {fsym(ss,spectro_x_scale(ss)); return(0);}
      if (strcmp(tok,S_spectro_y_angle) == 0) {fsym(ss,spectro_y_angle(ss)); return(0);}
      if (strcmp(tok,S_spectro_y_scale) == 0) {fsym(ss,spectro_y_scale(ss)); return(0);}
      if (strcmp(tok,S_spectro_z_angle) == 0) {fsym(ss,spectro_z_angle(ss)); return(0);}
      if (strcmp(tok,S_spectro_z_scale) == 0) {fsym(ss,spectro_z_scale(ss)); return(0);}
      if (strcmp(tok,S_speed) == 0) {sp = get_sp(ss,str[1]); if (sp) fsym(ss,sp->srate); else isym(ss,0); return(0);}
      if (strcmp(tok,S_speed_style) == 0) {isym(ss,speed_style(ss)); return(0);}
      if (strcmp(tok,S_speed_tones) == 0) {isym(ss,speed_tones(ss)); return(0);}
      if (strcmp(tok,S_squelch_update) == 0) {cp = get_cp(ss,str[1],str[2]); if (cp) isym(ss,cp->squelch_update); else isym(ss,0); return(0);}
      if (strcmp(tok,S_srate) == 0) {sp = get_sp(ss,str[1]); if (sp) isym(ss,(sp->hdr)->srate); else isym(ss,0); return(0);}
      if (strcmp(tok,S_src_sound) == 0) 
	{
	  cp = get_cp(ss,str[3],str[4]); 
	  src_env_or_num(ss,cp,NULL,fstr(str[1]),TRUE,NOT_FROM_ENVED,S_src_sound,FALSE); 
	  isym(ss,0); 
	  return(0);
	} /* no env case */
      if (strcmp(tok,S_src_selection) == 0) 
	{
	  src_env_or_num(ss,NULL,NULL,fstr(str[1]),TRUE,NOT_FROM_ENVED,S_src_selection,TRUE); 
	  isym(ss,0); 
	  return(0);
	} /* no env case */
      if (strcmp(tok,S_sync) == 0) {sp = get_sp(ss,str[1]); if (sp) isym(ss,sp->syncing); return(0);}
      if (strcmp(tok,S_selected_sound) == 0) {isym(ss,ss->selected_sound); return(0);}
      if (strcmp(tok,S_save_sound) == 0) {sp = get_sp(ss,str[1]); if (sp) save_edits(sp,NULL); isym(ss,0); return(0);}
      if (strcmp(tok,S_save_sound_as) == 0) 
	{
	  sp = get_sp(ss,str[2]);
	  if (str[3] == NULL)
	    save_edits_2(sp,filename = mus_file_full_name(sstr(str[1])),(sp->hdr)->type,(sp->hdr)->format,(sp->hdr)->srate,NULL);
	  else save_edits_2(sp,filename = mus_file_full_name(sstr(str[1])),istr(str[3]),istr(str[4]),istr(str[5]),NULL);
	  if (filename) FREE(filename);
	  isym(ss,0);
	  return(0);
	}
      break;
    case 't':
      if (strcmp(tok,S_temp_dir) == 0) {ssym(ss,temp_dir(ss)); return(0);}
      if (strcmp(tok,S_transform_dialog) == 0) {fire_up_transform_dialog(ss); isym(ss,0); return(0);}
      if (strcmp(tok,S_transform_type) == 0) {isym(ss,transform_type(ss)); return(0);}
      if (strcmp(tok,S_trap_segfault) == 0) {isym(ss,trap_segfault(ss)); return(0);}
      break;
    case 'u':
      if (strcmp(tok,S_uniting) == 0) {sp = get_sp(ss,str[1]); if (sp) isym(ss,sp->combining); return(0);}
      if (strcmp(tok,S_update_fft) == 0) {cp = get_cp(ss,str[1],str[2]); calculate_fft(cp,NULL); isym(ss,0); return(0);}
      if (strcmp(tok,S_update_graph) == 0) {cp = get_cp(ss,str[1],str[2]); update_graph(cp,NULL); isym(ss,0); return(0);}
      if (strcmp(tok,S_update_sound) == 0) {sp = get_sp(ss,str[1]); if (sp) snd_update(ss,sp); isym(ss,0); return(0);}
      if (strcmp(tok,S_use_raw_defaults) == 0) {isym(ss,use_raw_defaults(ss)); return(0);}
      if (strcmp(tok,S_use_sinc_interp) == 0) {isym(ss,use_sinc_interp(ss)); return(0);}
      if (strcmp(tok,S_undo) == 0) 
	{
	  ival = istr(str[1]); 
	  if (ival == 0) ival = 1; 
	  cp = get_cp(ss,str[2],str[3]); 
	  if (cp) 
	    {
	      undo_edit_with_sync(cp,ival); 
	      update_graph(cp,NULL);
	    }
	  isym(ss,0);
	  return(0);
	}
      break;
    case 'v':
      if (strcmp(tok,S_verbose_cursor) == 0) {isym(ss,verbose_cursor(ss)); return(0);}
      if (strcmp(tok,S_vu_font) == 0) {ssym(ss,vu_font(ss)); return(0);}
      if (strcmp(tok,S_vu_font_size) == 0) {fsym(ss,vu_font_size(ss)); return(0);}
      if (strcmp(tok,S_vu_size) == 0) {fsym(ss,vu_size(ss)); return(0);}
      if (strcmp(tok,S_view_sound) == 0) 
	{
	  ss->viewing = 1;
	  sp = snd_open_file(sstr(str[1]),ss);
	  ss->viewing = 0;
	  if (sp) isym(ss,sp->index); else isym(ss,0);
	  return(0);
	}
      break;
    case 'w':
      if (strcmp(tok,S_wavelet_type) == 0) {isym(ss,wavelet_type(ss)); return(0);}
      if (strcmp(tok,S_waving) == 0) {cp = get_cp(ss,str[1],str[2]); if (cp) isym(ss,cp->waving); else isym(ss,0); return(0);}
      if (strcmp(tok,S_wavo) == 0) {isym(ss,wavo(ss)); return(0);}
      if (strcmp(tok,S_wavo_hop) == 0) {isym(ss,wavo_hop(ss)); return(0);}
      if (strcmp(tok,S_wavo_trace) == 0) {isym(ss,wavo_trace(ss)); return(0);}
      if (strcmp(tok,S_window_height) == 0) {isym(ss,widget_height(MAIN_SHELL(ss))); return(0);}
      if (strcmp(tok,S_window_width) == 0) {isym(ss,widget_width(MAIN_SHELL(ss))); return(0);}
      if (strcmp(tok,S_window_x) == 0) {isym(ss,widget_x(MAIN_SHELL(ss))); return(0);}
      if (strcmp(tok,S_window_y) == 0) {isym(ss,widget_y(MAIN_SHELL(ss))); return(0);}
      if (strcmp(tok,S_with_mix_tags) == 0) {isym(ss,with_mix_tags(ss)); return(0);}
      break;
    case 'x':
      if (strcmp(tok,S_x_axis_style) == 0) {isym(ss,x_axis_style(ss)); return(0);}
      if (strcmp(tok,S_x_bounds) == 0) {cp = get_cp(ss,str[3],str[4]); if (cp) fsym(ss,(cp->axis)->x0); else isym(ss,0); return(0);}
      break;
    case 'y':
      if (strcmp(tok,S_y_bounds) == 0) {cp = get_cp(ss,str[3],str[4]); if (cp) fsym(ss,(cp->axis)->y0); else isym(ss,0); return(0);}
      break;
    case 'z':
      if (strcmp(tok,S_zero_pad) == 0) {isym(ss,zero_pad(ss)); return(0);}
      if (strcmp(tok,S_zoom_focus_style) == 0) {isym(ss,zoom_focus_style(ss)); return(0);}
      break;
    default: break;
    }
  isym(ss,-1);
  return(-1);
}

static char init_file_buffer[128];

void snd_load_init_file(snd_state *ss, int nog, int noi)
{
  /* look for ".snd" on the home directory, and load it using the lisp-like CLM syntax given above */
  int fd;
  char *str;
  no_guile_errors = 1;
#ifdef SND_CONF
  if (nog == 0)
    {
      fd = open(SND_CONF,O_RDONLY,0);
      if (fd != -1)
	{
	  while (eval_forever(ss,fd));
	  close(fd);
	}
    }
#endif
  if ((ss->init_file) && (noi == 0))
    {
      str = ss->init_file;
      if ((*str) == '~')
	{
	  strcpy(init_file_buffer,getenv("HOME"));
	  strcat(init_file_buffer,++str);
	  fd = open(init_file_buffer,O_RDONLY,0);
	}
      else fd = open(ss->init_file,O_RDONLY,0);
      if (fd == -1) return;
      while (eval_forever(ss,fd));
      close(fd);
    }
  no_guile_errors = 0;
}

void snd_load_file(char *filename)
{
  int fd;
  char *str,*saved_buf;
  snd_state *ss;
  str=filename;
  ss = get_global_state();
  saved_buf = NULL;
  if ((*str) == '~')
    {
      strcpy(init_file_buffer,getenv("HOME"));
      strcat(init_file_buffer,++str);
      fd = open(init_file_buffer,O_RDONLY,0);
    }
  else fd = open(str,O_RDONLY,0);
  if (fd == -1) return;
  while (eval_forever(ss,fd));
  close(fd);
}

int dont_exit(snd_state *ss) {return(0);}
int dont_start(snd_state *ss, char *filename) {return(0);}
void during_open(int fd, char *file, int reason) {}
void after_open(int index) {}
void clear_listener(void) {}

#endif
