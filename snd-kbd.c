#include "snd.h"

static int defining_macro = 0;

/* -------- Keyboard Macros -------- */
/* optimized for the most common case (pure keyboard commands) */

static int macro_cmd_size = 0;
static int macro_size = 0;
typedef struct {int keysym; int state;} macro_cmd;
static macro_cmd **macro_cmds = NULL;
typedef struct {char *name; int macro_size; macro_cmd **cmds;} named_macro;
static named_macro **named_macros = NULL;
static int named_macro_ctr = 0;
static int named_macro_size = 0;

static void allocate_macro_cmds(void)
{
  int i, old_size;
  old_size = macro_cmd_size;
  macro_cmd_size += 16;
  if (!macro_cmds)
    macro_cmds = (macro_cmd **)CALLOC(macro_cmd_size, sizeof(macro_cmd *));
  else 
    {
      macro_cmds = (macro_cmd **)REALLOC(macro_cmds, macro_cmd_size * sizeof(macro_cmd *));
      for (i = old_size; i < macro_cmd_size; i++) macro_cmds[i] = NULL;
    }
}

static void start_defining_macro (void)
{
  macro_size = 0;
  defining_macro = 1;
  if ((!macro_cmds) || (macro_size == macro_cmd_size)) allocate_macro_cmds();
}

static void stop_defining_macro (void)
{
  /* the last C-x ) went into the macro before we noticed it should not have */
  macro_size -= 2;
  defining_macro = 0;
}

static void execute_last_macro (chan_info *cp, int count)
{
  int i, j;
  if (macro_cmds)
    for (j = 0; j < count; j++)
      for (i = 0; i < macro_size; i++) 
	keyboard_command(cp, 
			 macro_cmds[i]->keysym, 
			 macro_cmds[i]->state);
}

static void continue_macro (int keysym, int state)
{
  if (!(macro_cmds[macro_size])) macro_cmds[macro_size] = (macro_cmd *)CALLOC(1, sizeof(macro_cmd));
  macro_cmds[macro_size]->keysym = keysym;
  macro_cmds[macro_size]->state = state;
  macro_size++;
  if (macro_size == macro_cmd_size) 
    allocate_macro_cmds();
}

static named_macro *name_macro(char *name)
{
  named_macro *nm;
  int i, old_size;
  if (named_macro_ctr == named_macro_size)
    {
      old_size = named_macro_size;
      named_macro_size += 16;
      if (!named_macros) named_macros = (named_macro **)CALLOC(named_macro_size, sizeof(named_macro *));
      else 
	{
	  named_macros = (named_macro **)REALLOC(named_macros, named_macro_size * sizeof(named_macro *));
	  for (i = old_size; i < named_macro_size; i++) named_macros[i] = NULL;
	}
    }
  if (!(named_macros[named_macro_ctr])) 
    named_macros[named_macro_ctr] = (named_macro *)CALLOC(1, sizeof(named_macro));
  nm = named_macros[named_macro_ctr];
  nm->name = copy_string(name);
  named_macro_ctr++;
  return(nm);
}

static void name_last_macro (char *name)
{
  named_macro *nm;
  macro_cmd *mc;
  int i;
  if (macro_size == 0)
    {
      snd_error("no macro active?");
      return;
    }
  nm = name_macro(name);
  nm->macro_size = macro_size;
  nm->cmds = (macro_cmd **)CALLOC(macro_size, sizeof(macro_cmd *));
  for (i = 0; i < macro_size; i++)
    {
      nm->cmds[i] = (macro_cmd *)CALLOC(1, sizeof(macro_cmd));
      mc = nm->cmds[i];
      mc->keysym = macro_cmds[i]->keysym;
      mc->state = macro_cmds[i]->state;
    }
}

static void save_macro_1(named_macro *nm, FILE *fd)
{
  int i;
  macro_cmd *mc;
  fprintf(fd, "(define (%s)\n", nm->name);
  for (i = 0; i < nm->macro_size; i++)
    {
      mc = nm->cmds[i];
      if (mc->keysym != 0)
	fprintf(fd, 
		"  (%s (char->integer #\\%c) %d)\n", 
		S_key, (char)(mc->keysym), mc->state);
    }
  fprintf(fd, ")\n");
}

static int execute_named_macro_1(chan_info *cp, char *name, int count)
{
  int i, j, k;
  named_macro *nm;
  macro_cmd *mc;
  for (k = 0; k < named_macro_ctr; k++)
    {
      if ((named_macros[k]->name) && 
	  (strcmp(name, named_macros[k]->name) == 0))
	{
	  nm = named_macros[k];
	  for (j = 0; j < count; j++)
	    for (i = 0; i < nm->macro_size; i++) 
	      {
		mc = nm->cmds[i];
		if (mc->keysym != 0)
		  keyboard_command(cp, mc->keysym, mc->state);
	      }
	  return(1);
	}
    }
  return(0);
}

static void execute_named_macro(chan_info *cp, char *name, int count)
{
  int one_edit;
  if (!(execute_named_macro_1(cp, name, count)))
    /* not a macro...*/
    {
      one_edit = cp->edit_ctr + 1;
      snd_eval_str(cp->state, name, count);
      if (cp->edit_ctr > one_edit)
	{
	  while (cp->edit_ctr > one_edit) backup_edit_list(cp);
	  if (cp->mixes) backup_mix_list(cp, one_edit);
	}
    }
}

typedef struct {int key; int state; int ignore_prefix; SCM func;} key_entry;
static key_entry *user_keymap = NULL;
static int keymap_size = 0;
static int keymap_top = 0;

static int in_user_keymap(int key, int state)
{
  int i;
  if (keymap_top == 0) return(-1);
  for (i = 0; i < keymap_top; i++)
    if ((user_keymap[i].key == key) && 
	(user_keymap[i].state == state) && 
	(user_keymap[i].func != SCM_UNDEFINED))
      return(i);
  return(-1);
}

void save_user_key_bindings(FILE *fd)
{
  int i;
  char binder[64];
  SCM con;
  for (i = 0; i < keymap_top; i++)
    if (user_keymap[i].func != SCM_UNDEFINED)
      {
	sprintf(binder,
		"(bind-key (char->integer #\\%c) %d ",
		(unsigned char)(user_keymap[i].key),
		user_keymap[i].state);
	con = TO_SCM_STRING(binder);
	fprintf(fd, ";    %s\n",
		g_print_1(user_keymap[i].func,
			  __FUNCTION__));
      }
}

static SCM g_key_binding(SCM key, SCM state)
{
  #define H_key_binding "(" S_key_binding " key state) -> function bound to this key"
  int i;
  ASSERT_TYPE(INTEGER_P(key), key, SCM_ARG1, S_key_binding, "an integer");
  ASSERT_TYPE(INTEGER_P(state), state, SCM_ARG2, S_key_binding, "an integer");
  i = in_user_keymap(TO_SMALL_C_INT(key),
		     TO_SMALL_C_INT(state));
  if (i >= 0) 
    return(user_keymap[i].func);
  return(SCM_UNDEFINED);
}

static void set_keymap_entry(int key, int state, int ignore, SCM func)
{
  int i;
  i = in_user_keymap(key, state);
  if (i == -1)
    {
      if (keymap_size == keymap_top)
	{
	  keymap_size += 16;
	  if (keymap_top == 0)
	    {
	      user_keymap = (key_entry *)CALLOC(keymap_size, sizeof(key_entry));
	      for (i = 0; i < keymap_size; i++) user_keymap[i].func = SCM_UNDEFINED;
	    }
	  else 
	    {
	      user_keymap = (key_entry *)REALLOC(user_keymap, keymap_size * sizeof(key_entry));
	      for (i = keymap_top; i < keymap_size; i++) 
		{
		  user_keymap[i].key = 0; 
		  user_keymap[i].state = 0; 
		  user_keymap[i].func = SCM_UNDEFINED;
		}
	    }
	}
      user_keymap[keymap_top].key = key;
      user_keymap[keymap_top].state = state;
      i = keymap_top;
      keymap_top++;
    }
  else
    {
      if ((user_keymap[i].func) && 
	  (PROCEDURE_P(user_keymap[i].func))) 
	snd_unprotect(user_keymap[i].func);
    }
  /* already checked arity etc */
  user_keymap[i].func = func;
  if (PROCEDURE_P(func)) snd_protect(func);
  user_keymap[i].ignore_prefix = ignore;
}

static int call_user_keymap(int hashedsym, int count)
{
  int i, res = KEYBOARD_NO_ACTION;
  SCM funcres;
  /* if guile call the associated scheme code, else see if basic string parser can handle it */
  if (user_keymap[hashedsym].ignore_prefix) count = 1;
  if (user_keymap[hashedsym].func != SCM_UNDEFINED)
    for (i = 0; i < count; i++) 
      {
	funcres = CALL0(user_keymap[hashedsym].func, "user key func");
	if (SYMBOL_P(funcres)) break; /* error tag returned? */
	res = TO_C_INT_OR_ELSE(funcres, KEYBOARD_NO_ACTION);
      }
  /* in emacs, apparently, prefix-arg refers to the next command, and current-prefix-arg is this command */
  return(res);
}

void save_macro_state (FILE *fd)
{
  int i;
  for (i = 0; i < named_macro_ctr; i++) 
    save_macro_1(named_macros[i], fd);
}


void report_in_minibuffer(snd_info *sp, char *format, ...)
{
  char *buf;
  int len;
#if HAVE_VPRINTF
  va_list ap;
  if (sp->active == 0) return;
  len = snd_strlen(format) + 256;
  buf = (char *)CALLOC(len, sizeof(char));
  va_start(ap, format);
#if HAVE_VSNPRINTF
  vsnprintf(buf, len, format, ap);
#else
  vsprintf(buf, format, ap);
#endif
  va_end(ap);
#else
  len = snd_strlen(format) + 256;
  buf = (char *)CALLOC(len, sizeof(char));
#if HAVE_SNPRINTF
  snprintf(buf, len, "%s...[you need vprintf]", format);
#else
  sprintf(buf, "%s...[you need vprintf]", format);
#endif
#endif
  set_minibuffer_string(sp, buf);
  sp->minibuffer_temp = 1;
  FREE(buf);
  /* leave sp->minibuffer off so that keyboard_command doesn't clear it */
}

void report_in_minibuffer_and_save(snd_info *sp, char *format, ...)
{
  /* kinda dumb to repeat this code -- is there a way to apply a C function to ... args? */
  char *buf;
  int len;
#if HAVE_VPRINTF
  va_list ap;
  if (sp->active == 0) return;
  len = snd_strlen(format) + 256;
  buf = (char *)CALLOC(len, sizeof(char));
  va_start(ap, format);
#if HAVE_VSNPRINTF
  vsnprintf(buf, len, format, ap);
#else
  vsprintf(buf, format, ap);
#endif
  va_end(ap);
#else
  len = snd_strlen(format) + 256;
  buf = (char *)CALLOC(len, sizeof(char));
#if HAVE_SNPRINTF
  snprintf(buf, len, "%s...[you need vprintf]", format);
#else
  sprintf(buf, "%s...[you need vprintf]", format);
#endif
#endif
  set_minibuffer_string(sp, buf);
  sp->minibuffer_temp = 1;
  add_to_error_history(sp->state, buf, FALSE);
  FREE(buf);
  /* leave sp->minibuffer off so that keyboard_command doesn't clear it */
}

enum {NOT_FILING, INPUT_FILING, REGION_FILING, CHANNEL_FILING, TEMP_FILING, CHANGE_FILING, INSERT_FILING, MACRO_FILING};

void clear_minibuffer(snd_info *sp)
{
  clear_minibuffer_prompt(sp);
  set_minibuffer_string(sp, NULL);
  sp->searching = 0;
  sp->evaling = 0;
  sp->marking = 0;
  sp->filing = NOT_FILING;
  sp->printing = 0;
  sp->minibuffer_on = 0;
  sp->loading = 0;
  sp->amping = 0;
  sp->macroing = 0;
  sp->prompting = 0;
}

void clear_minibuffer_prompt(snd_info *sp)
{
  make_minibuffer_label(sp, "     ");
  sp->minibuffer_temp = 0;
}

static int prompt(snd_info *sp, char *msg, char *preload)
{
  if (preload)
    {
      set_minibuffer_string(sp, preload);
      set_minibuffer_cursor_position(sp, snd_strlen(preload));
    }
  else
    set_minibuffer_string(sp, NULL);
  make_minibuffer_label(sp, msg);
  sp->minibuffer_on = 1;
  sp->minibuffer_temp = 0;
  goto_minibuffer(sp);
  return(CURSOR_NO_ACTION); /* make sure verbose cursor doesn't preload our prompt text field with garbage! */
}

static int region_count = 0;
static void get_amp_expression(snd_info *sp, int count, int regexpr) 
{
  prompt(sp, "env:", NULL); 
  sp->amping = count; 
  sp->reging = regexpr;
}

static void get_eval_expression(snd_info *sp, int count, int regexpr) 
{
  prompt(sp, "eval:", NULL); 
  sp->evaling = count; 
  sp->reging = regexpr;
}

static int prompt_named_mark(chan_info *cp) 
{
  snd_info *sp = cp->sound;
  clear_minibuffer(sp);
  make_minibuffer_label(sp, "mark:");
  sp->minibuffer_on = 1;
  goto_minibuffer(sp);
  sp->marking = cp->cursor + 1; /* +1 so it's not confused with 0 (if (sp->marking)...) */
  return(CURSOR_IN_VIEW);
}


int use_filename_completer(int filing)
{
  return((filing) &&
	 ((filing == INPUT_FILING) ||  /* C-x C-f */
	  (filing == CHANGE_FILING) || /* C-x C-q */
	  (filing == INSERT_FILING)));   /* C-x C-i */
}

static char *dir_from_tempnam(snd_state *ss)
{
  char *name;
  int i;
  name = snd_tempnam(ss);
  i = strlen(name)-1;
  while ((name[i] != '/') && (i > 0)) i--;
  if (i == 0) name[0] ='.';
  name[i + 1] ='\0';
  return(name);
}

static void goto_next_graph (chan_info *cp, int count);

static void goto_previous_graph (chan_info *cp, int count)
{
  snd_info *sp;
  snd_state *ss;
  chan_info *ncp, *vcp;
  int i, k, j, chan;
  if (count == 0) return;
  sp = cp->sound;
  ss = cp->state;
  vcp = virtual_selected_channel(cp);
  chan = vcp->chan;
  ncp = NULL;
  if (count < 0) 
    k = -count; 
  else 
    {
      goto_next_graph(cp, count); 
      return;
    }
  if (chan > 0)
    {
      /* goto previous channel in current sound */
      k -= chan;
      if (k <= 0)
	ncp = sp->chans[chan+count];
    }
  while (k > 0)
    {
      /* look for previous sound, (wrap around) */
      /* goto channel n */
      for (i = (sp->index - 1); i >= 0; i--)
	if (snd_ok(ss->sounds[i]))
	  {
	    sp = (snd_info *)(ss->sounds[i]);
	    j = k;
	    k -= sp->nchans;
	    if (k <= 0)
	      ncp = sp->chans[sp->nchans-j];
	    break;
	  }
      if (k > 0)
	for (i = ss->max_sounds-1; i >= sp->index; i--)
	  if (snd_ok(ss->sounds[i]))
	    {
	      sp = (snd_info *)(ss->sounds[i]);
	      j = k;
	      k -= sp->nchans;
	      if (k <= 0)
		ncp = sp->chans[sp->nchans-j];
	      break;
	    }
    }
  if (ncp == vcp) return;
  if (!ncp) snd_error("goto previous graph lost!");
  select_channel(ncp->sound, ncp->chan);
  normalize_sound(ss, ncp->sound, ncp); /* snd-xsnd.c */
  /* goto_graph(ncp); */
}

static void goto_next_graph (chan_info *cp, int count)
{
  snd_info *sp;
  snd_state *ss;
  chan_info *ncp, *vcp;
  int i, k, j, chan;
  if (count == 0) return;
  sp = cp->sound;
  ss = cp->state;
  vcp = virtual_selected_channel(cp);
  chan = vcp->chan;
  ncp = NULL;
  if (count < 0) 
    {
      goto_previous_graph(cp, count); 
      return;
    }
  k = count;
  if (chan < (sp->nchans-1))
    {
      /* goto next channel in current sound */
      k -= (sp->nchans-chan-1);
      if (k <= 0)
	ncp = sp->chans[chan+count];
    }
  while (k > 0)
    {
      /* look for next sound, (wrap around) */
      /* goto channel 0 */
      for (i = (sp->index + 1); i < ss->max_sounds; i++)
	if (snd_ok(ss->sounds[i]))
	  {
	    sp = (snd_info *)(ss->sounds[i]);
	    j = k;
	    k -= sp->nchans;
	    if (k <= 0)
	      ncp = sp->chans[j-1];
	    break;
	  }
      /* if (ss->listening) {goto_listener(); k--; if (k == 0) return;} */
      /* not really right because C-x in listener already exits listener (so C-x O in one chan case bounces back to self) */
      if (k > 0)
	for (i = 0; i <= sp->index; i++)
	  if (snd_ok(ss->sounds[i]))
	    {
	      sp = (snd_info *)(ss->sounds[i]);
	      j = k;
	      k -= sp->nchans;
	      if (k <= 0)
		ncp = sp->chans[j-1];
	      break;
	    }
    }
  if (ncp == vcp) return;
  if (!ncp) snd_error("goto next graph lost!");
  select_channel(ncp->sound, ncp->chan);
  normalize_sound(ss, ncp->sound, ncp);
  /* goto_graph(ncp); */
}



#if HAVE_DIRENT_H
  #include <dirent.h>
#else
  #define dirent direct
  #if HAVE_SYS_NDIR_H
    #include <sys/ndir.h>
  #endif
  #if HAVE_SYS_DIR_H
    #include <sys/dir.h>
  #endif
  #if HAVE_NDIR_H
    #include <ndir.h>
  #endif
#endif

void snd_minibuffer_activate(snd_info *sp, int keysym, int with_meta)
{
  snd_state *ss;
  snd_info *nsp;
  int s_or_r = 0;
  int nc, i, len;
  chan_info *active_chan;
  char *str = NULL, *mcf = NULL;
  char *tok, *newdir, *str1;
  env *e;
  mark *m;
#if HAVE_OPENDIR
  DIR *dp;
#endif
  SCM proc;
  if ((keysym == snd_K_s) || (keysym == snd_K_r)) s_or_r = 1;
  ss = sp->state;
  if (sp != selected_sound(ss)) select_channel(sp, 0);
  active_chan = any_selected_channel(sp);
  if (active_chan)
    {
      goto_graph(active_chan);
    }
  if ((keysym == snd_K_g) || (keysym == snd_K_G)) /* c-g => abort whatever we're doing and return */
    {
      set_minibuffer_string(sp, NULL);
      clear_minibuffer(sp);
      return;
    }
  if ((with_meta) && ((keysym == snd_K_p) || (keysym == snd_K_P) || (keysym == snd_K_n) || (keysym == snd_K_N)))
    {
      restore_mini_string(sp, (keysym == snd_K_p) || (keysym == snd_K_P));
      goto_minibuffer(sp);
      return;
    }

  str = get_minibuffer_string(sp);
  if ((str) && (*str)) 
    remember_mini_string(sp, str);

  if (sp->searching)
    {
      /* it's the search expr request */
      /* if not nil, replace previous */
      if (!s_or_r)
	{
	  /* str = get_minibuffer_string(sp); */
	  if ((str) && (*str))
	    {
	      /* check for procedure as arg, or lambda form:
	       * (lambda (y) (> y .1)) 
	       * if returns #t, search stops
	       */
	      if (sp->search_expr) free(sp->search_expr);
	      sp->search_expr = str;
	      if ((sp->search_proc) && 
		  (PROCEDURE_P(sp->search_proc))) 
		snd_unprotect(sp->search_proc);
	      sp->search_proc = SCM_UNDEFINED;
	      proc = snd_catch_any(eval_str_wrapper, str, str);
	      if (procedure_ok_with_error(proc, 1, 0, "find", "find procedure", 1))
		{
		  sp->search_proc = proc;
		  snd_protect(proc);
 		}
	    }
	}
      if (active_chan)
	handle_cursor_with_sync(active_chan, cursor_search(active_chan, sp->searching));
      return;
    }
  
  /* str = get_minibuffer_string(sp); */
  if ((sp->marking) || (sp->finding_mark))
    {
      if (sp->marking) 
	{
	  m = add_mark(sp->marking - 1, str, active_chan);
	  if (m)
	    {
	      report_in_minibuffer(sp, "%s placed at sample %d", str, sp->marking - 1);
	      draw_mark(active_chan, active_chan->axis, m);
	    }
	  else report_in_minibuffer(sp, "There is already a mark at sample %d", sp->marking - 1);
	  sp->marking = 0;
	}	
      else 
	{
	  handle_cursor_with_sync(active_chan, goto_named_mark(active_chan, str));
	  sp->finding_mark = 0;
	}
      if (str) free(str);
      return;
    }
  if (snd_strlen(str) != 0)
    {
      if (sp->printing)
	{
	  snd_print(ss, str);
	  sp->printing = 0;
	  clear_minibuffer(sp);
	  free(str);
	  return;
	}
      if (sp->loading)
	{
	  snd_load_file(str);
	  sp->loading = 0;
	  clear_minibuffer(sp);
	  free(str);
	  return;
	}
      if (sp->filing)
	{
	  switch (sp->filing)
	    {
	      /* don't free(str) locally in this switch statement without setting it to null as well */
	    case INPUT_FILING:
	      nsp = snd_open_file(str, ss); /* will post error if any */
	      if (nsp) 
		{
		  select_channel(nsp, 0);
		  clear_minibuffer(sp);
		}
	      break;
	    case REGION_FILING:
	      str1 = mus_expand_filename(str);
	      if (!(snd_overwrite_ok(ss, str1))) 
		{
		  free(str); 
		  FREE(str1); 
		  return;
		}
	      if ((region_count == 0) && (selection_is_active()))
		save_selection(ss, str1, MUS_NEXT, MUS_OUT_FORMAT, SND_SRATE(sp), NULL);
	      else save_region(ss, region_count, str1, MUS_OUT_FORMAT);
	      clear_minibuffer(sp);
	      FREE(str1);
	      break;
	    case CHANNEL_FILING:
	      chan_save_edits(active_chan, mcf = mus_expand_filename(str));
	      if (mcf) FREE(mcf);
	      clear_minibuffer(sp);
	      break;
#if HAVE_OPENDIR
	    case TEMP_FILING:
	      newdir = snd_strdup(str);
	      clear_minibuffer(sp);
	      dp = opendir(newdir);
	      if (dp) 
		{
		  closedir(dp);
		  set_temp_dir(ss, newdir);
		}
	      else 
		{
		  tok = dir_from_tempnam(ss);
		  report_in_minibuffer_and_save(sp, "can't access %s! temp dir is still %s", newdir, tok);
		  if (newdir) free(newdir);
		  if (tok) free(tok);
		}
	      break;
#endif
	    case CHANGE_FILING:
	      mix_complete_file(sp, str, "C-x C-q", with_mix_tags(ss));
	      break;
	    case INSERT_FILING:
	      str1 = mus_expand_filename(str);
	      nc = mus_sound_chans(str1);
	      if (nc != -1)
		{
		  len = mus_sound_samples(str1)/nc;
		  if (nc > sp->nchans) nc = sp->nchans;
		  if (!active_chan) active_chan = sp->chans[0];
		  for (i = 0; i < nc; i++)
		    {
		      file_insert_samples(active_chan->cursor, len, str1, sp->chans[i], i, DONT_DELETE_ME, "C-x C-i");
		      update_graph(sp->chans[i], NULL);
		    }
		  clear_minibuffer(sp);
		}
	      else report_in_minibuffer_and_save(sp, "can't read %s's header", str);
	      FREE(str1);
	      break;
	    case MACRO_FILING: 
	      name_last_macro(str); 
	      clear_minibuffer(sp); 
	      break;
	    }
	  sp->filing = NOT_FILING;
	  if (str) free(str);
	  return;
	}
      if (sp->amping)
	{
	  if (!active_chan) active_chan = sp->chans[0];
	  e = string2env(str);
	  if (e)
	    {
	      if (sp->amping != 1)
		apply_env(active_chan, e, active_chan->cursor, 
			  sp->amping, 1.0, sp->reging, NOT_FROM_ENVED,
			  (char *)((sp->reging) ? "C-x a" : "C-x C-a"), NULL);
	      else apply_env(active_chan, e, 0, current_ed_samples(active_chan), 1.0, 
			     sp->reging, NOT_FROM_ENVED,
			     (char *)((sp->reging) ? "C-x a" : "C-x C-a"), NULL);
	      e = free_env(e);
	    }
	  sp->reging = 0;
	  sp->amping = 0;
	  clear_minibuffer(sp);
	  if (str) free(str);
	  return;
	}
      if (sp->macroing)
	{
	  len = active_chan->cursor;
	  execute_named_macro(active_chan, str, sp->macroing);
	  /* if this is a close command from the current minibuffer, the sound may not exist when we return */
	  ss->mx_sp = NULL;
	  if (sp->state == NULL) return;
	  sp->macroing = 0;
	  if (active_chan->cursor != len)
	    {
	      nc = cursor_decision(active_chan);
	      if (nc == CURSOR_IN_VIEW) nc = CURSOR_UPDATE_DISPLAY; 
	    }
	  else nc = CURSOR_UPDATE_DISPLAY; 
	  handle_cursor_with_sync(active_chan, nc);
	  if (str) free(str);
	  return;
	}
      if (sp->prompting)
	{
	  proc = snd_catch_any(eval_str_wrapper, str, str);
	  snd_protect(proc);
	  if ((sp->prompt_callback) && 
	      (PROCEDURE_P(sp->prompt_callback)))
	    CALL2(sp->prompt_callback, proc, TO_SMALL_SCM_INT(sp->index), "prompt callback func");
	  snd_unprotect(proc);
	  if (str) free(str);
	  sp->prompting = 0;
	  return;
	}
      if ((sp->evaling) || (snd_eval_str(sp->state, str, 1) == -1))
	{
	  /* check for procedure as arg, or lambda form:
	   * (lambda (y) (+ y .1))
	   * if returns non-number, original value is used
	   * need handle in eval_expression and a flag in sp l4540
	   */
	  if (sp->eval_expr) free(sp->eval_expr);
	  sp->eval_expr = str;
	  if (sp->evaling)
	    {
	      if ((sp->eval_proc) && 
		  (PROCEDURE_P(sp->eval_proc))) 
		snd_unprotect(sp->eval_proc);
	      sp->eval_proc = SCM_UNDEFINED;
	      proc = snd_catch_any(eval_str_wrapper, str, str);
	      if (procedure_ok_with_error(proc, 1, 0, "eval", "eval procedure", 1))
		{
		  sp->eval_proc = proc;
		  snd_protect(proc);
		  eval_expression(active_chan, sp, sp->evaling, sp->reging);
		  clear_minibuffer_prompt(sp);
		  sp->evaling = 0;
		  sp->reging = 0;
		  return;
		}
	    }
	}
      sp->evaling = 0;
      sp->reging = 0;
    }
  else clear_minibuffer(sp);
}

static int stop_fft_in_progress(chan_info *cp, void *ptr)
{
  chan_context *cx;
  if ((cp) && (cp->cgx))
    {
      cx = cp->cgx;
      if (cx->fft_in_progress) 
	{
	  BACKGROUND_REMOVE(cx->fft_in_progress);
	  finish_progress_report(cp->sound, NOT_FROM_ENVED);
	  cx->fft_in_progress = 0;
	}
    }
  return(0);
}

static int cursor_moveto_beginning(chan_info *cp) 
{
  return(cursor_moveto(cp, 0)); /* is ap->xmin ever going to be non-zero? */
}

static int cursor_moveto_end(chan_info *cp)
{
  return(cursor_moveto(cp, current_ed_samples(cp)-1));
}

static int set_window_bounds(chan_info *cp, int count) 
{
  /* count = sample number to start at */
  axis_info *ap;
  Float sx;
  ap = cp->axis;
  sx = (((double)count / (double)SND_SRATE(cp->sound)) - ap->xmin) / ap->x_ambit;
  reset_x_display(cp, sx, ap->zx);
  return(CURSOR_IN_VIEW);
}

static int set_window_size(chan_info *cp, int count) 
{
  /* set samples within window */
  axis_info *ap;
  Float zx;
  ap = cp->axis;
  zx = ((double)count / (((double)SND_SRATE(cp->sound)) * ap->x_ambit));
  reset_x_display(cp, ap->sx, zx);
  return(CURSOR_IN_VIEW);
}

static int set_window_percentage(chan_info *cp, int count) 
{
  /* set percentage of file within window */
  axis_info *ap;
  Float zx;
  ap = cp->axis;
  zx = (double)count / (double)SND_SRATE(cp->sound);
  reset_x_display(cp, ap->sx, zx);
  return(CURSOR_IN_VIEW);
}

static void window_frames_selection(chan_info *cp)
{
  Float x0, x1;
  int i;
  snd_info *sp;
  snd_state *ss;
  x0 = (((double)(selection_beg(cp))) / ((double)SND_SRATE(cp->sound)));
  x1 = x0 + ((double)(selection_len())) / ((double)(SND_SRATE(cp->sound)));
  set_x_axis_x0x1(cp, x0, x1);
  ss = cp->state;
  for (i = 0; i < ss->max_sounds; i++)
    {
      sp = ss->sounds[i];
      if ((sp) && 
	  (sp->inuse) && 
	  (cp->sound != sp) && 
	  (selection_is_active_in_channel(sp->chans[0])) && 
	  (sp->syncing != (cp->sound->syncing)))
	set_x_axis_x0x1(sp->chans[0], x0, x1);
    }
}



static int get_count_1(char *number_buffer, int number_ctr, int dot_seen, chan_info *cp)
{
  /* allow floats here = secs */
  float f;
  int i;
  if (number_ctr == 0) return(1); /* c-u followed by nothing = 1 */
  number_buffer[number_ctr] = '\0';
  if (number_ctr == 1)
    { /* handle special cases of just - or + */
      if (number_buffer[0] == '-') return(-1);
      if (number_buffer[0] == '+') return(1);
    }
  if (dot_seen)
    {
      sscanf(number_buffer, "%f", &f);
      return((int)(f * SND_SRATE(cp->sound)));
    }
  else
    {
      sscanf(number_buffer, "%d", &i);
      return(i);
    }
  return(1);
}

static int get_count(char *number_buffer, int number_ctr, int dot_seen, chan_info *cp, int mark_wise)
{
  int val, old_cursor;
  val = get_count_1(number_buffer, number_ctr, dot_seen, cp);
  if (!mark_wise) return(val);
  old_cursor = cp->cursor;
  goto_mark(cp, val);
  val = cp->cursor - old_cursor; /* will be 0 if no relevant marks */
  cp->cursor = old_cursor;
  return(val);
}

#define NUMBER_BUFFER_SIZE 12

static Float state_amount (int state)
{
  Float amount;
  amount = 1.0;
  if (state & snd_ControlMask) amount *= 0.5;
  if (state & snd_MetaMask) amount *= 0.5;
  if (state & snd_ShiftMask) amount *= 0.5;
  return(amount);
}

static void no_selection_error(snd_info *sp)
{
  report_in_minibuffer(sp, "no active selection");
}

static int stop_selecting(int keysym, int state)
{
  return(((state & snd_ControlMask) == 0) ||
	 (keysym == snd_K_D) || (keysym == snd_K_d) ||
	 (keysym == snd_K_H) || (keysym == snd_K_H) ||
	 (keysym == snd_K_Y) || (keysym == snd_K_Y));
}

static char *key_to_name(int keysym) {if (keysym) return(KEY_TO_NAME(keysym)); else return("NUL");}

#define NO_CX_ARG_SPECIFIED -1

int keyboard_command (chan_info *cp, int keysym, int state)
{
  /* we can't use the meta bit in some cases because this is trapped at a higher level for the Menu mnemonics */
  /* state here is the kbd bucky-bit state */
  /* keysym has Shift taken into account already (see snd-xchn.c XKeycodeToKeysym, and same snd-xsnd.c) */
  static int number_ctr = 0;
  static int dot_seen = 0;
  static int counting = 0;
  static int u_count = 0;
  static char number_buffer[NUMBER_BUFFER_SIZE];
  static int extended_mode = 0;
  static int count = 1, got_count = 0;
  static int m = 0;
  int redisplay, searching, cursor_searching, hashloc, loc, sync_num, i;
  static int ext_count = NO_CX_ARG_SPECIFIED;
  snd_info *sp;
  axis_info *ap;
  snd_state *ss;
  sync_info *si;
  mark *mk = NULL;
  /* fprintf(stderr, "kbd: %d %d %d ", keysym, state, extended_mode); */
  if (!cp) return(KEYBOARD_NO_ACTION);
  redisplay = CURSOR_IN_VIEW;
  searching = 0;
  cursor_searching = 0;
  /* cp->cursor_on = 1; */
  sp = cp->sound;
  ss = cp->state;
  ap = cp->axis;
  if (keysym >= snd_K_Shift_L) return(KEYBOARD_NO_ACTION); 
  /* this happens when the user presses Control or Shift etc prior to hitting the actual (modified) key */
  if (defining_macro) continue_macro(keysym, state);
  if (!m) count = 1; else m = 0;
  if (u_count == 0) set_prefix_arg(ss, 0);
  
  /* should we check snd_keypad_Decimal as well as snd_K_period? -- is this assuming USA float syntax? */
  /*   (similarly snd_keypad_0...9) */

  if ((selection_creation_in_progress()) &&
      ((extended_mode) || (stop_selecting(keysym, state))))
    finish_selection_creation();

  if ((counting) && (((keysym < snd_K_0) || (keysym > snd_K_9)) && 
		     (keysym != snd_K_minus) && 
		     (keysym != snd_K_period) && 
		     (keysym != snd_K_plus)))
    {
      m = ((u_count) && 
	   ((keysym == snd_K_M) || (keysym == snd_K_m)));
      count = get_count(number_buffer, number_ctr, dot_seen, cp, m);
      got_count = 1;
      number_ctr = 0;
      counting = 0;
      dot_seen = 0;
      set_prefix_arg(ss, count);
      if (m) return(KEYBOARD_NO_ACTION);
    }
  u_count = 0;
  if ((keysym != snd_K_X) && (keysym != snd_K_x))
    {
      got_count = 0;
      if (count == 0) return(KEYBOARD_NO_ACTION);
    }
  if ((state & snd_MetaMask) && 
      ((keysym == snd_K_X) || (keysym == snd_K_x)))
    {
      /* named macros invoked and saved here */
      ss->mx_sp = sp;
      prompt(sp, "M-x:", NULL);
      sp->macroing = count;
      return(KEYBOARD_NO_ACTION);
    }
  hashloc = in_user_keymap(keysym, state);
  if (hashloc != -1)                       /* found user-defined key */
    return(call_user_keymap(hashloc, count));
  if (sp->minibuffer_temp) clear_minibuffer(sp);

  if (state & snd_ControlMask)
    {
      if (!extended_mode)
	{
	  /* -------------------------------- C-key -------------------------------- */
	  switch (keysym)
	    {
	    case snd_K_A: case snd_K_a: 
	      cp->cursor_on = 1; 
	      loc = (int)(ap->x0*SND_SRATE(sp)); 
	      if ((loc+1) == ap->losamp) loc = ap->losamp; /* handle dumb rounding problem */
	      cursor_moveto(cp, loc); 
	      break;
	    case snd_K_B: case snd_K_b: 
	      cp->cursor_on = 1; 
	      redisplay = cursor_move(cp, -count); 
	      break;
	    case snd_K_D: case snd_K_d: 
	      cp->cursor_on = 1; 
	      redisplay = cursor_delete(cp, count, "C-d"); 
	      break;
	    case snd_K_E: case snd_K_e:
	      cp->cursor_on = 1; 
	      loc = (int)(ap->x1*(double)SND_SRATE(sp));
	      if ((loc+1) == ap->hisamp) loc = ap->hisamp;
	      cursor_moveto(cp, loc); 
	      break;
	    case snd_K_F: case snd_K_f:
	      cp->cursor_on = 1; 
	      redisplay = cursor_move(cp, count); 
	      break;
	    case snd_K_G: case snd_K_g: 
	      number_ctr = 0; 
	      counting = 0; 
	      dot_seen = 0; 
	      deactivate_selection();
	      defining_macro = 0;
	      if ((ss->checking_explicitly) || (play_in_progress())) ss->stopped_explicitly = 1; 
	      /* this tries to break out of long filter/src computations (and perhaps others) */
	      if (sp->playing) stop_playing_all_sounds();
	      if (sp->applying) stop_applying(sp);
	      map_over_sound_chans(sp, stop_fft_in_progress, NULL);
	      clear_minibuffer(sp);
	      clear_listener();
	      break;
	    case snd_K_H: case snd_K_h: 
	      cp->cursor_on = 1; 
	      redisplay = cursor_delete_previous(cp, count, "C-h"); 
	      break; 
	    case snd_K_I: case snd_K_i: 
	      show_cursor_info(cp); 
	      searching = 1; 
	      break;
	    case snd_K_J: case snd_K_j: 
	      cp->cursor_on = 1; 
	      redisplay = goto_mark(cp, count); 
	      break;
	    case snd_K_K: case snd_K_k: 
	      cp->cursor_on = 1; 
	      redisplay = cursor_delete(cp, count*cp->line_size, "C-k"); 
	      break;
	    case snd_K_L: case snd_K_l: 
	      cp->cursor_on = 1; 
	      redisplay = CURSOR_IN_MIDDLE; 
	      break;
	    case snd_K_M: case snd_K_m:
	      if (count > 0) 
		{
		  cp->cursor_on = 1;
		  set_show_marks(ss, 1);
		  mk = add_mark(cp->cursor, NULL, cp);
		  if (mk) draw_mark(cp, cp->axis, mk);
		}
	      else delete_mark_samp(cp->cursor, cp);
	      if ((keysym == snd_K_M) && 
		  ((cp->sound)->syncing != 0))
		{
		  sync_num = mark_sync_max() + 1; 
		  if (mk) set_mark_sync(mk, sync_num);
		  si = snd_sync(cp->state, (cp->sound)->syncing);
		  for (i = 0; i < si->chans; i++) 
		    if (cp != si->cps[i])
		      {
			if (count > 0)
			  {
			    mk = add_mark(cp->cursor, NULL, si->cps[i]);
			    if (mk)
			      {
				set_mark_sync(mk, sync_num);
				draw_mark(si->cps[i], (si->cps[i])->axis, mk);
			      }
			  }
			else delete_mark_samp(cp->cursor, si->cps[i]);
		      }
		  si = free_sync_info(si);
		}
	      break;
	    case snd_K_N: case snd_K_n: 
	      cp->cursor_on = 1; 
	      redisplay = cursor_move(cp, count * cp->line_size); 
	      break;
	    case snd_K_O: case snd_K_o: 
	      cp->cursor_on = 1; 
	      redisplay = cursor_insert(cp, cp->cursor, count, "C-o"); 
	      break;
	    case snd_K_P: case snd_K_p: 
	      cp->cursor_on = 1; 
	      redisplay = cursor_move(cp, -count * cp->line_size); 
	      break;
	    case snd_K_Q: case snd_K_q: 
	      play_channel(cp, cp->cursor, NO_END_SPECIFIED, TRUE); 
	      set_play_button(sp, 1); 
	      redisplay = NO_ACTION; 
	      break;
	    case snd_K_R: case snd_K_r: 
	      cp->cursor_on = 1; 
	      redisplay = cursor_search(cp, -count); 
	      searching = 1; 
	      cursor_searching = 1; 
	      break;
	    case snd_K_S: case snd_K_s: 
	      cp->cursor_on = 1; 
	      redisplay = cursor_search(cp, count); 
	      searching = 1; 
	      cursor_searching = 1; 
	      break;
	    case snd_K_T: case snd_K_t: 
	      stop_playing_sound(sp); 
	      set_play_button(sp, 0);
	      redisplay = NO_ACTION; 
	      break;
	    case snd_K_U: case snd_K_u: 
	      counting = 1; 
	      u_count = 1;
	      number_ctr = 0; 
	      dot_seen = 0; 
	      break;
	    case snd_K_V: case snd_K_v:
	      cp->cursor_on = 1;
	      if (count > 0)
		redisplay = cursor_moveto(cp, (int)(ap->x1 * SND_SRATE(sp) + 1 + 
						    (count-1) * SND_SRATE(sp) * (ap->x1 - ap->x0)));
	      else redisplay = cursor_moveto(cp, (int)(ap->x0 * SND_SRATE(sp) - 1 + 
						       (count+1) * SND_SRATE(sp) * (ap->x1 - ap->x0)));
	      break;
	    case snd_K_W: case snd_K_w: 
	      delete_selection("C-x C-w", UPDATE_DISPLAY); 
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      break;
	    case snd_K_X: case snd_K_x: 
	      extended_mode = 1; 
	      if (got_count) 
		{
		  ext_count = count; 
		  got_count = 0;
		}
	      break;
	    case snd_K_Y: case snd_K_y: 
	      paste_region(count, cp, "C-y"); 
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      break;
	    case snd_K_Z: case snd_K_z: 
	      cp->cursor_on = 1; 
	      redisplay = cursor_zeros(cp, count, 0); 
	      break;
	    case snd_K_Right: 
	      sx_incremented(cp, state_amount(state)); 
	      break;
	    case snd_K_Left: 
	      sx_incremented(cp, -state_amount(state)); 
	      break;
	    case snd_K_Up: 
	      zx_incremented(cp, 1.0 + state_amount(state)); 
	      break;
	    case snd_K_Down: 
	      zx_incremented(cp, 1.0 / (1.0+state_amount(state))); 
	      break;
	    case snd_K_0: case snd_K_1: case snd_K_2: case snd_K_3: case snd_K_4:
	    case snd_K_5: case snd_K_6: case snd_K_7: case snd_K_8: case snd_K_9: 
	      counting = 1;
	      number_buffer[number_ctr] = (char)('0' + keysym - snd_K_0); 
	      if (number_ctr < (NUMBER_BUFFER_SIZE-2)) 
		number_ctr++; 
	      /* there is also the bare-number case below */
	      break;
	    case snd_K_space: 
	      if (count > 0)
		{
		  start_selection_creation(cp, cp->cursor);
		  redisplay = NO_ACTION;
		}
	      break;
	    case snd_K_period:
	      counting = 1; 
	      number_buffer[number_ctr] = '.'; 
	      number_ctr++; 
	      dot_seen = 1; 
	      break;
	    case snd_K_greater: 
	      cp->cursor_on = 1; 
	      redisplay = cursor_moveto_end(cp); 
	      break;
	    case snd_K_less: 
	      cp->cursor_on = 1; 
	      redisplay = cursor_moveto_beginning(cp); 
	      break;
	    case snd_K_minus: 
	      counting = 1; 
	      number_ctr = 1; 
	      number_buffer[0] = '-'; 
	      break;
	    case snd_K_underscore: 
	      undo_edit_with_sync(cp, count); 
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      break;
#if HAVE_ARROW_KEYS
	    case snd_keypad_Left: 
	    case snd_keypad_4: 
	      set_spectro_y_angle(ss, spectro_y_angle(ss) - 1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_Right: 
	    case snd_keypad_6: 
	      set_spectro_y_angle(ss, spectro_y_angle(ss) + 1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_Down: 
	    case snd_keypad_2: 
	      set_spectro_x_angle(ss, spectro_x_angle(ss) - 1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_Up: 
	    case snd_keypad_8: 
	      set_spectro_x_angle(ss, spectro_x_angle(ss) + 1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
#else
	    case snd_keypad_4: 
	      set_spectro_y_angle(ss, spectro_y_angle(ss) - 1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_6: 
	      set_spectro_y_angle(ss, spectro_y_angle(ss) + 1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_2:
	      set_spectro_x_angle(ss, spectro_x_angle(ss) - 1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_8: 
	      set_spectro_x_angle(ss, spectro_x_angle(ss) + 1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
#endif
	    default:
	      report_in_minibuffer(sp, "C-%s undefined", key_to_name(keysym));
	      break;
	    }
	}
      else /* extended mode with ctrl down */
	{
	  /* -------------------------------- C-x C-key -------------------------------- */
	  if (ext_count == NO_CX_ARG_SPECIFIED) ext_count = 1;
	  extended_mode = 0;
	  switch (keysym)
	    {
	    case snd_K_A: case snd_K_a: 
	      get_amp_expression(sp, ext_count, 0);
	      searching = 1; 
	      redisplay = CURSOR_IN_VIEW; 
	      break;
	    case snd_K_B: case snd_K_b: 
	      redisplay = set_window_bounds(cp, ext_count); 
	      break;
	    case snd_K_C: case snd_K_c: 
	      sound_hide_ctrls(sp); 
	      break;
	    case snd_K_D: case snd_K_d: 
	      redisplay = prompt(sp, "eps file:", NULL); 
	      sp->printing = ext_count; 
	      searching = 1; 
	      break;
	    case snd_K_E: case snd_K_e: 
	      redisplay = prompt(sp, "macro name:", NULL); 
	      sp->filing = MACRO_FILING; 
	      searching = 1; 
	      break;
	    case snd_K_F: case snd_K_f: 
	      redisplay = prompt(sp, "file:", NULL); 
	      sp->filing = INPUT_FILING; 
	      searching = 1; 
	      break;
	    case snd_K_G: case snd_K_g: 
	      number_ctr = 0;
	      counting = 0; 
	      dot_seen = 0; 
	      defining_macro = 0;
	      if ((ss->checking_explicitly) || (play_in_progress())) ss->stopped_explicitly = 1; 
	      clear_listener();
	      break;
	    case snd_K_I: case snd_K_i: 
	      redisplay = prompt(sp, "insert file:", NULL); 
	      sp->filing = INSERT_FILING; 
	      searching = 1; 
	      break;
	    case snd_K_J: case snd_K_j:
	      cp->cursor_on = 1; 
	      redisplay = goto_mix(cp, ext_count); 
	      break;
	    case snd_K_L: case snd_K_l: 
	      redisplay = prompt(sp, "load:", NULL); 
	      sp->loading = 1;
	      searching = 1; 
	      break;
	    case snd_K_M: case snd_K_m:
	      cp->cursor_on = 1; 
	      redisplay = prompt_named_mark(cp);
	      set_show_marks(ss, 1); 
	      searching = 1; 
	      break;
	    case snd_K_N: case snd_K_n: 
	      eval_expression(cp, sp, ext_count, 0); 
	      searching = 1; 
	      redisplay = CURSOR_IN_VIEW; 
	      break;
	    case snd_K_O: case snd_K_o: 
	      sound_show_ctrls(sp); 
	      break;
	    case snd_K_P: case snd_K_p: 
	      redisplay = set_window_size(cp, ext_count); 
	      break;
	    case snd_K_Q: case snd_K_q: 
	      redisplay = prompt(sp, STR_mix_file_p, NULL); 
	      sp->filing = CHANGE_FILING; 
	      searching = 1; 
	      break;
	    case snd_K_R: case snd_K_r: 
	      redo_edit_with_sync(cp, ext_count); 
	      redisplay = CURSOR_UPDATE_DISPLAY;
	      break;
	    case snd_K_S: case snd_K_s: 
	      save_edits(sp, NULL); 
	      redisplay = CURSOR_IN_VIEW; 
	      break;
	    case snd_K_T: case snd_K_t: 
	      stop_playing_sound(sp); 
	      redisplay = NO_ACTION; 
	      break;
	    case snd_K_U: case snd_K_u: 
	      undo_edit_with_sync(cp, ext_count); 
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      break;
	    case snd_K_V: case snd_K_v:
	      redisplay = set_window_percentage(cp, ext_count);
	      break;
	    case snd_K_W: case snd_K_w: 
	      redisplay = prompt(sp, STR_file_p, NULL); 
	      sp->filing = CHANNEL_FILING; 
	      searching = 1; 
	      break;
	    case snd_K_X: case snd_K_x: 
	      get_eval_expression(sp, ext_count, 0); 
	      searching = 1; 
	      redisplay = CURSOR_IN_VIEW; 
	      break;
	    case snd_K_Z: case snd_K_z: 
	      cp->cursor_on = 1; 
	      cos_smooth(cp, cp->cursor, ext_count, 0, "C-x C-z"); 
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      break;
	    case snd_K_Right: 
	      sx_incremented(cp, state_amount(state)); 
	      break;
	    case snd_K_Left:  
	      sx_incremented(cp, -state_amount(state)); 
	      break;
	    case snd_K_Up: 
	      zx_incremented(cp, 1.0 + state_amount(state)); 
	      break;
	    case snd_K_Down: 
	      zx_incremented(cp, 1.0 / (1.0 + state_amount(state))); 
	      break;
	    default:
	      report_in_minibuffer(sp, "C-x C-%s undefined", key_to_name(keysym));
	      break;
	    }
	}
    }
  else
    {
      if (!extended_mode)
	/* -------------------------------- key (or M-key) -------------------------------- */
	{ /* no control (but possibly meta), not extended mode -- bare alpha chars sent to listener if possible */
	  /*   (we already checked for possible user key bindings above) */
	  switch (keysym)
	    {
	    case snd_K_0: case snd_K_1: case snd_K_2: case snd_K_3: case snd_K_4:
	    case snd_K_5: case snd_K_6: case snd_K_7: case snd_K_8: case snd_K_9: 
	      counting = 1;
	      number_buffer[number_ctr] =(char)('0' + keysym-snd_K_0); 
	      if (number_ctr < (NUMBER_BUFFER_SIZE-2)) 
		number_ctr++; 
	      break;
	    case snd_K_period: 
	      counting = 1; 
	      number_buffer[number_ctr] = '.'; 
	      number_ctr++; 
	      dot_seen = 1; 
	      break;
	    case snd_K_greater: 
	      cp->cursor_on = 1; 
	      redisplay = cursor_moveto_end(cp); 
	      break;
	    case snd_K_less: 
	      cp->cursor_on = 1; 
	      redisplay = cursor_moveto_beginning(cp); 
	      break;
	    case snd_K_minus: 
	      counting = 1; 
	      number_buffer[0] = '-'; 
	      number_ctr = 1; 
	      break;
	    case snd_K_Right: 
	      sx_incremented(cp, state_amount(state)); 
	      break;
	    case snd_K_Left:  
	      sx_incremented(cp, -state_amount(state)); 
	      break;
	    case snd_K_Up:    
	      zx_incremented(cp, 1.0 + state_amount(state)); 
	      break;
	    case snd_K_Down: 
	      zx_incremented(cp, 1.0 / (1.0 + state_amount(state))); 
	      break;
	    case snd_K_Home: 
	      snd_update(ss, sp); 
	      break;
	    case snd_K_space: 
	      if (play_in_progress())
		toggle_dac_pausing(ss); 
	      else deactivate_selection();
	      break;

	      /* fUn WiTh KeYpAd! */
#if HAVE_ARROW_KEYS
	    case snd_keypad_Up: case snd_keypad_8: 
	      set_spectro_z_scale(ss, spectro_z_scale(ss) + .01);
	      redisplay = CURSOR_UPDATE_DISPLAY;
	      reflect_spectro(ss);
	      break;
	    case snd_keypad_Down: case snd_keypad_2: 
	      set_spectro_z_scale(ss, spectro_z_scale(ss) - .01);
	      redisplay = CURSOR_UPDATE_DISPLAY;
	      reflect_spectro(ss);
	      break;
	    case snd_keypad_Left: case snd_keypad_4: 
	      set_spectro_z_angle(ss, spectro_z_angle(ss) - 1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY;
	      reflect_spectro(ss);
	      break;
	    case snd_keypad_Right: case snd_keypad_6: 
	      set_spectro_z_angle(ss, spectro_z_angle(ss) + 1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
#else
	    case snd_keypad_8: 
	      set_spectro_z_scale(ss, spectro_z_scale(ss) + .01);
	      redisplay = CURSOR_UPDATE_DISPLAY;
	      reflect_spectro(ss);
	      break;
	    case snd_keypad_2: 
	      set_spectro_z_scale(ss, spectro_z_scale(ss) - .01);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_4: 
	      set_spectro_z_angle(ss, spectro_z_angle(ss) - 1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY;
	      reflect_spectro(ss);
	      break;
	    case snd_keypad_6: 
	      set_spectro_z_angle(ss, spectro_z_angle(ss) + 1.0);
	      redisplay = CURSOR_UPDATE_DISPLAY;
	      reflect_spectro(ss); 
	      break;
#endif
	    case snd_keypad_Add:
	      if (wavo(ss)) 
		set_wavo_trace(ss, wavo_trace(ss)+1); 
	      else set_spectro_hop(ss, spectro_hop(ss) + 1);
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_Subtract: 
	      if (wavo(ss)) 
		{
		  if (wavo_trace(ss)>1) 
		    set_wavo_trace(ss, wavo_trace(ss) - 1);
		} 
	      else 
		{
		  if (spectro_hop(ss)>1) 
		    set_spectro_hop(ss, spectro_hop(ss) - 1);
		}
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_Multiply: 
	      set_fft_size(ss, fft_size(ss) * 2); 
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      break;
	    case snd_keypad_Divide: 
	      if (fft_size(ss) > 4) 
		set_fft_size(ss, fft_size(ss) / 2); 
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      break;
#if HAVE_ARROW_KEYS
	    case snd_keypad_Delete: case snd_keypad_Decimal: 
	      set_dot_size(ss, dot_size(ss) + 1); 
	      redisplay = KEYBOARD_NO_ACTION; 
	      break;
	    case snd_keypad_Insert: case snd_keypad_0: 
	      if (dot_size(ss) > 1) 
		set_dot_size(ss, dot_size(ss) - 1); 
	      redisplay = KEYBOARD_NO_ACTION; 
	      break;
	    case snd_keypad_PageDown: case snd_keypad_3: 
	      set_spectro_cutoff(ss, spectro_cutoff(ss) * .95); 
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_PageUp: case snd_keypad_9: 
	      if (spectro_cutoff(ss) < 1.0) 
		set_spectro_cutoff(ss, spectro_cutoff(ss) / .95); 
	      redisplay = CURSOR_UPDATE_DISPLAY; reflect_spectro(ss); 
	      break;
#else
	    case snd_keypad_Decimal: 
	      set_dot_size(ss, dot_size(ss) + 1);
	      redisplay = KEYBOARD_NO_ACTION; 
	      break;
	    case_snd_keypad_0: 
	      if (dot_size(ss) > 1) 
		set_dot_size(ss, dot_size(ss) - 1); 
	      redisplay = KEYBOARD_NO_ACTION;
	      break;
	    case snd_keypad_3: 
	      set_spectro_cutoff(ss, spectro_cutoff(ss) * .95); 
	      redisplay = CURSOR_UPDATE_DISPLAY;
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_9: 
	      if (spectro_cutoff(ss) < 1.0) 
		set_spectro_cutoff(ss, spectro_cutoff(ss) / .95); 
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
#endif
	    case snd_keypad_Enter: 
	      reset_spectro(ss); 
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      reflect_spectro(ss); 
	      break;
	    default:
	      /* try to send unbuckified random keyboard input to the lisp listener */

#if USE_MOTIF
	      /* if meta key, assume for now that it's intended as a menu accelerator (don't send it to the listener) */
	      /*    in gtk (and apparently some motif's?) we don't even see the accelerator, but in Linux we do */
	      if ((state & snd_MetaMask) &&
		  ((keysym == snd_K_f) || (keysym == snd_K_e) || (keysym == snd_K_v) || (keysym == snd_K_o) || (keysym == snd_K_h)))
		/* here the accelerators are f e v o h (not upper case for some reason -- it's given as 'F' in snd-xmenu.c) */
		return(KEYBOARD_NO_ACTION);
	      /* it might be better to remove the menu accelerators -- they are a dubious feature to begin with */
#endif

	      if (ss->listening != LISTENER_CLOSED)
		{
		  if (listener_height() > 5)
		    {
		      char buf[2];
		      goto_listener();
		      buf[0] = keysym; buf[1] = 0;
		      snd_append_char(ss, buf);
		    }
		  /* else activate?? */
		}
	      else 
		{
		  if (keysym == snd_K_openparen)
		    {
		      ss->mx_sp = sp;
		      prompt(sp, "M-x:", "(");
		      sp->macroing = count;
		      redisplay = KEYBOARD_NO_ACTION;
		    }
		  report_in_minibuffer(sp, "%s undefined", key_to_name(keysym));
		}
	      /* should we open the minibuffer in all cases? */
	      break;
	    }
	}
      else 
	/* extended mode with ctrl up -- where not an emacs analogy, related to the current selection */
	/*   the active selection now follows the edit list, so there may be no relation between
	 *   region 0 and the active selection -- in ambiguous cases, we need to look explicitly
	 *   for the selection first.
	 */
	{
	  /* -------------------------------- C-x key -------------------------------- */
	  extended_mode = 0;
	  switch (keysym)
	    {
	    case snd_K_A: case snd_K_a: 
	      if (selection_is_active_in_channel(cp)) 
		{
		  get_amp_expression(sp, (ext_count == NO_CX_ARG_SPECIFIED) ? 1 : ext_count, 1); 
		  searching = 1; 
		  redisplay = CURSOR_IN_VIEW;
		} 
	      else no_selection_error(sp); 
	      break;
	    case snd_K_B: case snd_K_b: 
	      cp->cursor_on = 1; 
	      redisplay = CURSOR_ON_LEFT; 
	      break;
	    case snd_K_C: case snd_K_c: 
	      mark_define_region(cp, (ext_count == NO_CX_ARG_SPECIFIED) ? 1 : ext_count); 
	      redisplay = CURSOR_UPDATE_DISPLAY;
	      break;
	    case snd_K_D: case snd_K_d: 
	      redisplay = prompt(sp, "temp dir:", NULL); 
	      sp->filing = TEMP_FILING; 
	      searching = 1; 
	      break;
	    case snd_K_E: case snd_K_e: 
	      execute_last_macro(cp, (ext_count == NO_CX_ARG_SPECIFIED) ? 1 : ext_count);
	      redisplay = cursor_decision(cp);
	      if (redisplay == CURSOR_IN_VIEW) 
		redisplay = CURSOR_UPDATE_DISPLAY; 
	      break;
	    case snd_K_F: case snd_K_f: 
	      cp->cursor_on = 1; 
	      redisplay = CURSOR_ON_RIGHT; 
	      break;
	    case snd_K_I: case snd_K_i: 
	      paste_selection_or_region(ss, (ext_count == NO_CX_ARG_SPECIFIED) ? 0 : ext_count, cp, "C-x i");
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      break;
	    case snd_K_J: case snd_K_j: 
	      redisplay = prompt(sp, "mark:", NULL); 
	      sp->finding_mark = 1; 
	      searching = 1; 
	      break;
	    case snd_K_K: case snd_K_k: 
	      snd_close_file(sp, ss); 
	      redisplay = CURSOR_NO_ACTION; 
	      break;
	    case snd_K_L: case snd_K_l: 
	      cp->cursor_on = 1;
	      if (selection_is_active_in_channel(cp))
		cursor_moveto(cp, (int)(selection_beg(cp) + 0.5 * selection_len()));
	      else no_selection_error(sp); 
	      redisplay = CURSOR_IN_MIDDLE;
	      break;
	    case snd_K_N: case snd_K_n: 
	      if (selection_is_active_in_channel(cp))
		eval_expression(cp, sp, (ext_count == NO_CX_ARG_SPECIFIED) ? 1 : ext_count, TRUE); 
	      else no_selection_error(sp); 
	      break;
	    case snd_K_O: case snd_K_o: 
	      if (ext_count > 0) 
		goto_next_graph(cp, ext_count); 
	      else goto_previous_graph(cp, ext_count); 
	      redisplay = CURSOR_NO_ACTION; 
	      break;
	    case snd_K_P: case snd_K_p: 
	      if (ext_count == NO_CX_ARG_SPECIFIED)
		play_selection(IN_BACKGROUND);
	      else play_region(ss, ext_count, IN_BACKGROUND);  /* was false?? */
	      redisplay = NO_ACTION;
	      break;
	    case snd_K_Q: case snd_K_q: 
	      add_selection_or_region(ss, (ext_count == NO_CX_ARG_SPECIFIED) ? 0 : ext_count, cp, "C-x q"); 
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      break;
	    case snd_K_R: case snd_K_r: 
	      redo_edit_with_sync(cp, (ext_count == NO_CX_ARG_SPECIFIED) ? 1 : ext_count); 
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      break;
	    case snd_K_U: case snd_K_u: 
	      undo_edit_with_sync(cp, (ext_count == NO_CX_ARG_SPECIFIED) ? 1 : ext_count); 
	      redisplay = CURSOR_UPDATE_DISPLAY; 
	      break;
	    case snd_K_V: case snd_K_v: 
	      if (selection_is_active_in_channel(cp))
		{
		  window_frames_selection(cp); 
		  redisplay = CURSOR_UPDATE_DISPLAY; 
		}
	      else no_selection_error(sp); 
	      break;
	    case snd_K_W: case snd_K_w:
	      region_count = ((ext_count == NO_CX_ARG_SPECIFIED) ? 0 : ext_count);
	      redisplay = prompt(sp, STR_file_p, NULL); 
	      sp->filing = REGION_FILING; 
	      searching = 1;
	      break;
	    case snd_K_X: case snd_K_x: 
	      if (selection_is_active_in_channel(cp))
		{
		  get_eval_expression(sp, (ext_count == NO_CX_ARG_SPECIFIED) ? 1 : ext_count, TRUE); 
		  searching = 1; 
		  redisplay = CURSOR_IN_VIEW;
		}
	      else no_selection_error(sp); 
	      break;
	    case snd_K_Z: case snd_K_z: 
	      if (selection_is_active_in_channel(cp))
		{
		  cos_smooth(cp, cp->cursor, (ext_count == NO_CX_ARG_SPECIFIED) ? 1 : ext_count, 1, "C-x z"); 
		  redisplay = CURSOR_UPDATE_DISPLAY; 
		}
	      else no_selection_error(sp); 
	      break;
	    case snd_K_Right:   
	      sx_incremented(cp, state_amount(state));
	      break;
	    case snd_K_Left:
	      sx_incremented(cp, -state_amount(state));
	      break;
	    case snd_K_Up:
	      zx_incremented(cp, 1.0 + state_amount(state));
              break;
	    case snd_K_Down:
	      zx_incremented(cp, 1.0 / (1.0 + state_amount(state)));
	      break;
	    case snd_K_less:
	      cp->cursor_on = 1; 
	      redisplay = cursor_moveto_beginning(cp); 
	      break;
	    case snd_K_greater: 
	      cp->cursor_on = 1; 
	      redisplay = cursor_moveto_end(cp);
	      break;
	    case snd_K_openparen:
	      if (defining_macro) 
		report_in_minibuffer(sp, "macro definition already in progress");
	      else
		{
		  start_defining_macro(); 
		  report_in_minibuffer(sp, "defining macro..."); 
		}
	      redisplay = NO_ACTION; 
	      break;
	    case snd_K_closeparen: 
	      if (defining_macro)
		{
		  stop_defining_macro(); 
		  clear_minibuffer(sp); 
		}
	      redisplay = NO_ACTION;
	      break;
	    case snd_K_slash: 
	      cp->cursor_on = 1;
	      redisplay = prompt_named_mark(cp); 
	      set_show_marks(ss, 1); 
	      searching = 1; 
	      break;
	    default:
	      report_in_minibuffer(sp, "C-x %s undefined", key_to_name(keysym));
	      break;
	    }
	}
    }
  if (!extended_mode) ext_count = NO_CX_ARG_SPECIFIED;
  if (redisplay != NO_ACTION)
    {
      if ((sp->minibuffer_on) && (!searching)) 
	clear_minibuffer(sp);
      else if (!cursor_searching) 
	sp->searching = 0;
      return(redisplay);
    }
  else return(KEYBOARD_NO_ACTION);
}


static SCM g_bind_key(SCM key, SCM state, SCM code, SCM ignore_prefix)
{
  #define H_bind_key "(" S_bind_key " key modifiers func (ignore-prefix #f)) causes 'key' (an integer) \
when typed with 'modifiers' (1:shift, 4:control, 8:meta) to invoke 'func', a function of \
no arguments.  If ignore-prefix is #t, preceding C-u arguments are not handled by Snd itself. \
The function should return one of the cursor choices (e.g. cursor-no-action)."

  int ip;
  char *errmsg;
  SCM errstr;
  ASSERT_TYPE(INTEGER_P(key), key, SCM_ARG1, S_bind_key, "an integer");
  ASSERT_TYPE(INTEGER_P(state), state, SCM_ARG2, S_bind_key, "an integer");
  ASSERT_TYPE((FALSE_P(code) || PROCEDURE_P(code)), code, SCM_ARG3, S_bind_key, "#f or a procedure");
  if ((FALSE_P(ignore_prefix)) || 
      (NOT_BOUND_P(ignore_prefix)) ||  
      ((NUMBER_P(ignore_prefix)) && 
       (TO_C_INT_OR_ELSE(ignore_prefix, 0) == 0)))
    ip = 0;
  else ip = 1;
  if (FALSE_P(code))
    set_keymap_entry(TO_C_INT(key), 
		     TO_C_INT(state), 
		     ip, SCM_UNDEFINED);
  else 
    {
      errmsg = procedure_ok(code, 0, 0, S_bind_key, "func", 3);
      if (errmsg)
	{
	  errstr = TO_SCM_STRING(errmsg);
	  FREE(errmsg);
	  snd_bad_arity_error(S_bind_key, errstr, code);
	}
      set_keymap_entry(TO_C_INT_OR_ELSE(key, 0), 
		       TO_C_INT_OR_ELSE(state, 0), 
		       ip, code);
    }
  return(SCM_BOOL_T);
}

static SCM g_key(SCM kbd, SCM buckybits, SCM snd, SCM chn)
{
  #define H_key "(" S_key " key modifiers &optional snd chn) simulates typing 'key' with 'modifiers' in snd's channel chn"
  chan_info *cp;
  ASSERT_TYPE(INTEGER_P(kbd), kbd, SCM_ARG1, S_key, "an integer");
  ASSERT_TYPE(INTEGER_P(buckybits), buckybits, SCM_ARG2, S_key, "an integer");
  SND_ASSERT_CHAN(S_key, snd, chn, 3);
  cp = get_cp(snd, chn, S_key);
  return(TO_SCM_INT(keyboard_command(cp, 
				     TO_C_INT(kbd), 
				     TO_C_INT(buckybits))));
}

static SCM g_save_macros(void) 
{
  #define H_save_macros "(" S_save_macros ") saves keyboard macros in Snd's init file (.snd)"
  FILE *fd = NULL;
  snd_state *ss;
  ss = get_global_state();
  fd = open_snd_init_file(ss);
  if (fd) save_macro_state(fd);
  if ((!fd) || (fclose(fd) != 0))
    scm_throw(CANNOT_SAVE,
	      SCM_LIST3(TO_SCM_STRING(S_save_macros),
			TO_SCM_STRING(ss->init_file),
			TO_SCM_STRING(strerror(errno))));
  return(TO_SCM_STRING(ss->init_file));
}

static SCM g_prompt_in_minibuffer(SCM msg, SCM callback, SCM snd_n)
{
  #define H_prompt_in_minibuffer "(" S_prompt_in_minibuffer " msg callback &optional snd) posts msg in snd's minibuffer \
then when the user eventually responds, invokes the function callback with the response and snd (the index)"

  snd_info *sp;
  ASSERT_TYPE(STRING_P(msg), msg, SCM_ARG1, S_prompt_in_minibuffer, "a string");
  ASSERT_TYPE((NOT_BOUND_P(callback)) || (BOOLEAN_P(callback)) || PROCEDURE_P(callback), callback, SCM_ARG2, S_prompt_in_minibuffer, "#f or a procedure");
  SND_ASSERT_SND(S_prompt_in_minibuffer, snd_n, 3);
  sp = get_sp(snd_n);
  if (sp == NULL) 
    snd_no_such_sound_error(S_prompt_in_minibuffer, snd_n);
  if ((sp->prompt_callback) && 
      (PROCEDURE_P(sp->prompt_callback))) 
    snd_unprotect(sp->prompt_callback);
  if (PROCEDURE_P(callback)) 
    {
      sp->prompt_callback = callback;
      snd_protect(sp->prompt_callback);
    }
  else sp->prompt_callback = SCM_BOOL_F;
  make_minibuffer_label(sp, TO_C_STRING(msg));
  sp->minibuffer_on = 1;
  sp->minibuffer_temp = 0;
  sp->prompting = 1;
  goto_minibuffer(sp);
  return(SCM_BOOL_F);
}

static SCM g_report_in_minibuffer(SCM msg, SCM snd_n)
{
  #define H_report_in_minibuffer "(" S_report_in_minibuffer " msg &optional snd) displays msg in snd's minibuffer"
  snd_info *sp;
  ASSERT_TYPE(STRING_P(msg), msg, SCM_ARG1, S_report_in_minibuffer, "a string");
  SND_ASSERT_SND(S_report_in_minibuffer, snd_n, 2);
  sp = get_sp(snd_n);
  if (sp == NULL) 
    snd_no_such_sound_error(S_report_in_minibuffer, snd_n);
  report_in_minibuffer(sp, TO_C_STRING(msg));
  return(msg);
}

static SCM g_append_to_minibuffer(SCM msg, SCM snd_n)
{
  #define H_append_to_minibuffer "(" S_append_to_minibuffer " msg &optional snd) appends msg to snd's minibuffer"
  snd_info *sp;
  char *str1 = NULL, *expr_str;
  ASSERT_TYPE(STRING_P(msg), msg, SCM_ARG1, S_append_to_minibuffer, "a string");
  SND_ASSERT_SND(S_append_to_minibuffer, snd_n, 2);
  sp = get_sp(snd_n);
  if (sp == NULL) 
    snd_no_such_sound_error(S_append_to_minibuffer, snd_n);
  expr_str = (char *)CALLOC(512, sizeof(char));
  mus_snprintf(expr_str, 512, "%s%s", str1 = get_minibuffer_string(sp), TO_C_STRING(msg));
  set_minibuffer_string(sp, expr_str);
  FREE(expr_str);
  sp->minibuffer_temp = 1;
  if (str1) free(str1);
  return(msg);
}

static SCM g_forward_graph(SCM count, SCM snd, SCM chn) 
{
  #define H_forward_graph "(" S_forward_graph " &optional (count 1) snd chn) moves the 'selected' graph forward by count"
  int val;
  chan_info *cp;
  ASSERT_TYPE(INTEGER_IF_BOUND_P(count), count, SCM_ARG1, S_forward_graph, "an integer");
  SND_ASSERT_CHAN(S_forward_graph, snd, chn, 2);
  cp = get_cp(snd, chn, S_forward_graph);
  val = TO_C_INT_OR_ELSE(count, 1);
  goto_next_graph(cp, val);
  return(TO_SCM_INT(val));
}

static SCM g_backward_graph(SCM count, SCM snd, SCM chn) 
{
  #define H_backward_graph "(" S_backward_graph " &optional (count 1) snd chn) moves the 'selected' graph back by count"
  int val;
  chan_info *cp;
  ASSERT_TYPE(INTEGER_IF_BOUND_P(count), count, SCM_ARG1, S_backward_graph, "an integer");
  SND_ASSERT_CHAN(S_backward_graph, snd, chn, 2);
  cp = get_cp(snd, chn, S_backward_graph);
  val = -(TO_C_INT_OR_ELSE(count, 1));
  goto_previous_graph(cp, val);
  return(TO_SCM_INT(val));
}


void g_init_kbd(SCM local_doc)
{
  DEFINE_PROC(S_forward_graph,           g_forward_graph, 0, 3, 0,           H_forward_graph);
  DEFINE_PROC(S_backward_graph,          g_backward_graph, 0, 3, 0,          H_backward_graph);

  DEFINE_PROC(S_key_binding,             g_key_binding, 2, 0, 0,             H_key_binding);
  DEFINE_PROC(S_bind_key,                g_bind_key, 3, 1, 0,                H_bind_key);
  DEFINE_PROC(S_key,                     g_key, 2, 2, 0,                     H_key);
  DEFINE_PROC(S_save_macros,             g_save_macros, 0, 0, 0,             H_save_macros);

  DEFINE_PROC(S_report_in_minibuffer,    g_report_in_minibuffer, 1, 1, 0,    H_report_in_minibuffer);
  DEFINE_PROC(S_prompt_in_minibuffer,    g_prompt_in_minibuffer, 1, 2, 0,    H_prompt_in_minibuffer);
  DEFINE_PROC(S_append_to_minibuffer,    g_append_to_minibuffer, 1, 1, 0,    H_append_to_minibuffer);
}
