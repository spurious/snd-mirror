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
#if HAVE_RUBY
  fprintf(fd, "def %s\n", nm->name);
  for (i = 0; i < nm->macro_size; i++)
    {
      mc = nm->cmds[i];
      if (mc->keysym != 0)
	fprintf(fd, 
		"  %s %d %d\n", 
		S_key, (int)(mc->keysym), mc->state);
    }
  fprintf(fd, "end\n");
#else
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
#endif
}

static int execute_named_macro_1(chan_info *cp, char *name, off_t count)
{
  int i, k;
  off_t j;
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

static void execute_named_macro(chan_info *cp, char *name, off_t count)
{
  int one_edit, i;
  XEN form; XEN result = XEN_UNDEFINED;
  if (!(execute_named_macro_1(cp, name, count)))
    /* not a macro...*/
    {
      one_edit = cp->edit_ctr + 1;
      form = string_to_form(name);
      for (i = 0; i < count; i++)
	result = snd_catch_any(eval_form_wrapper, (void *)form, name);
      snd_report_result(cp->state, result, name);
      if (cp->edit_ctr > one_edit)
	{
	  if (cp->state->deferred_regions > 0) 
	    sequester_deferred_regions(cp, one_edit - 1);
	  while (cp->edit_ctr > one_edit) backup_edit_list(cp);
	  if (cp->mixes) backup_mix_list(cp, one_edit);
	}
    }
}

typedef struct {int key; int state; int args; XEN func; int extended; char *origin;} key_entry;
static key_entry *user_keymap = NULL;
static int keymap_size = 0;
static int keymap_top = 0;

static int in_user_keymap(int key, int state, int extended)
{
  int i;
  if (keymap_top == 0) return(-1);
  for (i = 0; i < keymap_top; i++)
    if ((user_keymap[i].key == key) && 
	(user_keymap[i].state == state) && 
	(user_keymap[i].extended == extended) && 
	(XEN_BOUND_P(user_keymap[i].func)))
      return(i);
  return(-1);
}

static XEN g_key_binding(XEN key, XEN state, XEN extended)
{
  #define H_key_binding "(" S_key_binding " key state extended) -> function bound to this key"
  int i;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(key), key, XEN_ARG_1, S_key_binding, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(state), state, XEN_ARG_2, S_key_binding, "an integer");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(extended), extended, XEN_ARG_3, S_key_binding, "a boolean");
  i = in_user_keymap(XEN_TO_SMALL_C_INT(key),
		     XEN_TO_SMALL_C_INT(state),
		     (XEN_TRUE_P(extended)) ? 1 : 0);
  if (i >= 0) 
    return(user_keymap[i].func);
  return(XEN_UNDEFINED);
}

static void set_keymap_entry(int key, int state, int args, XEN func, int extended, char *origin)
{
  int i;
  i = in_user_keymap(key, state, extended);
  if (i == -1)
    {
      if (keymap_size == keymap_top)
	{
	  keymap_size += 16;
	  if (keymap_top == 0)
	    {
	      user_keymap = (key_entry *)CALLOC(keymap_size, sizeof(key_entry));
	      for (i = 0; i < keymap_size; i++) user_keymap[i].func = XEN_UNDEFINED;
	    }
	  else 
	    {
	      user_keymap = (key_entry *)REALLOC(user_keymap, keymap_size * sizeof(key_entry));
	      for (i = keymap_top; i < keymap_size; i++) 
		{
		  user_keymap[i].key = 0; 
		  user_keymap[i].state = 0; 
		  user_keymap[i].func = XEN_UNDEFINED;
		  user_keymap[i].extended = 0;
		  user_keymap[i].origin = NULL;
		}
	    }
	}
      user_keymap[keymap_top].key = key;
      user_keymap[keymap_top].state = state;
      user_keymap[keymap_top].extended = extended;
      check_menu_labels(key, state, extended);
      i = keymap_top;
      keymap_top++;
    }
  else
    {
      if (XEN_PROCEDURE_P(user_keymap[i].func))
	snd_unprotect(user_keymap[i].func);
      if (user_keymap[i].origin)
	{
	  FREE(user_keymap[i].origin);
	  user_keymap[i].origin = NULL;
	}
    }
  user_keymap[i].origin = copy_string(origin);
  user_keymap[i].args = args;
  user_keymap[i].func = func;
  if (XEN_PROCEDURE_P(func)) snd_protect(func);
}

static void call_user_keymap(int hashedsym, int count)
{
  int res = KEYBOARD_NO_ACTION;
  /* if guile call the associated scheme code, else see if basic string parser can handle it */
  if (XEN_BOUND_P(user_keymap[hashedsym].func))
    {
      /* not _NO_CATCH here because the code is not protected at any higher level */
      if (user_keymap[hashedsym].args == 0)
	res = XEN_TO_C_INT_OR_ELSE(XEN_CALL_0(user_keymap[hashedsym].func, 
					      user_keymap[hashedsym].origin), 
				   KEYBOARD_NO_ACTION);
      else res = XEN_TO_C_INT_OR_ELSE(XEN_CALL_1(user_keymap[hashedsym].func, 
						 C_TO_XEN_INT(count), 
						 user_keymap[hashedsym].origin),
				      KEYBOARD_NO_ACTION);
    }
  handle_cursor(selected_channel(get_global_state()), res);
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
  if (!(sp->active)) return;
  len = snd_strlen(format) + PRINT_BUFFER_SIZE;
  buf = (char *)CALLOC(len, sizeof(char));
  va_start(ap, format);
#if HAVE_VSNPRINTF
  vsnprintf(buf, len, format, ap);
#else
  vsprintf(buf, format, ap);
#endif
  va_end(ap);
#else
  len = snd_strlen(format) + PRINT_BUFFER_SIZE;
  buf = (char *)CALLOC(len, sizeof(char));
#if HAVE_SNPRINTF
  snprintf(buf, len, "%s...[you need vprintf]", format);
#else
  sprintf(buf, "%s...[you need vprintf]", format);
#endif
#endif
  set_minibuffer_string(sp, buf);
  sp->minibuffer_on = MINI_REPORT;
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
  if (!(sp->active)) return;
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
  sp->minibuffer_on = MINI_REPORT;
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
  sp->marking = 0;
  sp->filing = NOT_FILING;
  sp->printing = 0;
  sp->minibuffer_on = MINI_OFF;
  sp->loading = 0;
  sp->amping = 0;
  sp->macroing = 0;
  sp->prompting = 0;
}

void clear_minibuffer_prompt(snd_info *sp)
{
  make_minibuffer_label(sp, "     ");
}

static void prompt(snd_info *sp, char *msg, char *preload)
{
  if (preload)
    {
      set_minibuffer_string(sp, preload);
      set_minibuffer_cursor_position(sp, snd_strlen(preload));
    }
  else
    set_minibuffer_string(sp, NULL);
  make_minibuffer_label(sp, msg);
  sp->minibuffer_on = MINI_PROMPT;
  goto_minibuffer(sp);
}

static int region_count = 0;
static void get_amp_expression(snd_info *sp, int count, int regexpr) 
{
  prompt(sp, "env:", NULL); 
  sp->amping = count; 
  sp->reging = regexpr;
}

static void prompt_named_mark(chan_info *cp) 
{
  snd_info *sp = cp->sound;
  clear_minibuffer(sp);
  make_minibuffer_label(sp, "mark:");
  sp->minibuffer_on = MINI_PROMPT;
  goto_minibuffer(sp);
  sp->marking = cp->cursor + 1; /*  + 1 so it's not confused with 0 (if (sp->marking)...) */
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

static chan_info *goto_next_graph (chan_info *cp, int count);

static chan_info *goto_previous_graph (chan_info *cp, int count)
{
  snd_info *sp;
  snd_state *ss;
  chan_info *ncp, *vcp;
  int i, k, j, chan;
  if (count == 0) return(cp);
  sp = cp->sound;
  ss = cp->state;
  vcp = virtual_selected_channel(cp);
  chan = vcp->chan;
  ncp = NULL;
  if (count < 0) 
    k = -count; 
  else return(goto_next_graph(cp, count)); 
  if (chan > 0)
    {
      /* goto previous channel in current sound */
      k -= chan;
      if (k <= 0)
	ncp = sp->chans[chan + count];
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
	      ncp = sp->chans[sp->nchans - j];
	    break;
	  }
      if (k > 0)
	for (i = ss->max_sounds - 1; i >= sp->index; i--)
	  if (snd_ok(ss->sounds[i]))
	    {
	      sp = (snd_info *)(ss->sounds[i]);
	      j = k;
	      k -= sp->nchans;
	      if (k <= 0)
		ncp = sp->chans[sp->nchans - j];
	      break;
	    }
    }
  if (ncp == vcp) return(ncp);
  if (!ncp) snd_error("goto previous graph failed!");
  select_channel(ncp->sound, ncp->chan);
  equalize_sound_panes(ss, ncp->sound, ncp, FALSE); /* snd-xsnd.c */
  /* goto_graph(ncp); */
  return(ncp);
}

static chan_info *goto_next_graph (chan_info *cp, int count)
{
  snd_info *sp;
  snd_state *ss;
  chan_info *ncp, *vcp;
  int i, k, j, chan;
  if (count == 0) return(cp);
  sp = cp->sound;
  ss = cp->state;
  vcp = virtual_selected_channel(cp);
  chan = vcp->chan;
  ncp = NULL;
  if (count < 0) 
    return(goto_previous_graph(cp, count)); 
  k = count;
  if (chan < (sp->nchans - 1))
    {
      /* goto next channel in current sound */
      k -= (sp->nchans-chan - 1);
      if (k <= 0)
	ncp = sp->chans[chan + count];
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
	      ncp = sp->chans[j - 1];
	    break;
	  }
      if (k > 0)
	for (i = 0; i <= sp->index; i++)
	  if (snd_ok(ss->sounds[i]))
	    {
	      sp = (snd_info *)(ss->sounds[i]);
	      j = k;
	      k -= sp->nchans;
	      if (k <= 0)
		ncp = sp->chans[j - 1];
	      break;
	    }
    }
  if (ncp == vcp) return(ncp);
  if (!ncp) snd_error("goto next graph failed!");
  select_channel(ncp->sound, ncp->chan);
  equalize_sound_panes(ss, ncp->sound, ncp, FALSE);
  /* goto_graph(ncp); */
  return(ncp);
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
  int nc, i, j;
  off_t len;
  chan_info *active_chan;
  char *str = NULL, *mcf = NULL;
  char *tok, *newdir, *str1;
  env *e;
  mark *m;
#if HAVE_OPENDIR
  DIR *dp;
#endif
  XEN proc;
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

#if HAVE_EXTENSION_LANGUAGE
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
	      if (sp->search_expr) FREE(sp->search_expr);
	      sp->search_expr = copy_string(str);
	      if (XEN_PROCEDURE_P(sp->search_proc))
		snd_unprotect(sp->search_proc);
	      sp->search_proc = XEN_UNDEFINED;
	      proc = snd_catch_any(eval_str_wrapper, str, str);
	      if (procedure_ok_with_error(proc, 1, "find", "find", 1))
		{
		  sp->search_proc = proc;
		  snd_protect(proc);
 		}
	      free(str);
	      if (active_chan) active_chan->last_search_result = SEARCH_OK;
	    }
	}
      if (active_chan)
	cursor_search(active_chan, sp->searching);
      return;
    }
#endif
  
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
	  goto_named_mark(active_chan, str);
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
	      nsp = snd_open_file(str, ss, FALSE); /* will post error if any */
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
		save_selection(ss, str1, MUS_NEXT, MUS_OUT_FORMAT, SND_SRATE(sp), NULL, SAVE_ALL_CHANS);
	      else save_region(ss, region_count, str1, MUS_OUT_FORMAT);
	      clear_minibuffer(sp);
	      FREE(str1);
	      break;
	    case CHANNEL_FILING:
	      save_channel_edits(active_chan, mcf = mus_expand_filename(str), C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), "C-x C-w", 0);
	      if (mcf) FREE(mcf);
	      clear_minibuffer(sp);
	      break;
#if HAVE_OPENDIR
	    case TEMP_FILING:
	      newdir = copy_string(str);
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
		  if (tok) FREE(tok);
		}
	      break;
#endif
	    case CHANGE_FILING:
	      mix_complete_file_at_cursor(sp, str, "C-x C-q", with_mix_tags(ss));
	      break;
	    case INSERT_FILING:
	      str1 = mus_expand_filename(str);
	      nc = mus_sound_chans(str1);
	      if (nc != -1)
		{
		  len = mus_sound_frames(str1);
		  if (len == 0)
		    report_in_minibuffer(sp, "%s has no data", str);
		  else
		    {
		      if (!active_chan) active_chan = sp->chans[0];
		      for (i = active_chan->chan, j = 0; (j < nc) && (i < sp->nchans); i++, j++)
			{
			  file_insert_samples(active_chan->cursor, len, str1, sp->chans[i], j, DONT_DELETE_ME, "C-x C-i", sp->chans[i]->edit_ctr);
			  update_graph(sp->chans[i]);
			}
		      clear_minibuffer(sp);
		    }
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
			  (char *)((sp->reging) ? "C-x a" : "C-x C-a"), NULL,
			  C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0, 1.0);
	      else apply_env(active_chan, e, 0, CURRENT_SAMPLES(active_chan), 1.0, 
			     sp->reging, NOT_FROM_ENVED,
			     (char *)((sp->reging) ? "C-x a" : "C-x C-a"), NULL,
			     C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0, 1.0);
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
	  if (sp == NULL) return;
	  sp->macroing = 0;
	  if (str) free(str);
	  return;
	}
    }
#if HAVE_EXTENSION_LANGUAGE
  /* strlen can be 0 here if <cr> response to prompt */
  if (sp->prompting)
    {
      if (snd_strlen(str) > 0)
	{
	  if (sp->raw_prompt)
	    proc = C_TO_XEN_STRING(str);
	  else proc = snd_catch_any(eval_str_wrapper, str, str);
	  if (XEN_PROCEDURE_P(sp->prompt_callback))
	    {
	      snd_protect(proc);
	      XEN_CALL_1(sp->prompt_callback, proc, "prompt callback func");
	      snd_unprotect(proc);
	    }
	  free(str);
	}
      sp->prompting = 0;
      clear_minibuffer(sp);
      return;
    }
#endif
  if (snd_strlen(str) > 0)
    {
      snd_eval_str(ss, str);
      sp->reging = 0;
    }
  else clear_minibuffer(sp);
}

static void stop_fft_in_progress(chan_info *cp)
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
}

static void cursor_moveto_end(chan_info *cp)
{
  cursor_moveto(cp, CURRENT_SAMPLES(cp) - 1);
}

static void set_window_bounds(chan_info *cp, int count) 
{
  /* count = sample number to start at */
  axis_info *ap;
  double sx;
  ap = cp->axis;
  sx = (((double)count / (double)SND_SRATE(cp->sound)) - ap->xmin) / ap->x_ambit;
  reset_x_display(cp, sx, ap->zx);
}

static void set_window_size(chan_info *cp, int count) 
{
  /* set samples within window */
  axis_info *ap;
  double zx;
  ap = cp->axis;
  zx = ((double)count / (((double)SND_SRATE(cp->sound)) * ap->x_ambit));
  reset_x_display(cp, ap->sx, zx);
}

static void set_window_percentage(chan_info *cp, int count) 
{
  /* set percentage of file within window */
  axis_info *ap;
  double zx;
  ap = cp->axis;
  zx = (double)count / (double)SND_SRATE(cp->sound);
  reset_x_display(cp, ap->sx, zx);
}

static void window_frames_selection(chan_info *cp)
{
  double x0, x1;
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
	  (sp->sync != (cp->sound->sync)))
	set_x_axis_x0x1(sp->chans[0], x0, x1);
    }
}



static off_t get_count_1(char *number_buffer, int number_ctr, int dot_seen, chan_info *cp)
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
      return((off_t)(f * SND_SRATE(cp->sound)));
    }
  else
    {
      sscanf(number_buffer, "%d", &i);
      return(i);
    }
  return(1);
}

static off_t get_count(char *number_buffer, int number_ctr, int dot_seen, chan_info *cp, int mark_wise)
{
  off_t val, old_cursor;
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
	 (keysym == snd_K_H) || (keysym == snd_K_h) ||
	 (keysym == snd_K_Y) || (keysym == snd_K_y));
}

static char *key_to_name(int keysym) {if (keysym) return(KEY_TO_NAME(keysym)); else return("NUL");}

#define NO_CX_ARG_SPECIFIED -1

static int number_ctr = 0;
static int dot_seen = 0;
static int counting = 0;
static int extended_mode = 0;

void control_g(snd_state *ss, snd_info *sp)
{
  number_ctr = 0; 
  counting = 0; 
  dot_seen = 0; 
  extended_mode = 0;
  deactivate_selection();
  defining_macro = 0;
  clear_stdin();
  if ((ss->checking_explicitly) || (play_in_progress())) ss->stopped_explicitly = 1; 
  /* this tries to break out of long filter/src computations (and perhaps others) */
  if (sp)
    {
      if (sp->playing) stop_playing_all_sounds();
      if (sp->applying) stop_applying(sp);
      for_each_sound_chan(sp, stop_fft_in_progress);
      clear_minibuffer(sp);
    }
  ss->error_lock = 0;
}

void keyboard_command (chan_info *cp, int keysym, int state)
{
  /* we can't use the meta bit in some cases because this is trapped at a higher level for the Menu mnemonics */
  /* state here is the kbd bucky-bit state */
  /* keysym has Shift taken into account already (see snd-xchn.c XKeycodeToKeysym, and same snd-xsnd.c) */
  static int u_count = 0;
  static char number_buffer[NUMBER_BUFFER_SIZE];
  static off_t count = 1;
  static int got_count = 0;
  static int m = 0;
  int searching, cursor_searching, hashloc, sync_num, i, clear_search = TRUE;
  off_t loc;
  static off_t ext_count = NO_CX_ARG_SPECIFIED;
  snd_info *sp;
  axis_info *ap;
  snd_state *ss;
  sync_info *si;
  mark *mk = NULL;
  /* fprintf(stderr, "kbd: %x %d, %d %d ", keysym, keysym, state, extended_mode);  */
  if (!cp) return;
  searching = 0;
  cursor_searching = 0;
  /* cp->cursor_on = 1; */
  sp = cp->sound;
  ss = cp->state;
  ap = cp->axis;
  if (keysym >= snd_K_Shift_L) return;
  /* this happens when the user presses Control or Shift etc prior to hitting the actual (modified) key */
  if (defining_macro) continue_macro(keysym, state);
  if (!m) count = 1; else m = 0;
  
  if ((selection_creation_in_progress()) &&
      ((extended_mode) || (stop_selecting(keysym, state))))
    finish_selection_creation();

  if ((counting) && (((keysym < snd_K_0) || (keysym > snd_K_9)) && 
		     ((keysym < snd_keypad_0) || (keysym > snd_keypad_9)) && /* these are in order in both Motif (X11) and gdk */
		     (keysym != snd_K_minus) && 
		     (keysym != snd_K_period) && 
		     (keysym != snd_keypad_Decimal) &&
		     (keysym != snd_K_plus)))
    {
      m = ((u_count) && 
	   ((keysym == snd_K_M) || (keysym == snd_K_m)));
      count = get_count(number_buffer, number_ctr, dot_seen, cp, m);
      got_count = 1;
      number_ctr = 0;
      counting = 0;
      dot_seen = 0;
      if (m) return;
    }
  u_count = 0;
  if ((keysym != snd_K_X) && (keysym != snd_K_x))
    {
      got_count = 0;
      if (count == 0) return;
    }
#if HAVE_EXTENSION_LANGUAGE
  if ((state & snd_MetaMask) && 
      ((keysym == snd_K_X) || (keysym == snd_K_x)))
    {
      /* named macros invoked and saved here */
      ss->mx_sp = sp;
      prompt(sp, "M-x:", NULL);
      sp->macroing = count;
      return;
    }
  hashloc = in_user_keymap(keysym, state, extended_mode);
  if (hashloc != -1)                       /* found user-defined key */
    {
      extended_mode = 0;
      call_user_keymap(hashloc, count);
      return;
    }
#endif
  /* if (sp->minibuffer_temp) clear_minibuffer(sp); */

  if (state & snd_ControlMask)
    {
      if (!extended_mode)
	{
	  /* -------------------------------- C-key -------------------------------- */
	  switch (keysym)
	    {
	    case snd_K_A: case snd_K_a: 
	      cp->cursor_on = 1; 
	      loc = (off_t)(ap->x0 * SND_SRATE(sp)); 
	      if ((loc + 1) == ap->losamp) loc = ap->losamp; /* handle dumb rounding problem */
	      cursor_moveto(cp, loc); 
	      break;
	    case snd_K_B: case snd_K_b: 
	      cp->cursor_on = 1; 
	      cursor_move(cp, -count); 
	      break;
	    case snd_K_D: case snd_K_d: 
	      cp->cursor_on = 1; 
	      cursor_delete(cp, count, "C-d"); 
	      break;
	    case snd_K_E: case snd_K_e:
	      cp->cursor_on = 1; 
	      loc = (off_t)(ap->x1 * (double)SND_SRATE(sp));
	      if ((loc + 1) == ap->hisamp) loc = ap->hisamp;
	      cursor_moveto(cp, loc); 
	      break;
	    case snd_K_F: case snd_K_f:
	      cp->cursor_on = 1; 
	      cursor_move(cp, count); 
	      break;
	    case snd_K_G: case snd_K_g: 
	      if (state & snd_MetaMask)
		clear_listener();
	      else control_g(ss, sp);
	      break;
	    case snd_K_H: case snd_K_h: 
	      cp->cursor_on = 1; 
	      cursor_delete(cp, -count, "C-h"); 
	      break; 
	    case snd_K_I: case snd_K_i: 
	      show_cursor_info(cp); 
	      searching = 1; 
	      break;
	    case snd_K_J: case snd_K_j: 
	      cp->cursor_on = 1; 
	      goto_mark(cp, count); 
	      break;
	    case snd_K_K: case snd_K_k: 
	      cp->cursor_on = 1; 
	      cursor_delete(cp, count * 128, "C-k");
	      break;
	    case snd_K_L: case snd_K_l: 
	      cp->cursor_on = 1; 
	      handle_cursor(cp, CURSOR_IN_MIDDLE);
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
		  ((cp->sound)->sync != 0))
		{
		  sync_num = mark_sync_max() + 1; 
		  if (mk) set_mark_sync(mk, sync_num);
		  si = snd_sync(cp->state, (cp->sound)->sync);
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
	      cursor_move(cp, count * 128); 
	      break;
	    case snd_K_O: case snd_K_o: 
	      cp->cursor_on = 1; 
	      cursor_insert(cp, cp->cursor, count, "C-o"); 
	      break;
	    case snd_K_P: case snd_K_p: 
	      cp->cursor_on = 1; 
	      cursor_move(cp, -count * 128); 
	      break;
	    case snd_K_Q: case snd_K_q: 
	      play_channel(cp, cp->cursor, NO_END_SPECIFIED, TRUE, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), "C-q", 0);
	      set_play_button(sp, 1); 
	      break;
#if HAVE_EXTENSION_LANGUAGE
	    case snd_K_R: case snd_K_r: 
	      cp->cursor_on = 1; 
	      cursor_search(cp, -count); 
	      searching = 1; 
	      cursor_searching = 1; 
	      break;
	    case snd_K_S: case snd_K_s: 
	      cp->cursor_on = 1; 
	      cursor_search(cp, count); 
	      searching = 1; 
	      cursor_searching = 1; 
	      break;
#endif
	    case snd_K_T: case snd_K_t: 
	      stop_playing_sound(sp); 
	      set_play_button(sp, 0);
	      break;
	    case snd_K_U: case snd_K_u: 
	      counting = 1; 
	      u_count = 1;
	      number_ctr = 0; 
	      dot_seen = 0; 
	      break;
	    case snd_K_V: case snd_K_v:
	      cp->cursor_on = 1;
	      /* in emacs this is move ahead one window, but for some reason in Snd it's center cursor?? */
	      cursor_moveto(cp, (off_t)((ap->losamp + ap->hisamp) / 2));
	      break;
	    case snd_K_W: case snd_K_w: 
	      delete_selection("C-w", UPDATE_DISPLAY); 
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
	      if (got_count)
		paste_region(count, cp, "C-y"); 
	      else paste_region(stack_position_to_id(0), cp, "C-y");
	      break;
	    case snd_K_Z: case snd_K_z: 
	      cp->cursor_on = 1; 
	      cursor_zeros(cp, count, 0); 
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
	    case snd_K_0: case snd_K_1: case snd_K_2: case snd_K_3: case snd_K_4:
	    case snd_K_5: case snd_K_6: case snd_K_7: case snd_K_8: case snd_K_9: 
	      counting = 1;
	      number_buffer[number_ctr] = (char)('0' + keysym - snd_K_0); 
	      if (number_ctr < (NUMBER_BUFFER_SIZE - 2)) 
		number_ctr++; 
	      /* there is also the bare-number case below */
	      break;
	    case snd_keypad_0: case snd_keypad_1: case snd_keypad_2: case snd_keypad_3: case snd_keypad_4:
	    case snd_keypad_5: case snd_keypad_6: case snd_keypad_7: case snd_keypad_8: case snd_keypad_9: 
	      counting = 1;
	      number_buffer[number_ctr] = (char)('0' + keysym - snd_keypad_0); 
	      if (number_ctr < (NUMBER_BUFFER_SIZE - 2)) 
		number_ctr++; 
	      /* there is also the bare-number case below */
	      break;
	    case snd_K_space: 
	      if (count > 0)
		{
		  start_selection_creation(cp, cp->cursor);
		  report_in_minibuffer(sp, "selection starts at " OFF_TD, cp->cursor);
		  clear_search = FALSE;
		}
	      break;
	    case snd_K_period:
	    case snd_keypad_Decimal:
	      counting = 1; 
	      number_buffer[number_ctr] = '.'; 
	      number_ctr++; 
	      dot_seen = 1; 
	      break;
	    case snd_K_greater: 
	      cp->cursor_on = 1; 
	      cursor_moveto_end(cp); 
	      break;
	    case snd_K_less: 
	      cp->cursor_on = 1; 
	      cursor_moveto(cp ,0); 
	      break;
	    case snd_K_minus: 
	      counting = 1; 
	      number_ctr = 1; 
	      number_buffer[0] = '-'; 
	      break;
	    case snd_K_underscore: 
	      undo_edit_with_sync(cp, count); 
	      break;
	    case snd_keypad_Left: 
	      set_spectro_y_angle(ss, spectro_y_angle(ss) - 1.0);
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_Right: 
	      set_spectro_y_angle(ss, spectro_y_angle(ss) + 1.0);
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_Down: 
	      set_spectro_x_angle(ss, spectro_x_angle(ss) - 1.0);
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_Up: 
	      set_spectro_x_angle(ss, spectro_x_angle(ss) + 1.0);
	      reflect_spectro(ss); 
	      break;
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
	      break;
	    case snd_K_B: case snd_K_b: 
	      set_window_bounds(cp, ext_count); 
	      break;
	    case snd_K_C: case snd_K_c: 
	      sound_hide_ctrls(sp); 
	      break;
	    case snd_K_D: case snd_K_d: 
	      prompt(sp, "eps file:", NULL); 
	      sp->printing = ext_count; 
	      searching = 1; 
	      break;
	    case snd_K_E: case snd_K_e: 
	      if (macro_size == 0)
		report_in_minibuffer(sp, "no macro active?");
	      else
		{
		  prompt(sp, "macro name:", NULL); 
		  sp->filing = MACRO_FILING; 
		  searching = 1; 
		}
	      break;
	    case snd_K_F: case snd_K_f: 
	      prompt(sp, "file:", NULL); 
	      sp->filing = INPUT_FILING; 
	      searching = 1; 
	      break;
	    case snd_K_G: case snd_K_g: 
	      control_g(ss, sp);
	      break;
	    case snd_K_I: case snd_K_i: 
	      prompt(sp, "insert file:", NULL); 
	      sp->filing = INSERT_FILING; 
	      searching = 1; 
	      break;
	    case snd_K_J: case snd_K_j:
	      cp->cursor_on = 1; 
	      goto_mix(cp, ext_count); 
	      break;
	    case snd_K_L: case snd_K_l: 
	      prompt(sp, "load:", NULL); 
	      sp->loading = 1;
	      searching = 1; 
	      break;
	    case snd_K_M: case snd_K_m:
	      cp->cursor_on = 1; 
	      prompt_named_mark(cp);
	      set_show_marks(ss, 1); 
	      searching = 1; 
	      break;
	    case snd_K_O: case snd_K_o: 
	      sound_show_ctrls(sp); 
	      break;
	    case snd_K_P: case snd_K_p: 
	      set_window_size(cp, ext_count); 
	      break;
	    case snd_K_Q: case snd_K_q: 
	      prompt(sp, "mix file:", NULL); 
	      sp->filing = CHANGE_FILING; 
	      searching = 1; 
	      break;
	    case snd_K_R: case snd_K_r: 
	      redo_edit_with_sync(cp, ext_count); 
	      break;
	    case snd_K_S: case snd_K_s: 
	      save_edits(sp, NULL); 
	      break;
	    case snd_K_T: case snd_K_t: 
	      stop_playing_sound(sp); 
	      break;
	    case snd_K_U: case snd_K_u: 
	      undo_edit_with_sync(cp, ext_count); 
	      break;
	    case snd_K_V: case snd_K_v:
	      set_window_percentage(cp, ext_count);
	      break;
	    case snd_K_W: case snd_K_w: 
	      prompt(sp, "file:", NULL); 
	      sp->filing = CHANNEL_FILING; 
	      searching = 1; 
	      break;
	    case snd_K_Z: case snd_K_z: 
	      cp->cursor_on = 1; 
	      cos_smooth(cp, cp->cursor, ext_count, 0, "C-x C-z"); 
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
	      number_buffer[number_ctr] = (char)('0' + keysym-snd_K_0); 
	      if (number_ctr < (NUMBER_BUFFER_SIZE - 2)) 
		number_ctr++; 
	      break;
	    case snd_keypad_0: case snd_keypad_1: case snd_keypad_2: case snd_keypad_3: case snd_keypad_4:
	    case snd_keypad_5: case snd_keypad_6: case snd_keypad_7: case snd_keypad_8: case snd_keypad_9: 
	      counting = 1;
	      number_buffer[number_ctr] = (char)('0' + keysym - snd_keypad_0); 
	      if (number_ctr < (NUMBER_BUFFER_SIZE - 2)) 
		number_ctr++; 
	      break;
	    case snd_K_period: 
	    case snd_keypad_Decimal:
	      counting = 1; 
	      number_buffer[number_ctr] = '.'; 
	      number_ctr++; 
	      dot_seen = 1; 
	      break;
	    case snd_K_greater: 
	      cp->cursor_on = 1; 
	      cursor_moveto_end(cp); 
	      break;
	    case snd_K_less: 
	      cp->cursor_on = 1; 
	      cursor_moveto(cp, 0); 
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
	      sp = snd_update(ss, sp); 
	      break;
	    case snd_K_space: 
	      if (play_in_progress())
		toggle_dac_pausing(ss); 
	      else deactivate_selection();
	      break;

	    case snd_keypad_Up:
	      set_spectro_z_scale(ss, spectro_z_scale(ss) + .01);
	      reflect_spectro(ss);
	      break;
	    case snd_keypad_Down:
	      set_spectro_z_scale(ss, spectro_z_scale(ss) - .01);
	      reflect_spectro(ss);
	      break;
	    case snd_keypad_Left:
	      set_spectro_z_angle(ss, spectro_z_angle(ss) - 1.0);
	      reflect_spectro(ss);
	      break;
	    case snd_keypad_Right:
	      set_spectro_z_angle(ss, spectro_z_angle(ss) + 1.0);
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_Add:
	      if (time_graph_type(ss) == GRAPH_AS_WAVOGRAM) 
		set_wavo_trace(ss, wavo_trace(ss) + 1); 
	      else set_spectro_hop(ss, spectro_hop(ss) + 1);
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_Subtract: 
	      if (time_graph_type(ss) == GRAPH_AS_WAVOGRAM) 
		{
		  if (wavo_trace(ss)>1) 
		    set_wavo_trace(ss, wavo_trace(ss) - 1);
		} 
	      else 
		{
		  if (spectro_hop(ss)>1) 
		    set_spectro_hop(ss, spectro_hop(ss) - 1);
		}
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_Multiply: 
	      set_transform_size(ss, transform_size(ss) * 2); 
	      break;
	    case snd_keypad_Divide: 
	      if (transform_size(ss) > 4) 
		set_transform_size(ss, transform_size(ss) / 2); 
	      break;
	    case snd_keypad_Delete:
	      set_dot_size(ss, dot_size(ss) + 1); 
	      break;
	    case snd_keypad_Insert:
	      if (dot_size(ss) > 1) 
		set_dot_size(ss, dot_size(ss) - 1); 
	      break;
	    case snd_keypad_PageDown:
	      set_spectro_cutoff(ss, spectro_cutoff(ss) * .95); 
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_PageUp:
	      if (spectro_cutoff(ss) < 1.0) 
		set_spectro_cutoff(ss, spectro_cutoff(ss) / .95); 
	      reflect_spectro(ss); 
	      break;
	    case snd_keypad_Enter: 
	      reset_spectro(ss); 
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
		return;
	      /* it might be better to remove the menu accelerators -- they are a dubious feature to begin with */
#endif
#if HAVE_EXTENSION_LANGUAGE
	      {
		char buf[2];
		buf[0] = keysym; 
		buf[1] = 0;
		if (listener_height() > 5)
		  {
		    goto_listener();
		    listener_append(buf);
		  }
		else 
		  {
		    ss->mx_sp = sp;
		    prompt(sp, "M-x:", buf);
		    sp->macroing = count;
		    clear_search = FALSE;
		  }
	      }
#else
	      report_in_minibuffer(sp, "%s%s undefined", (state & snd_MetaMask) ? "M-" : "", key_to_name(keysym));
#endif
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
		} 
	      else no_selection_error(sp); 
	      break;
	    case snd_K_B: case snd_K_b: 
	      cp->cursor_on = 1; 
	      handle_cursor(cp, CURSOR_ON_LEFT);
	      break;
	    case snd_K_C: case snd_K_c: 
	      mark_define_region(cp, (ext_count == NO_CX_ARG_SPECIFIED) ? 1 : ext_count); 
	      break;
	    case snd_K_D: case snd_K_d: 
	      prompt(sp, "temp dir:", NULL); 
	      sp->filing = TEMP_FILING; 
	      searching = 1; 
	      break;
	    case snd_K_E: case snd_K_e: 
	      execute_last_macro(cp, (ext_count == NO_CX_ARG_SPECIFIED) ? 1 : ext_count);
	      handle_cursor(cp, cursor_decision(cp));
	      break;
	    case snd_K_F: case snd_K_f: 
	      cp->cursor_on = 1; 
	      handle_cursor(cp, CURSOR_ON_RIGHT); 
	      break;
	    case snd_K_I: case snd_K_i: 
	      insert_selection_or_region(ss, (ext_count == NO_CX_ARG_SPECIFIED) ? 0 : ext_count, cp, "C-x i");
	      break;
	    case snd_K_J: case snd_K_j: 
	      prompt(sp, "mark:", NULL); 
	      sp->finding_mark = 1; 
	      searching = 1; 
	      break;
	    case snd_K_K: case snd_K_k: 
	      snd_close_file(sp, ss); 
	      break;
	    case snd_K_L: case snd_K_l: 
	      cp->cursor_on = 1;
	      if (selection_is_active_in_channel(cp))
		cursor_moveto(cp, (off_t)(selection_beg(cp) + 0.5 * selection_len()));
	      else no_selection_error(sp); 
	      handle_cursor(cp, CURSOR_IN_MIDDLE);
	      break;
	    case snd_K_O: case snd_K_o: 
	      if (ext_count > 0) 
		goto_next_graph(cp, ext_count); 
	      else goto_previous_graph(cp, ext_count); 
	      break;
	    case snd_K_P: case snd_K_p: 
	      if (ext_count == NO_CX_ARG_SPECIFIED)
		play_selection(IN_BACKGROUND, C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), "C-x p", 0);
	      else play_region(ss, ext_count, IN_BACKGROUND);
	      break;
	    case snd_K_Q: case snd_K_q: 
	      add_selection_or_region(ss, (ext_count == NO_CX_ARG_SPECIFIED) ? 0 : ext_count, cp, "C-x q"); 
	      break;
	    case snd_K_R: case snd_K_r: 
	      redo_edit_with_sync(cp, (ext_count == NO_CX_ARG_SPECIFIED) ? 1 : ext_count); 
	      break;
	    case snd_K_U: case snd_K_u: 
	      undo_edit_with_sync(cp, (ext_count == NO_CX_ARG_SPECIFIED) ? 1 : ext_count); 
	      break;
	    case snd_K_V: case snd_K_v: 
	      if (selection_is_active_in_channel(cp))
		window_frames_selection(cp); 
	      else no_selection_error(sp); 
	      break;
	    case snd_K_W: case snd_K_w:
	      region_count = ((ext_count == NO_CX_ARG_SPECIFIED) ? 0 : ext_count);
	      prompt(sp, "file:", NULL); 
	      sp->filing = REGION_FILING; 
	      searching = 1;
	      break;
	    case snd_K_Z: case snd_K_z: 
	      if (selection_is_active_in_channel(cp))
		cos_smooth(cp, cp->cursor, (ext_count == NO_CX_ARG_SPECIFIED) ? 1 : ext_count, 1, "C-x z"); 
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
	      cursor_moveto(cp, 0); 
	      break;
	    case snd_K_greater: 
	      cp->cursor_on = 1; 
	      cursor_moveto_end(cp);
	      break;
	    case snd_K_openparen:
	      if (defining_macro) 
		report_in_minibuffer(sp, "macro definition already in progress");
	      else
		{
		  start_defining_macro(); 
		  report_in_minibuffer(sp, "defining macro..."); 
		}
	      clear_search = FALSE; 
	      break;
	    case snd_K_closeparen: 
	      if (defining_macro)
		{
		  stop_defining_macro(); 
		  clear_minibuffer(sp); 
		}
	      clear_search = FALSE;
	      break;
	    case snd_K_slash: 
	      cp->cursor_on = 1;
	      prompt_named_mark(cp); 
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
  if (clear_search)
    {
      if ((sp->minibuffer_on == MINI_FIND) && (!searching)) 
	clear_minibuffer(sp);
      else 
	if (!cursor_searching) 
	  sp->searching = 0;
    }
}


static XEN g_bind_key(XEN key, XEN state, XEN code, XEN extended, XEN origin)
{
  #define H_bind_key "(" S_bind_key " key modifiers func (extended #f) (origin \"user key func\")\n\
causes 'key' (an integer) \
when typed with 'modifiers' (1:shift, 4:control, 8:meta) (and C-x if extended) to invoke 'func', a function of \
zero or one arguments. If the function takes one argument, it is passed the preceding C-u number, if any. \
The function should return one of the cursor choices (e.g. cursor-no-action).  'origin' is \
the name reported if an error occurs."

  int args;
  char *errstr;
  XEN errmsg;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(key), key, XEN_ARG_1, S_bind_key, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(state), state, XEN_ARG_2, S_bind_key, "an integer");
  XEN_ASSERT_TYPE((XEN_FALSE_P(code) || XEN_PROCEDURE_P(code)), code, XEN_ARG_3, S_bind_key, "#f or a procedure");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(extended), extended, XEN_ARG_4, S_bind_key, "a boolean");
  if (XEN_FALSE_P(code))
    set_keymap_entry(XEN_TO_C_INT(key), 
		     XEN_TO_C_INT(state), 
		     0,
		     XEN_UNDEFINED,
		     0,
		     NULL);
  else 
    {
      args = XEN_REQUIRED_ARGS(code);
      if (args > 1)
	{
	  errstr = mus_format("bind-key function arg should take either zero or one args, not %d", args);
	  errmsg = C_TO_XEN_STRING(errstr);
	  FREE(errstr);
	  return(snd_bad_arity_error(S_bind_key, 
				     errmsg,
				     code));
	}
      set_keymap_entry(XEN_TO_C_INT(key), 
		       XEN_TO_C_INT(state), 
		       args, 
		       code,
		       (XEN_TRUE_P(extended)) ? 1 : 0,
		       (XEN_STRING_P(origin)) ? XEN_TO_C_STRING(origin) : (char *)("user key func"));
    }
  return(code);
}

static XEN g_unbind_key(XEN key, XEN state, XEN extended)
{
  #define H_unbind_key "(" S_unbind_key " key state &optional extended) undoes the effect of a prior bind-key call."
  return(g_bind_key(key, state, XEN_FALSE, extended, XEN_UNDEFINED));
}

static XEN g_key(XEN kbd, XEN buckybits, XEN snd, XEN chn)
{
  #define H_key "(" S_key " key modifiers &optional snd chn) simulates typing 'key' with 'modifiers' in snd's channel chn"
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(kbd), kbd, XEN_ARG_1, S_key, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(buckybits), buckybits, XEN_ARG_2, S_key, "an integer");
  ASSERT_CHANNEL(S_key, snd, chn, 3);
  cp = get_cp(snd, chn, S_key);
  keyboard_command(cp, XEN_TO_C_INT(kbd), XEN_TO_C_INT(buckybits));
  return(kbd);
}

static XEN g_save_macros(void) 
{
  #define H_save_macros "(" S_save_macros ") saves keyboard macros in Snd's init file (.snd)"
  FILE *fd = NULL;
  snd_state *ss;
  ss = get_global_state();
  fd = open_snd_init_file(ss);
  if (fd) save_macro_state(fd);
  if ((!fd) || (FCLOSE(fd) != 0))
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_save_macros),
			 C_TO_XEN_STRING(ss->init_file),
			 C_TO_XEN_STRING(strerror(errno))));
  return(C_TO_XEN_STRING(ss->init_file));
}

static XEN g_prompt_in_minibuffer(XEN msg, XEN callback, XEN snd_n, XEN raw)
{
  #define H_prompt_in_minibuffer "(" S_prompt_in_minibuffer " msg callback &optional snd (raw #f)) posts msg in snd's minibuffer \
then when the user eventually responds, invokes the function callback with the response.  If 'raw' is #t, the response is \
returned as a string; otherwise it is evaluated first as Scheme code"

  snd_info *sp;
  char *errstr;
  XEN errmsg;
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, XEN_ARG_1, S_prompt_in_minibuffer, "a string");
  XEN_ASSERT_TYPE((XEN_NOT_BOUND_P(callback)) || (XEN_BOOLEAN_P(callback)) || XEN_PROCEDURE_P(callback), 
		  callback, XEN_ARG_2, S_prompt_in_minibuffer, "#f or a procedure");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(raw), raw, XEN_ARG_4, S_prompt_in_minibuffer, "a boolean");
  ASSERT_SOUND(S_prompt_in_minibuffer, snd_n, 3);
  sp = get_sp(snd_n);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_prompt_in_minibuffer, snd_n));
  if (XEN_PROCEDURE_P(sp->prompt_callback))
    snd_unprotect(sp->prompt_callback);
  sp->prompt_callback = XEN_FALSE; /* just in case something goes awry */
  if (XEN_BOUND_P(raw)) sp->raw_prompt = XEN_TO_C_BOOLEAN(raw); else sp->raw_prompt = 0;
  if (XEN_PROCEDURE_P(callback))
    {
      errstr = procedure_ok(callback, 1, S_prompt_in_minibuffer, "callback", 2);
      if (errstr)
	{
	  errmsg = C_TO_XEN_STRING(errstr);
	  FREE(errstr);
	  return(snd_bad_arity_error(S_prompt_in_minibuffer, 
				     errmsg,
				     callback));
	}
      snd_protect(callback);  
    }
  sp->prompt_callback = callback;
  make_minibuffer_label(sp, XEN_TO_C_STRING(msg));
  sp->minibuffer_on = MINI_USER;
  sp->prompting = 1;
  goto_minibuffer(sp);
  return(callback);
}

static XEN g_report_in_minibuffer(XEN msg, XEN snd_n)
{
  #define H_report_in_minibuffer "(" S_report_in_minibuffer " msg &optional snd) displays msg in snd's minibuffer"
  snd_info *sp;
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, XEN_ARG_1, S_report_in_minibuffer, "a string");
  ASSERT_SOUND(S_report_in_minibuffer, snd_n, 2);
  sp = get_sp(snd_n);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_report_in_minibuffer, snd_n));
  report_in_minibuffer(sp, XEN_TO_C_STRING(msg));
  return(msg);
}

static XEN g_append_to_minibuffer(XEN msg, XEN snd_n)
{
  #define H_append_to_minibuffer "(" S_append_to_minibuffer " msg &optional snd) appends msg to snd's minibuffer"
  snd_info *sp;
  char *str1 = NULL, *expr_str;
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, XEN_ARG_1, S_append_to_minibuffer, "a string");
  ASSERT_SOUND(S_append_to_minibuffer, snd_n, 2);
  sp = get_sp(snd_n);
  if (sp == NULL)
    return(snd_no_such_sound_error(S_append_to_minibuffer, snd_n));
  expr_str = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  mus_snprintf(expr_str, PRINT_BUFFER_SIZE, 
	       "%s%s", 
	       str1 = get_minibuffer_string(sp), 
	       XEN_TO_C_STRING(msg));
  set_minibuffer_string(sp, expr_str);
  FREE(expr_str);
  sp->minibuffer_on = MINI_USER;
  if (str1) free(str1);
  return(msg);
}

static XEN g_forward_graph(XEN count, XEN snd, XEN chn) 
{
  #define H_forward_graph "(" S_forward_graph " &optional (count 1) snd chn) moves the 'selected' graph forward by count"
  int val;
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(count), count, XEN_ARG_1, S_forward_graph, "an integer");
  ASSERT_CHANNEL(S_forward_graph, snd, chn, 2);
  cp = get_cp(snd, chn, S_forward_graph);
  val = XEN_TO_C_INT_OR_ELSE(count, 1);
  cp = goto_next_graph(cp, val);
  return(XEN_LIST_2(C_TO_SMALL_XEN_INT(cp->sound->index),
		    C_TO_SMALL_XEN_INT(cp->chan)));
}

static XEN g_backward_graph(XEN count, XEN snd, XEN chn) 
{
  #define H_backward_graph "(" S_backward_graph " &optional (count 1) snd chn) moves the 'selected' graph back by count"
  int val;
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(count), count, XEN_ARG_1, S_backward_graph, "an integer");
  ASSERT_CHANNEL(S_backward_graph, snd, chn, 2);
  cp = get_cp(snd, chn, S_backward_graph);
  val = -(XEN_TO_C_INT_OR_ELSE(count, 1));
  cp = goto_previous_graph(cp, val);
  return(XEN_LIST_2(C_TO_SMALL_XEN_INT(cp->sound->index),
		    C_TO_SMALL_XEN_INT(cp->chan)));
}

static XEN g_control_g_x(void)
{
  #define H_control_g_x "(" S_c_g_x ") simulates typing C-g"
  snd_state *ss;
  ss = get_global_state();
  control_g(ss, any_selected_sound(ss));
  return(XEN_FALSE);
}

#ifdef XEN_ARGIFY_1
XEN_ARGIFY_3(g_forward_graph_w, g_forward_graph)
XEN_ARGIFY_3(g_backward_graph_w, g_backward_graph)
XEN_ARGIFY_3(g_key_binding_w, g_key_binding)
XEN_ARGIFY_5(g_bind_key_w, g_bind_key)
XEN_ARGIFY_3(g_unbind_key_w, g_unbind_key)
XEN_ARGIFY_4(g_key_w, g_key)
XEN_NARGIFY_0(g_save_macros_w, g_save_macros)
XEN_NARGIFY_0(g_control_g_x_w, g_control_g_x)
XEN_ARGIFY_2(g_report_in_minibuffer_w, g_report_in_minibuffer)
XEN_ARGIFY_4(g_prompt_in_minibuffer_w, g_prompt_in_minibuffer)
XEN_ARGIFY_2(g_append_to_minibuffer_w, g_append_to_minibuffer)
#else
#define g_forward_graph_w g_forward_graph
#define g_backward_graph_w g_backward_graph
#define g_key_binding_w g_key_binding
#define g_bind_key_w g_bind_key
#define g_unbind_key_w g_unbind_key
#define g_key_w g_key
#define g_save_macros_w g_save_macros
#define g_control_g_x_w g_control_g_x
#define g_report_in_minibuffer_w g_report_in_minibuffer
#define g_prompt_in_minibuffer_w g_prompt_in_minibuffer
#define g_append_to_minibuffer_w g_append_to_minibuffer
#endif

void g_init_kbd(void)
{
  XEN_DEFINE_PROCEDURE(S_forward_graph,           g_forward_graph_w, 0, 3, 0,           H_forward_graph);
  XEN_DEFINE_PROCEDURE(S_backward_graph,          g_backward_graph_w, 0, 3, 0,          H_backward_graph);

  XEN_DEFINE_PROCEDURE(S_key_binding,             g_key_binding_w, 2, 1, 0,             H_key_binding);
  XEN_DEFINE_PROCEDURE(S_bind_key,                g_bind_key_w, 3, 2, 0,                H_bind_key);
  XEN_DEFINE_PROCEDURE(S_unbind_key,              g_unbind_key_w, 2, 1, 0,              H_unbind_key);
  XEN_DEFINE_PROCEDURE(S_key,                     g_key_w, 2, 2, 0,                     H_key);
  XEN_DEFINE_PROCEDURE(S_save_macros,             g_save_macros_w, 0, 0, 0,             H_save_macros);
  XEN_DEFINE_PROCEDURE(S_c_g_x,                   g_control_g_x_w, 0, 0, 0,             H_control_g_x);  

  XEN_DEFINE_PROCEDURE(S_report_in_minibuffer,    g_report_in_minibuffer_w, 1, 1, 0,    H_report_in_minibuffer);
  XEN_DEFINE_PROCEDURE(S_prompt_in_minibuffer,    g_prompt_in_minibuffer_w, 1, 3, 0,    H_prompt_in_minibuffer);
  XEN_DEFINE_PROCEDURE(S_append_to_minibuffer,    g_append_to_minibuffer_w, 1, 1, 0,    H_append_to_minibuffer);
}
