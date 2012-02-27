#include "snd.h"


/* -------- Keyboard Macros -------- */

#define MIN_KEY_CODE 0
#define MAX_KEY_CODE 65535
#define MIN_KEY_STATE 0
#define MAX_KEY_STATE 15

static bool defining_macro = false;
static int macro_cmd_size = 0;
static int macro_size = 0;
typedef struct {int keysym; int state;} macro_cmd;
static macro_cmd **macro_cmds = NULL;


static void allocate_macro_cmds(void)
{
  int i, old_size;
  old_size = macro_cmd_size;
  macro_cmd_size += 16;
  if (!macro_cmds)
    macro_cmds = (macro_cmd **)calloc(macro_cmd_size, sizeof(macro_cmd *));
  else 
    {
      macro_cmds = (macro_cmd **)realloc(macro_cmds, macro_cmd_size * sizeof(macro_cmd *));
      for (i = old_size; i < macro_cmd_size; i++) macro_cmds[i] = NULL;
    }
}


static void start_defining_macro(void)
{
  macro_size = 0;
  defining_macro = true;
  if ((!macro_cmds) || (macro_size == macro_cmd_size)) allocate_macro_cmds();
}


static void stop_defining_macro(void)
{
  /* the last C-x ) went into the macro before we noticed it should not have */
  macro_size -= 2;
  defining_macro = false;
}


static void execute_last_macro(chan_info *cp, int count)
{
  int i, j;
  if ((macro_cmds) && (macro_size > 0))
    for (j = 0; j < count; j++)
      for (i = 0; i < macro_size; i++) 
	keyboard_command(cp, 
			 macro_cmds[i]->keysym, 
			 macro_cmds[i]->state);
}


static void continue_macro(int keysym, int state)
{
  if (!(macro_cmds[macro_size])) macro_cmds[macro_size] = (macro_cmd *)calloc(1, sizeof(macro_cmd));
  macro_cmds[macro_size]->keysym = keysym;
  macro_cmds[macro_size]->state = state;
  macro_size++;
  if (macro_size == macro_cmd_size) 
    allocate_macro_cmds();
}




/* ---------------- keys ---------------- */

typedef struct {
  int key; 
  int state; 
  int args; 
  XEN func; 
  bool cx_extended; /* Sun/Forte C defines "extended" somewhere */
  const char *origin, *prefs_info;
  int gc_loc;
} key_entry; 

static key_entry *keymap = NULL;
static int keymap_size = 0;
static int keymap_top = 0;


int in_keymap(int key, int state, bool cx_extended)
{
  int i;
  if (keymap_top == 0) return(-1);
  for (i = 0; i < keymap_top; i++)
    if ((keymap[i].key == key) && 
	(keymap[i].state == state) &&
	(keymap[i].cx_extended == cx_extended) && 
	(XEN_BOUND_P(keymap[i].func)))
      return(i);
  return(-1);
}


#if HAVE_SCHEME
  #define kbd_false 0
#else
  #define kbd_false XEN_FALSE
#endif

#define NUM_BUILT_IN_KEYS 81

static key_entry built_in_keys[NUM_BUILT_IN_KEYS] = {
  {snd_K_Down,       0, 0, kbd_false, false, "zoom out",                                                 NULL, -1},
  {snd_K_Up,         0, 0, kbd_false, false, "zoom in",                                                  NULL, -1},
  {snd_K_Left,       0, 0, kbd_false, false, "move window left",                                         NULL, -1},
  {snd_K_Right,      0, 0, kbd_false, false, "move window right",                                        NULL, -1},
  {snd_keypad_Down,  0, 0, kbd_false, false, "zoom fft out",                                             NULL, -1},
  {snd_keypad_Up,    0, 0, kbd_false, false, "zoom fft in",                                              NULL, -1},
  {snd_keypad_Left,  0, 0, kbd_false, false, "move fft left",                                            NULL, -1},
  {snd_keypad_Right, 0, 0, kbd_false, false, "move fft right",                                           NULL, -1},
  {snd_K_less,       0, 0, kbd_false, false, "move cursor to sample 0",                                  NULL, -1},
  {snd_K_greater,    0, 0, kbd_false, false, "move cursor to last sample",                               NULL, -1},
  {snd_K_space,      0, 0, kbd_false, false, "play from cursor or stop playing",                         NULL, -1},

  {snd_K_less,       snd_ControlMask, 0, kbd_false, false, "move cursor to sample 0",                    NULL, -1},
  {snd_K_greater,    snd_ControlMask, 0, kbd_false, false, "move cursor to last sample",                 NULL, -1},
  {snd_K_a,          snd_ControlMask, 0, kbd_false, false, "move cursor to window start",                NULL, -1},
  {snd_K_b,          snd_ControlMask, 0, kbd_false, false, "move cursor back one pixel",                 NULL, -1},
  {snd_K_d,          snd_ControlMask, 0, kbd_false, false, "delete sample at cursor",                    NULL, -1},
  {snd_K_e,          snd_ControlMask, 0, kbd_false, false, "move cursor to window end",                  NULL, -1},
  {snd_K_f,          snd_ControlMask, 0, kbd_false, false, "move cursor ahead one pixel",                NULL, -1},
  {snd_K_g,          snd_ControlMask, 0, kbd_false, false, "abort current command",                      NULL, -1},
  {snd_K_h,          snd_ControlMask, 0, kbd_false, false, "delete previous sample",                     NULL, -1},
  {snd_K_i,          snd_ControlMask, 0, kbd_false, false, "display cursor info",                        NULL, -1},
  {snd_K_j,          snd_ControlMask, 0, kbd_false, false, "goto mark",                                  NULL, -1},
  {snd_K_k,          snd_ControlMask, 0, kbd_false, false, "delete one line's worth of samples",         NULL, -1},
  {snd_K_l,          snd_ControlMask, 0, kbd_false, false, "position window so cursor is in the middle", NULL, -1},
  {snd_K_m,          snd_ControlMask, 0, kbd_false, false, "place (or remove) mark at cursor location",  NULL, -1},
  {snd_K_n,          snd_ControlMask, 0, kbd_false, false, "move cursor ahead one 'line'",               NULL, -1},
  {snd_K_o,          snd_ControlMask, 0, kbd_false, false, "insert one zero sample at cursor",           NULL, -1},
  {snd_K_p,          snd_ControlMask, 0, kbd_false, false, "move cursor back one 'line'",                NULL, -1},
  {snd_K_q,          snd_ControlMask, 0, kbd_false, false, "play current channel starting at cursor",    "play-channel-from-cursor", -1},
  {snd_K_s,          snd_ControlMask, 0, kbd_false, false, "search forwards",                            NULL, -1},
  {snd_K_t,          snd_ControlMask, 0, kbd_false, false, "stop playing",                               NULL, -1},
  {snd_K_u,          snd_ControlMask, 0, kbd_false, false, "start arg count definition.",                NULL, -1},
  {snd_K_v,          snd_ControlMask, 0, kbd_false, false, "move cursor to mid-window",                  NULL, -1},
  {snd_K_w,          snd_ControlMask, 0, kbd_false, false, "delete selection",                           "delete-selection", -1},
  {snd_K_y,          snd_ControlMask, 0, kbd_false, false, "insert selection.",                          "insert-selection", -1},
  {snd_K_z,          snd_ControlMask, 0, kbd_false, false, "set sample at cursor to 0.0",                NULL, -1},
  {snd_K_underscore, snd_ControlMask, 0, kbd_false, false, "undo",                                       NULL, -1},
  {snd_K_space,      snd_ControlMask, 0, kbd_false, false, "start selection definition",                 NULL, -1},
  {snd_K_g,          snd_ControlMask | snd_MetaMask, 0, kbd_false, false, "clear listener",              NULL, -1},
  {snd_K_less,       snd_MetaMask,    0, kbd_false, false, "move cursor to sample 0",                    NULL, -1},
  {snd_K_greater,    snd_MetaMask,    0, kbd_false, false, "move cursor to last sample",                 NULL, -1},

  {snd_K_b,          0, 0, kbd_false, true, "position window so cursor is on left margin",               NULL, -1},
  {snd_K_c,          0, 0, kbd_false, true, "define selection from cursor to nth mark",                  NULL, -1},
  {snd_K_e,          0, 0, kbd_false, true, "execute keyboard macro",                                    NULL, -1},
  {snd_K_f,          0, 0, kbd_false, true, "position window so cursor is on right margin",              NULL, -1},
  {snd_K_k,          0, 0, kbd_false, true, "close file",                                                NULL, -1},
  {snd_K_l,          0, 0, kbd_false, true, "position selection in mid-view",                            NULL, -1},
  {snd_K_o,          0, 0, kbd_false, true, "move to next or previous graph",                            NULL, -1},
  {snd_K_p,          0, 0, kbd_false, true, "play selection or region n",                                "play-selection", -1},
  {snd_K_q,          0, 0, kbd_false, true, "mix in selection",                                          "mix-selection", -1},
  {snd_K_r,          0, 0, kbd_false, true, "redo",                                                      NULL, -1},
  {snd_K_u,          0, 0, kbd_false, true, "undo",                                                      NULL, -1},
  {snd_K_v,          0, 0, kbd_false, true, "position window over selection",                            NULL, -1},
  {snd_K_z,          0, 0, kbd_false, true, "smooth selection",                                          NULL, -1},
  {snd_K_openparen,  0, 0, kbd_false, true, "begin keyboard macro definition",                           NULL, -1},
  {snd_K_closeparen, 0, 0, kbd_false, true, "end keyboard macro definition",                             NULL, -1},

  {snd_K_b, snd_ControlMask, 0, kbd_false, true, "set x window bounds (preceded by 1 arg)",              NULL, -1},
  {snd_K_c, snd_ControlMask, 0, kbd_false, true, "hide control panel",                                   NULL, -1},
  {snd_K_f, snd_ControlMask, 0, kbd_false, true, "open file",                                            NULL, -1},
  {snd_K_g, snd_ControlMask, 0, kbd_false, true, "abort command",                                        NULL, -1},
  {snd_K_o, snd_ControlMask, 0, kbd_false, true, "show control panel",                                   NULL, -1},
  {snd_K_p, snd_ControlMask, 0, kbd_false, true, "set window size (preceded by 1 arg)",                  NULL, -1},
  {snd_K_q, snd_ControlMask, 0, kbd_false, true, "mix in file",                                          NULL, -1},
  {snd_K_r, snd_ControlMask, 0, kbd_false, true, "redo",                                                 "save-sound", -1},
  {snd_K_s, snd_ControlMask, 0, kbd_false, true, "save file",                                            NULL, -1},
  {snd_K_u, snd_ControlMask, 0, kbd_false, true, "undo",                                                 NULL, -1},
  {snd_K_v, snd_ControlMask, 0, kbd_false, true, "set window size as percentage of total",               NULL, -1},
  {snd_K_w, snd_ControlMask, 0, kbd_false, true, "save current channel in file",                         NULL, -1},
  {snd_K_z, snd_ControlMask, 0, kbd_false, true, "smooth using cosine",                                  NULL, -1},
};


void map_over_keys(bool (*func)(int key, int state, bool cx, XEN xf))
{
  int i;
  for (i = 0; i < keymap_top; i++)
    if ((XEN_BOUND_P(keymap[i].func)) &&
	((*func)(keymap[i].key, 
		 keymap[i].state, 
		 keymap[i].cx_extended, 
		 keymap[i].func)))
      return;
}


static key_info *make_key_info(key_entry k)
{
  key_info *ki;
  ki = (key_info *)calloc(1, sizeof(key_info));
#if USE_MOTIF
  ki->key = XKeysymToString(k.key); /* no free! */
#else
  #if USE_GTK
  ki->key = gdk_keyval_name(k.key);
  #endif
#endif
  ki->c = k.state & snd_ControlMask;
  ki->m = k.state & snd_MetaMask;
  ki->x = k.cx_extended;
  return(ki);
}


key_info *find_prefs_key(const char *prefs_name)
{
  int i;
  key_info *ki;
  for (i = 0; i < keymap_top; i++)
    if ((XEN_BOUND_P(keymap[i].func)) &&
	(mus_strcmp(keymap[i].prefs_info, prefs_name)))
      return(make_key_info(keymap[i]));

  for (i = 0; i < NUM_BUILT_IN_KEYS; i++)
    if (mus_strcmp(built_in_keys[i].prefs_info, prefs_name))
      return(make_key_info(built_in_keys[i]));

  ki = (key_info *)calloc(1, sizeof(key_info));
  ki->key = NULL;
  ki->c = false;
  ki->m = false;
  ki->x = false;
  return(ki);
}


char *key_description(int key, int state, bool cx_extended)
{
  int pos;
  if ((key < MIN_KEY_CODE) || (key > MAX_KEY_CODE) ||
      (state < MIN_KEY_STATE) || (state > MAX_KEY_STATE))
    return(NULL);
  pos = in_keymap(key, state, cx_extended);
  if (pos < 0) pos = in_keymap(key, state, cx_extended);
  if (pos >= 0)
    {
      if (keymap[pos].origin)
	return(mus_strdup(keymap[pos].origin));
      return(mus_strdup("something indescribable")); /* NULL would mean "no binding" */
    }
  for (pos = 0; pos < NUM_BUILT_IN_KEYS; pos++)
    if ((built_in_keys[pos].key == key) && 
	(built_in_keys[pos].state == state) && 
	(built_in_keys[pos].cx_extended == cx_extended))
      return(mus_strdup(built_in_keys[pos].origin));
  return(NULL);
}


void set_keymap_entry(int key, int state, int args, XEN func, bool cx_extended, const char *origin, const char *prefs_info)
{
  int i;
  i = in_keymap(key, state, cx_extended);
  if (i == -1)
    {
      if (keymap_size == keymap_top)
	{
	  keymap_size += 16;
	  if (keymap_top == 0)
	    {
	      keymap = (key_entry *)calloc(keymap_size, sizeof(key_entry));
	      for (i = 0; i < keymap_size; i++) 
		keymap[i].func = XEN_UNDEFINED;
	    }
	  else 
	    {
	      keymap = (key_entry *)realloc(keymap, keymap_size * sizeof(key_entry));
	      for (i = keymap_top; i < keymap_size; i++) 
		{
		  keymap[i].key = 0; 
		  keymap[i].state = 0; 
		  keymap[i].func = XEN_UNDEFINED;
		  keymap[i].cx_extended = false;
		  keymap[i].origin = NULL;
		  keymap[i].prefs_info = NULL;
		  keymap[i].gc_loc = NOT_A_GC_LOC;
		}
	    }
	}
      keymap[keymap_top].key = key;
      keymap[keymap_top].state = state;
      keymap[keymap_top].cx_extended = cx_extended;
      check_menu_labels(key, state, cx_extended);
      i = keymap_top;
      keymap_top++;
    }
  else
    {
      if ((XEN_PROCEDURE_P(keymap[i].func)) &&
	  (keymap[i].gc_loc != NOT_A_GC_LOC))
	{
	  snd_unprotect_at(keymap[i].gc_loc);
	  keymap[i].gc_loc = NOT_A_GC_LOC;
	}
      if (keymap[i].origin)
	{
	  /* this is silly... */
	  char *tmp;
	  tmp = (char *)keymap[i].origin;
	  keymap[i].origin = NULL;
	  free(tmp);
	}
      if (keymap[i].prefs_info)
	{
	  char *tmp;
	  tmp = (char *)keymap[i].prefs_info;
	  keymap[i].prefs_info = NULL;
	  free(tmp);
	}
    }
  keymap[i].origin = mus_strdup(origin);
  keymap[i].prefs_info = mus_strdup(prefs_info);
  keymap[i].args = args;
  keymap[i].func = func;
  if (XEN_PROCEDURE_P(func)) 
    keymap[i].gc_loc = snd_protect(func);
}


static void call_keymap(int hashedsym, int count)
{
  kbd_cursor_t res = KEYBOARD_NO_ACTION;

  if (XEN_BOUND_P(keymap[hashedsym].func))
    {
      /* not _NO_CATCH here because the code is not protected at any higher level */
      if (keymap[hashedsym].args == 0)
	res = (kbd_cursor_t)XEN_TO_C_INT_OR_ELSE(XEN_CALL_0(keymap[hashedsym].func, 
							    keymap[hashedsym].origin), 
						 (int)KEYBOARD_NO_ACTION);
      else res = (kbd_cursor_t)XEN_TO_C_INT_OR_ELSE(XEN_CALL_1(keymap[hashedsym].func, 
							       C_TO_XEN_INT(count), 
							       keymap[hashedsym].origin),
						    (int)KEYBOARD_NO_ACTION);
    }
  handle_cursor(selected_channel(), res);
}


/* ---------------- minibuffer ---------------- */

void string_to_minibuffer(snd_info *sp, const char *buf)
{
  if ((sp->minibuffer_on == MINI_PROMPT) || 
      (sp->minibuffer_on == MINI_USER))
    display_minibuffer_error(sp, buf); /* leave the prompt alone */
  else
    {
      set_minibuffer_string(sp, buf, false); /* was true, but that causes bogus expose events of entire graph widget -- perhaps pass this as parameter? */
      sp->minibuffer_on = MINI_REPORT;
    }
}


void report_in_minibuffer(snd_info *sp, const char *format, ...)
{
#if (!USE_NO_GUI)
  char *buf;
  va_list ap;
  if ((!sp) || (!(sp->active)) || (sp->inuse != SOUND_NORMAL)) return;
  va_start(ap, format);
  buf = vstr(format, ap);
  va_end(ap);
  string_to_minibuffer(sp, buf);
  free(buf);
#endif
}


void clear_minibuffer(snd_info *sp)
{
  set_minibuffer_string(sp, NULL, true);
  sp->search_count = 0;
  sp->marking = 0;
  sp->minibuffer_on = MINI_OFF;
}


void errors_to_minibuffer(const char *msg, void *data)
{
  snd_info *sp;
  sp = (snd_info *)data;
  if (!(snd_ok(sp)))
    {
      sp = any_selected_sound();
      if (!snd_ok(sp)) return;
    }
  display_minibuffer_error((snd_info *)data, msg);
}


void printout_to_minibuffer(const char *msg, void *data)
{
  string_to_minibuffer((snd_info *)data, msg);
}



/* ---------------- other kbd built-in commands ---------------- */

static void cursor_moveto_end(chan_info *cp)
{
  cursor_moveto(cp, CURRENT_SAMPLES(cp) - 1);
}


static void set_window_bounds(chan_info *cp, int count) 
{
  /* count = sample number to start at */
  axis_info *ap;
  ap = cp->axis;
  if (ap->x_ambit != 0.0)
    {
      double sx;
      sx = (((double)count / (double)SND_SRATE(cp->sound)) - ap->xmin) / ap->x_ambit;
      reset_x_display(cp, sx, ap->zx);
    }
}


static void set_window_size(chan_info *cp, int count) 
{
  /* set samples within window */
  axis_info *ap;
  ap = cp->axis;
  if (ap->x_ambit != 0.0)
    {
      double zx;
      zx = ((double)count / (((double)SND_SRATE(cp->sound)) * ap->x_ambit));
      reset_x_display(cp, ap->sx, zx);
    }
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
  x0 = (((double)(selection_beg(cp))) / ((double)SND_SRATE(cp->sound)));
  x1 = x0 + ((double)(selection_len())) / ((double)(SND_SRATE(cp->sound)));
  set_x_axis_x0x1(cp, x0, x1);
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      sp = ss->sounds[i];
      if ((sp) && 
	  (sp->inuse == SOUND_NORMAL) && 
	  (cp->sound != sp) && 
	  (selection_is_active_in_channel(sp->chans[0])) && 
	  (sp->sync != (cp->sound->sync)))
	set_x_axis_x0x1(sp->chans[0], x0, x1);
    }
}


static chan_info *goto_next_graph(chan_info *cp, int count);

static chan_info *goto_previous_graph(chan_info *cp, int count)
{
  snd_info *sp;
  chan_info *ncp, *vcp;
  int i, k, j, chan;
  if (count == 0) return(cp);
  sp = cp->sound;
  if (sp->inuse != SOUND_NORMAL) return(cp);
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
	if ((snd_ok(ss->sounds[i])) && (ss->sounds[i]->inuse == SOUND_NORMAL))
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
	  if ((snd_ok(ss->sounds[i])) && (ss->sounds[i]->inuse == SOUND_NORMAL))
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
  if (!ncp) snd_error_without_format("goto previous graph failed!");
  select_channel(ncp->sound, ncp->chan);
  return(ncp);
}


static chan_info *goto_next_graph(chan_info *cp, int count)
{
  snd_info *sp;
  chan_info *ncp, *vcp;
  int i, k, j, chan;
  if (count == 0) return(cp);
  sp = cp->sound;
  if (sp->inuse != SOUND_NORMAL) return(cp);
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
	if ((snd_ok(ss->sounds[i])) && (ss->sounds[i]->inuse == SOUND_NORMAL))
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
	  if ((snd_ok(ss->sounds[i])) && (ss->sounds[i]->inuse == SOUND_NORMAL))
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
  if (!ncp) snd_error_without_format("goto next graph failed!");
  select_channel(ncp->sound, ncp->chan);
  return(ncp);
}


void save_edits_from_kbd(snd_info *sp)
{
  /* this used to prompt for confirmation, but we now use a dialog
   */
  redirect_everything_to(printout_to_minibuffer, (void *)sp);
#if (!USE_NO_GUI)
  {
    io_error_t err;
    err = save_edits(sp);
    if (err == IO_NEED_WRITE_CONFIRMATION)
      changed_file_dialog(sp);
  }
#else
  save_edits_without_asking(sp);
#endif

  redirect_everything_to(NULL, NULL);
}


/* ---------------- key response ---------------- */

static mus_long_t get_count_1(char *number_buffer, int number_ctr, bool dot_seen, chan_info *cp)
{
  /* allow floats here = secs */
  int i;
  if (number_ctr == 0) return(1); /* c-u followed by nothing = 1 */
  number_buffer[number_ctr] = '\0';
  if (number_ctr == 1)
    { /* handle special cases of just - or + */
      if (number_buffer[0] == '-') return(-1);
      if (number_buffer[0] == '+') return(1);
      if (number_buffer[0] == '.') return(0);  /* -. and +. -> 0 from sscanf */
    }
  if (dot_seen)
    {
      float f;
      if (!(sscanf(number_buffer, "%f", &f)))
	{
	  /* this doesn't happen for most bogus cases -- the C spec says "it is not possible to determine
	   *    directly whether matches of literal character in the control string succeed or fail."
	   *    but really bad stuff like C-u ... C-f does get flagged.
	   */
	  snd_error("invalid number: %s", number_buffer);
	  return(0);
	}
      return((mus_long_t)(f * SND_SRATE(cp->sound)));
    }
  if (!(sscanf(number_buffer, "%d", &i)))
    {
      snd_error("invalid number: %s", number_buffer);
      return(0);
    }
  return(i);
}


static mus_long_t get_count(char *number_buffer, int number_ctr, bool dot_seen, chan_info *cp, bool mark_wise)
{
  mus_long_t val, old_cursor;
  val = get_count_1(number_buffer, number_ctr, dot_seen, cp);
  if (!mark_wise) return(val);
  old_cursor = CURSOR(cp);
  if (!(goto_mark(cp, val)))
    string_to_minibuffer(cp->sound, "no such mark");
  val = CURSOR(cp) - old_cursor; /* will be 0 if no relevant marks */
  CURSOR(cp) = old_cursor;
  return(val);
}


#define NUMBER_BUFFER_SIZE 12

static mus_float_t state_amount(int state)
{
  mus_float_t amount;
  amount = 1.0;
  if (state & snd_ControlMask) amount *= 0.5;
  if (state & snd_MetaMask) amount *= 0.5;
  if (state & snd_ShiftMask) amount *= 0.5;
  return(amount);
}


static void zoom_fft(mus_float_t amount)
{
  mus_float_t zx, mx;
  zx = spectrum_end(ss) - spectrum_start(ss);
  mx = (spectrum_end(ss) + spectrum_start(ss)) * 0.5;
  zx *= amount;
  mx -= (zx * 0.5);
  if (mx < 0.0) mx = 0.0;
  set_spectrum_start(mx);
  if ((mx + zx) <= 1.0)
    set_spectrum_end(mx + zx);
  else set_spectrum_end(1.0);
}


static void move_fft(mus_float_t amount)
{
  mus_float_t mx, zx;
  zx = spectrum_end(ss) - spectrum_start(ss);
  mx = spectrum_start(ss) + zx * amount;
  if (mx < 0.0) mx = 0.0;
  if (mx >= 1.0) mx = 1.0 - zx;
  set_spectrum_start(mx);
  if ((mx + zx) <= 1.0)
    set_spectrum_end(mx + zx);
  else set_spectrum_end(1.0);
}


static bool stop_selecting(int keysym, int state)
{
  return(((state & snd_ControlMask) == 0) ||
	 (keysym == snd_K_D) || (keysym == snd_K_d) ||
	 (keysym == snd_K_H) || (keysym == snd_K_h) ||
	 (keysym == snd_K_Y) || (keysym == snd_K_y));
}


static const char *key_to_name(int keysym) 
{
  if (keysym) 
    {
      const char *str;
      str = KEY_TO_NAME(keysym);
      if (str) return(str);
    }
  return("NUL");
}


static int number_ctr = 0;
static bool dot_seen = false;
static bool counting = false;
static bool extended_mode = false;

void control_g(snd_info *sp)
{
  ss->C_g_typed = true;

  number_ctr = 0; 
  counting = false; 
  dot_seen = false; 
  extended_mode = false;
  /* if (selection_is_active()) deactivate_selection(); */
  defining_macro = false;
  clear_stdin();
  redirect_everything_to(NULL, NULL);

  if ((ss->checking_explicitly) || 
      (play_in_progress())) 
    ss->stopped_explicitly = true; 
  /* this tries to break out of long filter/src computations (and perhaps others) */
  /*   but, as in other such cases, it leaves this flag set so all subsequent uses of it need to clear it first */

  stop_playing_all_sounds(PLAY_C_G); /* several scm files assume hooks called upon C-g -- could be region play, etc */

  if (sp)
    {
      if (sp->applying) stop_applying(sp);
      for_each_sound_chan(sp, stop_fft_in_progress);
      clear_minibuffer(sp);
    }
}

#ifndef SND_KEYMASK
  #define SND_KEYMASK (snd_ControlMask | snd_MetaMask)
#endif


void keyboard_command(chan_info *cp, int keysym, int unmasked_state)
{
  /* we can't use the meta bit in some cases because this is trapped at a higher level for the Menu mnemonics */
  /* state here is the kbd bucky-bit state -- it might have bogus junk like NumLock */
  /* keysym has Shift taken into account already (see snd-xchn.c XKeycodeToKeysym, and same snd-xsnd.c) */

  static bool u_count = false;
  static char number_buffer[NUMBER_BUFFER_SIZE];
  static mus_long_t count = 1;
  static bool got_count = false;
  static bool m = false;
  int shift = 0;
  bool cursor_searching = false, clear_search = true;
  int hashloc, i, state;
  mus_long_t loc;
  static mus_long_t ext_count = 1;
  static bool got_ext_count = false;
  snd_info *sp;
  axis_info *ap;

  /* fprintf(stderr, "%d (%s %d%s) ", keysym, KEY_TO_NAME(keysym), unmasked_state, (extended_mode) ? " (c-x)" : ""); */

  if ((!cp) || (!(cp->sound)) || (cp->active < CHANNEL_HAS_EDIT_LIST)) return;
  sp = cp->sound;
  if ((!sp) || (sp->inuse != SOUND_NORMAL)) return;

  ap = cp->axis;

  if (keysym >= snd_K_Shift_L) return;
  /* this happens when the user presses Control or Shift etc prior to hitting the actual (modified) key */
  shift = unmasked_state & snd_ShiftMask;
  state = unmasked_state & SND_KEYMASK; /* mask off stuff we don't care about */

  if (defining_macro) continue_macro(keysym, state);
  if (!m) count = 1; else m = false;
  
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
      got_count = true;
      number_ctr = 0;
      counting = false;
      dot_seen = false;
      if (m) return;
    }

  u_count = false;

  if ((keysym != snd_K_X) && (keysym != snd_K_x) &&
      (keysym != snd_K_B) && (keysym != snd_K_b) &&
      (keysym != snd_K_F) && (keysym != snd_K_f))
    {
      got_count = false;
      if (count == 0) return;
    }

#if HAVE_EXTENSION_LANGUAGE
  hashloc = in_keymap(keysym, state, extended_mode);
  if (hashloc != -1)                       /* found user-defined key */
    {
      extended_mode = false;
      call_keymap(hashloc, count);
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
	      cp->cursor_on = true; 
	      loc = (mus_long_t)(ap->x0 * SND_SRATE(sp)); 
	      if ((loc + 1) == ap->losamp) loc = ap->losamp; /* handle dumb rounding problem */
	      cursor_moveto(cp, loc); 
	      break;

	    case snd_K_B: case snd_K_b: 
	      /* this was by samples which is nuts if we're looking at a large window -- should be by pixel, I think */
	      {
		int samples_per_pixel = 1;
		if ((!got_count) &&                              /* C-u 2.1 C-b moves back 2.1 seconds */
		    (cp->axis->x_axis_x1 > cp->axis->x_axis_x0)) /* avoid divide by zero if window tiny */
		  {
		    samples_per_pixel = (cp->axis->hisamp - cp->axis->losamp) / (cp->axis->x_axis_x1 - cp->axis->x_axis_x0);
		    if (samples_per_pixel < 1) samples_per_pixel = 1;
		  }
		cp->cursor_on = true; 
		cursor_move(cp, -count * samples_per_pixel);
		got_count = false;
	      }
	      break;

	    case snd_K_D: case snd_K_d: 
	      cp->cursor_on = true; 
	      cursor_delete(cp, count); 
	      break;

	    case snd_K_E: case snd_K_e:
	      cp->cursor_on = true; 
	      loc = (mus_long_t)(ap->x1 * (double)SND_SRATE(sp));
	      if ((loc + 1) == ap->hisamp) loc = ap->hisamp;
	      cursor_moveto(cp, loc); 
	      break;

	    case snd_K_F: case snd_K_f:
	      {
		int samples_per_pixel = 1;
		if ((!got_count) &&
		    (cp->axis->x_axis_x1 > cp->axis->x_axis_x0))
		  {
		    samples_per_pixel = (cp->axis->hisamp - cp->axis->losamp) / (cp->axis->x_axis_x1 - cp->axis->x_axis_x0);
		    if (samples_per_pixel < 1) samples_per_pixel = 1;
		  }
		cp->cursor_on = true; 
		cursor_move(cp, count * samples_per_pixel); 
		got_count = false;
	      }
	      break;

	    case snd_K_G: case snd_K_g: 
	      if (state & snd_MetaMask)
		clear_listener();
	      else control_g(sp);
	      break;

	    case snd_K_H: case snd_K_h: 
	      cp->cursor_on = true; 
	      cursor_delete(cp, -count); 
	      break; 

	    case snd_K_I: case snd_K_i: 
	      show_cursor_info(cp); 
	      break;

	    case snd_K_J: case snd_K_j: 
	      cp->cursor_on = true; 
	      if (!(goto_mark(cp, count)))
		string_to_minibuffer(cp->sound, "no such mark");
	      break;

	    case snd_K_K: case snd_K_k: 
	      cp->cursor_on = true; 
	      cursor_delete(cp, count * 128);
	      break;

	    case snd_K_L: case snd_K_l: 
	      cp->cursor_on = true; 
	      handle_cursor_with_sync(cp, CURSOR_IN_MIDDLE);
	      break;

	    case snd_K_M: case snd_K_m:
	      {
		mark *mk = NULL;
		if (count > 0) 
		  {
		    cp->cursor_on = true;
		    set_show_marks(true);
		    mk = add_mark(CURSOR(cp), NULL, cp);
		    display_channel_marks(cp);
		  }
		else 
		  {
		    if (!(delete_mark_samp(CURSOR(cp), cp)))
		      report_in_minibuffer(cp->sound, "no mark at sample %lld", CURSOR(cp));
		  }
		if ((keysym == snd_K_M) && 
		    (cp->sound->sync != 0))
		  {
		    sync_info *si;
		    int sync_num;
		    sync_num = mark_sync_max() + 1; 
		    if (mk) set_mark_sync(mk, sync_num);
		    si = snd_sync(cp->sound->sync);
		    for (i = 0; i < si->chans; i++) 
		      if (cp != si->cps[i])
			{
			  if (count > 0)
			    {
			      mk = add_mark(CURSOR(cp), NULL, si->cps[i]);
			      if (mk)
				{
				  set_mark_sync(mk, sync_num);
				  display_channel_marks(si->cps[i]);
				}
			    }
			  else 
			    {
			      if (!(delete_mark_samp(CURSOR(cp), si->cps[i])))
				report_in_minibuffer(cp->sound, "no mark at sample %lld", CURSOR(cp));
			    }
			}
		    si = free_sync_info(si);
		  }
	      }
	      break;

	    case snd_K_N: case snd_K_n: 
	      cp->cursor_on = true; 
	      cursor_move(cp, count * 128); 
	      break;

	    case snd_K_O: case snd_K_o: 
	      cp->cursor_on = true; 
	      cursor_insert(cp, CURSOR(cp), count); 
	      break;

	    case snd_K_P: case snd_K_p: 
	      cp->cursor_on = true; 
	      cursor_move(cp, -count * 128); 
	      break;

	    case snd_K_Q: case snd_K_q: 
	      play_channel(cp, CURSOR(cp), NO_END_SPECIFIED);
	      break;

#if HAVE_EXTENSION_LANGUAGE
	    case snd_K_S: case snd_K_s: 
	      /* TODO: handle this in a dialog */
	      cp->cursor_on = true; 
	      cursor_search(cp, count); 
	      cursor_searching = true; 
	      break;
#endif
	    case snd_K_T: case snd_K_t: 
	      stop_playing_sound(sp, PLAY_C_T); 
	      set_play_button(sp, false);
	      break;

	    case snd_K_U: case snd_K_u: 
	      counting = true; 
	      u_count = true;
	      number_ctr = 0; 
	      dot_seen = false; 
	      break;

	    case snd_K_V: case snd_K_v:
	      cp->cursor_on = true;
	      /* in emacs this is move ahead one window, but for some reason in Snd it's center cursor?? */
	      cursor_moveto(cp, (mus_long_t)((ap->losamp + ap->hisamp) / 2));
	      break;

	    case snd_K_W: case snd_K_w: 
	      delete_selection(UPDATE_DISPLAY); 
	      break;

	    case snd_K_X: case snd_K_x: 
	      extended_mode = true; 
	      if (got_count) 
		{
		  ext_count = count; 
		  got_ext_count = got_count;
		  got_count = false;
		}
	      break;

	    case snd_K_Y: case snd_K_y: 
	      paste_region(region_list_position_to_id(0), cp);
	      break;

	    case snd_K_Z: case snd_K_z: 
	      cp->cursor_on = true; 
	      cursor_zeros(cp, count, OVER_SOUND); 
	      break;

	    case snd_K_Right: 
	      sx_incremented(cp, state_amount(state | shift));
	      break;

	    case snd_K_Left: 
	      sx_incremented(cp, -state_amount(state | shift)); 
	      break;

	    case snd_K_Up: 
	      zx_incremented(cp, 1.0 + state_amount(state | shift)); 
	      break;

	    case snd_K_Down: 
	      zx_incremented(cp, 1.0 / (1.0 + state_amount(state | shift))); 
	      break;

	    case snd_K_0: case snd_K_1: case snd_K_2: case snd_K_3: case snd_K_4:
	    case snd_K_5: case snd_K_6: case snd_K_7: case snd_K_8: case snd_K_9: 
	      counting = true;
	      number_buffer[number_ctr] = (char)('0' + keysym - snd_K_0); 
	      if (number_ctr < (NUMBER_BUFFER_SIZE - 2)) 
		number_ctr++; 
	      /* there is also the bare-number case below */
	      break;

	    case snd_keypad_0: case snd_keypad_1: case snd_keypad_2: case snd_keypad_3: case snd_keypad_4:
	    case snd_keypad_5: case snd_keypad_6: case snd_keypad_7: case snd_keypad_8: case snd_keypad_9: 
	      counting = true;
	      number_buffer[number_ctr] = (char)('0' + keysym - snd_keypad_0); 
	      if (number_ctr < (NUMBER_BUFFER_SIZE - 2)) 
		number_ctr++; 
	      /* there is also the bare-number case below */
	      break;

	    case snd_K_space: 
	      if (count > 0)
		{
		  start_selection_creation(cp, CURSOR(cp));
		  report_in_minibuffer(sp, "selection starts at %lld", CURSOR(cp));
		  clear_search = false;
		}
	      break;

	    case snd_K_period:
	    case snd_keypad_Decimal:
	      counting = true; 
	      number_buffer[number_ctr] = '.'; 
	      number_ctr++; 
	      dot_seen = true; 
	      break;

	    case snd_K_greater: 
	      cp->cursor_on = true; 
	      cursor_moveto_end(cp); 
	      break;

	    case snd_K_less: 
	      cp->cursor_on = true; 
	      cursor_moveto(cp ,0); 
	      break;

	    case snd_K_minus: 
	      counting = true; 
	      number_ctr = 1; 
	      number_buffer[0] = '-'; 
	      break;

	    case snd_K_underscore: 
	      undo_edit_with_sync(cp, count); 
	      break;

	    default:
	      report_in_minibuffer(sp, "C-%s undefined", key_to_name(keysym));
	      break;
	    }
	}
      else /* extended mode with ctrl down */
	{
	  /* -------------------------------- C-x C-key -------------------------------- */
	  if (!got_ext_count) ext_count = 1;
	  extended_mode = false;
	  switch (keysym)
	    {
	    case snd_K_B: case snd_K_b: 
	      set_window_bounds(cp, ext_count); 
	      break;

	    case snd_K_C: case snd_K_c: 
	      hide_controls(sp); 
	      break;

	    case snd_K_F: case snd_K_f: 
	      make_open_file_dialog(FILE_READ_WRITE, true);
	      break;

	    case snd_K_G: case snd_K_g: 
	      control_g(sp);
	      break;

	    case snd_K_J: case snd_K_j:
	      cp->cursor_on = true; 
	      goto_mix(cp, ext_count); 
	      break;

	    case snd_K_O: case snd_K_o: 
	      /* this doesn't change the View:Controls menu label because (sigh...) it's specific to the currently selected sound */
	      show_controls(sp); 
	      break;

	    case snd_K_P: case snd_K_p: 
	      set_window_size(cp, ext_count); 
	      break;

	    case snd_K_Q: case snd_K_q: 
	      make_mix_file_dialog(true);
	      break;

	    case snd_K_R: case snd_K_r: 
	      redo_edit_with_sync(cp, ext_count); 
	      break;

	    case snd_K_S: case snd_K_s: 
	      save_edits_from_kbd(sp);
	      break;

	    case snd_K_T: case snd_K_t: 
	      stop_playing_sound(sp, PLAY_C_T); 
	      break;

	    case snd_K_U: case snd_K_u: 
	      undo_edit_with_sync(cp, ext_count); 
	      break;

	    case snd_K_V: case snd_K_v:
	      set_window_percentage(cp, ext_count);
	      break;

	    case snd_K_W: case snd_K_w: 
	      {
		chan_info *cp;
		cp = any_selected_channel(any_selected_sound());
		make_channel_extract_dialog(cp->chan);
	      }
	      break;

	    case snd_K_Z: case snd_K_z: 
	      cp->cursor_on = true; 
	      cos_smooth(cp, CURSOR(cp), ext_count, OVER_SOUND); 
	      break;

	    case snd_K_Right: 
	      sx_incremented(cp, state_amount(state | shift));
	      break;

	    case snd_K_Left:  
	      sx_incremented(cp, -state_amount(state | shift)); 
	      break;

	    case snd_K_Up: 
	      zx_incremented(cp, 1.0 + state_amount(state | shift)); 
	      break;

	    case snd_K_Down: 
	      zx_incremented(cp, 1.0 / (1.0 + state_amount(state | shift))); 
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
	      counting = true;
	      number_buffer[number_ctr] = (char)('0' + keysym-snd_K_0); 
	      if (number_ctr < (NUMBER_BUFFER_SIZE - 2)) 
		number_ctr++; 
	      break;

	    case snd_keypad_0: case snd_keypad_1: case snd_keypad_2: case snd_keypad_3: case snd_keypad_4:
	    case snd_keypad_5: case snd_keypad_6: case snd_keypad_7: case snd_keypad_8: case snd_keypad_9: 
	      counting = true;
	      number_buffer[number_ctr] = (char)('0' + keysym - snd_keypad_0); 
	      if (number_ctr < (NUMBER_BUFFER_SIZE - 2)) 
		number_ctr++; 
	      break;

	    case snd_K_period: 
	    case snd_keypad_Decimal:
	      counting = true; 
	      number_buffer[number_ctr] = '.'; 
	      number_ctr++; 
	      dot_seen = true; 
	      break;

	    case snd_K_greater: 
	      cp->cursor_on = true; 
	      cursor_moveto_end(cp); 
	      break;

	    case snd_K_less: 
	      cp->cursor_on = true; 
	      cursor_moveto(cp, 0); 
	      break;

	    case snd_K_minus: 
	      counting = true; 
	      number_buffer[0] = '-'; 
	      number_ctr = 1; 
	      break;

	    case snd_K_Right: 
	      sx_incremented(cp, state_amount(state | shift)); 
	      break;

	    case snd_K_Left:  
	      sx_incremented(cp, -state_amount(state | shift)); 
	      break;

	    case snd_K_Up:    
	      zx_incremented(cp, 1.0 + state_amount(state | shift)); 
	      break;

	    case snd_K_Down: 
	      zx_incremented(cp, 1.0 / (1.0 + state_amount(state | shift))); 
	      break;

	    case snd_K_Home: 
	      redirect_everything_to(printout_to_minibuffer, (void *)sp);
	      sp = snd_update(sp); 
	      redirect_everything_to(NULL, NULL);
	      break;

	    case snd_K_space: 
	      if (play_in_progress())
		toggle_dac_pausing(); 
	      else play_sound(sp, CURSOR(cp), NO_END_SPECIFIED); /* was deactivate_selection */
	      break;

	    case snd_keypad_Add:
	      if (time_graph_type(ss) == GRAPH_AS_WAVOGRAM) 
		set_wavo_trace(wavo_trace(ss) + 1); 
	      else set_spectro_hop(spectro_hop(ss) + 1);
	      reflect_spectro(); 
	      break;

	    case snd_keypad_Subtract: 
	      if (time_graph_type(ss) == GRAPH_AS_WAVOGRAM) 
		{
		  if (wavo_trace(ss) > 1) 
		    set_wavo_trace(wavo_trace(ss) - 1);
		} 
	      else 
		{
		  if (spectro_hop(ss) > 1) 
		    set_spectro_hop(spectro_hop(ss) - 1);
		}
	      reflect_spectro(); 
	      break;

	    case snd_keypad_Multiply: 
	      set_transform_size(transform_size(ss) * 2); 
	      break;

	    case snd_keypad_Divide: 
	      if (transform_size(ss) > 4) 
		set_transform_size(transform_size(ss) / 2); 
	      break;

	    case snd_keypad_Delete:
	      set_dot_size(dot_size(ss) + 1); 
	      break;

	    case snd_keypad_Insert:
	      if (dot_size(ss) > 1) 
		set_dot_size(dot_size(ss) - 1); 
	      break;

	    case snd_keypad_Enter: 
	      reset_spectro(); 
	      reflect_spectro(); 
	      break;

	    case snd_keypad_Up:
	      zoom_fft(1.0 + state_amount(state | shift));
	      break;

	    case snd_keypad_Down:
	      zoom_fft(1.0 / (1.0 + state_amount(state | shift))); 
	      break;

	    case snd_keypad_Left:
	      move_fft(-state_amount(state | shift)); 
	      break;

	    case snd_keypad_Right:
	      move_fft(state_amount(state | shift)); 
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
		if (listener_is_visible())
		  {
		    goto_listener();
		    listener_append(buf);
		  }
	      }
#else
	      report_in_minibuffer(sp, "key %s%s undefined", (state & snd_MetaMask) ? "M-" : "", key_to_name(keysym));
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
	  extended_mode = false;
	  if (!(state & snd_MetaMask))
	    {
	      switch (keysym)
		{
		case snd_K_B: case snd_K_b: 
		  cp->cursor_on = true; 
		  handle_cursor(cp, CURSOR_ON_LEFT);
		  break;

		case snd_K_C: case snd_K_c: 
		  if (!(mark_define_region(cp, (!got_ext_count) ? 1 : ext_count)))
		    string_to_minibuffer(cp->sound, "no such mark");
		  break;

		case snd_K_E: case snd_K_e: 
		  if (defining_macro) 
		    {
		      string_to_minibuffer(sp, "can't call macro while it's being defined");
		      defining_macro = false;
		      macro_size = 0; /* so subsequent M-x e doesn't get something silly */
		    }
		  else
		    {
		      execute_last_macro(cp, (!got_ext_count) ? 1 : ext_count);
		      if ((cp) && (cp->sound) && (cp->active >= CHANNEL_HAS_EDIT_LIST)) handle_cursor(cp, cursor_decision(cp)); else return;
		    }
		  break;

		case snd_K_F: case snd_K_f: 
		  cp->cursor_on = true; 
		  handle_cursor(cp, CURSOR_ON_RIGHT); 
		  break;

		case snd_K_K: case snd_K_k: 
		  snd_close_file(sp); 
		  break;

		case snd_K_L: case snd_K_l: 
		  cp->cursor_on = true;
		  if (selection_is_active_in_channel(cp))
		    cursor_moveto(cp, (mus_long_t)(selection_beg(cp) + 0.5 * selection_len()));
		  else string_to_minibuffer(sp, "no active selection");
		  handle_cursor_with_sync(cp, CURSOR_IN_MIDDLE);
		  break;

		case snd_K_O: case snd_K_o: 
		  if (ext_count > 0) 
		    goto_next_graph(cp, ext_count); 
		  else goto_previous_graph(cp, ext_count); 
		  break;

		case snd_K_P: case snd_K_p: 
		  if (!got_ext_count)
		    play_selection(IN_BACKGROUND);
		  else play_region(ext_count, IN_BACKGROUND);
		  break;

		case snd_K_Q: case snd_K_q: 
		  add_selection_or_region((!got_ext_count) ? 0 : ext_count, cp); 
		  break;

		case snd_K_R: case snd_K_r: 
		  redo_edit_with_sync(cp, (!got_ext_count) ? 1 : ext_count); 
		  break;

		case snd_K_U: case snd_K_u: 
		  undo_edit_with_sync(cp, (!got_ext_count) ? 1 : ext_count); 
		  break;

		case snd_K_V: case snd_K_v: 
		  if (selection_is_active_in_channel(cp))
		    window_frames_selection(cp); 
		  else 
		    {
		      bool complain = true;
		      if (cp->sound->channel_style != CHANNELS_SEPARATE)
			{
			  int i;
			  for (i = 0; i < sp->nchans; i++)
			    if ((i != cp->chan) &&
				(selection_is_active_in_channel(sp->chans[i])))
			      {
				window_frames_selection(sp->chans[i]);
				complain = false;
				break;
			      }
			}
		      if (complain)
			string_to_minibuffer(sp, "no active selection");
		    }
		  break;

		case snd_K_Z: case snd_K_z: 
		  if (selection_is_active_in_channel(cp))
		    cos_smooth(cp, CURSOR(cp), (!got_ext_count) ? 1 : ext_count, OVER_SELECTION); 
		  else string_to_minibuffer(sp, "no active selection");
		  break;

		case snd_K_Right:   
		  sx_incremented(cp, state_amount(state | shift));
		  break;

		case snd_K_Left:
		  sx_incremented(cp, -state_amount(state | shift));
		  break;

		case snd_K_Up:
		  zx_incremented(cp, 1.0 + state_amount(state | shift));
		  break;

		case snd_K_Down:
		  zx_incremented(cp, 1.0 / (1.0 + state_amount(state | shift)));
		  break;

		case snd_K_less:
		  cp->cursor_on = true; 
		  cursor_moveto(cp, 0); 
		  break;

		case snd_K_greater: 
		  cp->cursor_on = true; 
		  cursor_moveto_end(cp);
		  break;

		case snd_K_openparen:
		  if (defining_macro) 
		    string_to_minibuffer(sp, "macro definition already in progress");
		  else
		    {
		      start_defining_macro(); 
		      string_to_minibuffer(sp, "defining macro..."); 
		    }
		  clear_search = false; 
		  break;

		case snd_K_closeparen: 
		  if (defining_macro)
		    {
		      stop_defining_macro(); 
		      clear_minibuffer(sp); 
		    }
		  clear_search = false;
		  break;

		default:
		  report_in_minibuffer(sp, "C-x %s undefined", key_to_name(keysym));
		  break;
		}
	    }
	  else
	    {
	      report_in_minibuffer(sp, "C-x M-%s undefined", key_to_name(keysym));
	    }
	}
    }
  if (!extended_mode) {got_ext_count = false; ext_count = 1;}
  if ((sp) && (clear_search))
    {
      if (!cursor_searching) 
	sp->search_count = 0;
    }
}



char *make_key_name(char *buf, int buf_size, int key, int state, bool extended)
{
  mus_snprintf(buf, buf_size, "%s%s%s",
	       (extended) ? "C-x " : "",
	       (state & snd_ControlMask) ? ((state & snd_MetaMask) ? "CM-" : "C-") : ((state & snd_MetaMask) ? "M-" : ""),
	       (key == snd_K_less) ? "<" : 
	       ((key == snd_K_greater) ? ">" : 
		((key == snd_K_openparen) ? "(" :
		 ((key == snd_K_closeparen) ? ")" :
		  ((key == snd_K_slash) ? "/" :
		   KEY_TO_NAME(key))))));
  return(buf);
}


static int key_name_to_key(XEN key, const char *caller)
{
  /* Ruby thinks chars are strings */
  if (XEN_INTEGER_P(key))
    return(XEN_TO_C_INT(key)); /* includes 0xffc0 style keys, and in Ruby things like ?a */

#if (!HAVE_RUBY)
  if (XEN_CHAR_P(key))
    return((int)(XEN_TO_C_CHAR(key)));
#endif

#if USE_MOTIF
  return((int)XStringToKeysym(XEN_TO_C_STRING(key)));  /* these are the X names: not "+" but "plus" etc */
#endif
#if USE_GTK
  return((int)gdk_keyval_from_name(XEN_TO_C_STRING(key)));
#endif
  return(0);
}


static XEN check_for_key_error(int k, int s, const char *caller)
{
  if ((k < MIN_KEY_CODE) || (k > MAX_KEY_CODE) ||
      (s < MIN_KEY_STATE) || (s > MAX_KEY_STATE))
    XEN_ERROR(XEN_ERROR_TYPE("no-such-key"),
	      XEN_LIST_4(C_TO_XEN_STRING("~A: no such key: ~A, state: ~A"),
			 C_TO_XEN_STRING(caller),
			 C_TO_XEN_INT(k),
			 C_TO_XEN_INT(s)));
  return(XEN_FALSE);
}


static XEN g_key_binding(XEN key, XEN state, XEN cx_extended)
{
  #define H_key_binding "(" S_key_binding " key :optional (state 0) extended): function bound to this key and associated \
modifiers.  As in " S_bind_key ", state is the logical 'or' of ctrl=4, meta=8, and 'extended' is " PROC_TRUE " if the key is \
prefixed with C-x. 'key' can be a character, a key name such as 'Home', or an integer."
  int i, k, s;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(key) || XEN_CHAR_P(key) || XEN_STRING_P(key), key, XEN_ARG_1, S_key_binding, "an integer, character, or string");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(state), state, XEN_ARG_2, S_key_binding, "an integer");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(cx_extended), cx_extended, XEN_ARG_3, S_key_binding, "a boolean");

  k = key_name_to_key(key, S_key_binding);
  s = XEN_TO_C_INT_OR_ELSE(state, 0) & 0xfffe; /* no shift bit */
  check_for_key_error(k, s, S_key_binding);
  i = in_keymap(k, s, XEN_TRUE_P(cx_extended));
  if (i >= 0) 
    return(keymap[i].func);

  return(XEN_UNDEFINED);
}


static XEN g_bind_key_1(XEN key, XEN state, XEN code, XEN cx_extended, XEN origin, XEN prefs_info, const char *caller)
{
  int args, k = 0, s;
  bool e;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(key) || XEN_STRING_P(key) || XEN_CHAR_P(key), key, XEN_ARG_1, caller, "an integer, char, or string");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(state), state, XEN_ARG_2, caller, "an integer");
  XEN_ASSERT_TYPE((XEN_FALSE_P(code) || XEN_PROCEDURE_P(code)), code, XEN_ARG_3, caller, PROC_FALSE " or a procedure");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(cx_extended), cx_extended, XEN_ARG_4, caller, "a boolean");
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(origin), origin, XEN_ARG_5, caller, "a string");
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(prefs_info), prefs_info, XEN_ARG_6, caller, "a string");

  k = key_name_to_key(key, caller);
  s = XEN_TO_C_INT(state) & 0xfffe; /* get rid of shift bit */
  check_for_key_error(k, s, caller);
  e = (XEN_TRUE_P(cx_extended));

  if (XEN_FALSE_P(code))
    set_keymap_entry(k, s, 0, XEN_UNDEFINED, e, NULL, NULL);
  else 
    {
      char buf[256];
      const char *comment = NULL, *prefs = NULL;
      args = XEN_REQUIRED_ARGS(code);
      if (args > 1)
	{
	  XEN errmsg;
	  char *errstr;
	  errstr = mus_format(S_bind_key " function arg should take either zero or one args, not %d", args);
	  errmsg = C_TO_XEN_STRING(errstr);

	  free(errstr);
	  return(snd_bad_arity_error(caller, errmsg, code));
	}
      if (XEN_STRING_P(origin)) comment = XEN_TO_C_STRING(origin); else comment = make_key_name(buf, 256, k, s, e);
      if (XEN_STRING_P(prefs_info)) prefs = XEN_TO_C_STRING(prefs_info);
      set_keymap_entry(k, s, args, code, e, comment, prefs);
    }

  return(code);
}


static XEN g_bind_key(XEN key, XEN state, XEN code, XEN cx_extended, XEN origin, XEN prefs_info)
{
  #define H_bind_key "(" S_bind_key " key modifiers func :optional extended origin prefs-info): \
causes 'key' (an integer, character, or string) \
when typed with 'modifiers' (0:none, 4:control, 8:meta) (and C-x if extended) to invoke 'func', a function of \
zero or one arguments. If the function takes one argument, it is passed the preceding C-u number, if any. \
The function should return one of the cursor choices (e.g. " S_keyboard_no_action ").  'origin' is \
the name reported if an error occurs. The 'key' argument can be the X/Gtk name of the key (e.g. \"plus\" for \"+\" or \"Home\"), \
the character on the key (#\a), or the integer corresponding to that character: (\"(char->integer #\a)\" in Scheme, \
or \"?a\" in Ruby."
  
  return(g_bind_key_1(key, state, code, cx_extended, origin, prefs_info, S_bind_key));
}


static XEN g_unbind_key(XEN key, XEN state, XEN cx_extended)
{
  #define H_unbind_key "(" S_unbind_key " key state :optional extended): undo the effect of a prior " S_bind_key " call."
  return(g_bind_key_1(key, state, XEN_FALSE, cx_extended, XEN_UNDEFINED, XEN_UNDEFINED, S_unbind_key));
}


static XEN g_key(XEN kbd, XEN buckybits, XEN snd, XEN chn)
{
  #define H_key "(" S_key " key modifiers :optional snd chn): simulate typing 'key' with 'modifiers' in snd's channel chn"
  chan_info *cp;
  int k, s;

  XEN_ASSERT_TYPE(XEN_INTEGER_P(kbd) || XEN_CHAR_P(kbd) || XEN_STRING_P(kbd), kbd, XEN_ARG_1, S_key, "an integer, character, or string");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(buckybits), buckybits, XEN_ARG_2, S_key, "an integer");
  ASSERT_CHANNEL(S_key, snd, chn, 3);

  cp = get_cp(snd, chn, S_key);
  if (!cp) return(XEN_FALSE);

  k = key_name_to_key(kbd, S_key);
  s = XEN_TO_C_INT(buckybits);
  check_for_key_error(k, s, S_key);
  keyboard_command(cp, k, s);

  return(kbd);
}


static XEN g_status_report(XEN msg, XEN snd)
{
  #define H_status_report "(" S_status_report " message :optional snd) posts message in snd's status area."

  snd_info *sp;
  const char *message;

  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, XEN_ARG_1, S_status_report, "a string");
  ASSERT_SOUND(S_status_report, snd, 2);

  sp = get_sp(snd);
  if ((sp == NULL) || 
      (sp->inuse != SOUND_NORMAL))
    return(snd_no_such_sound_error(S_status_report, snd));

  message = XEN_TO_C_STRING(msg);
  if ((message) && (*message))
    string_to_minibuffer(sp, message);
  else clear_minibuffer(sp);
  return(msg);
}


#if (!SND_DISABLE_DEPRECATED)
static XEN g_save_macros(XEN file)
{
  fprintf(stderr, "save-macros no longer does anything\n");
  return(XEN_FALSE);
}

static XEN g_prompt_in_minibuffer(XEN msg, XEN callback, XEN snd, XEN raw)
{
  fprintf(stderr, "prompt-in-minibuffer no longer does anything\n");
  return(XEN_FALSE);
}

static XEN g_report_in_minibuffer(XEN msg, XEN snd, XEN as_error)
{
  return(g_status_report(msg, snd));
}

static XEN g_clear_minibuffer(XEN snd)
{
  return(g_status_report(C_TO_XEN_STRING(""), snd));
}
#endif


#ifdef XEN_ARGIFY_1

XEN_ARGIFY_3(g_key_binding_w, g_key_binding)
XEN_ARGIFY_6(g_bind_key_w, g_bind_key)
XEN_ARGIFY_3(g_unbind_key_w, g_unbind_key)
XEN_ARGIFY_4(g_key_w, g_key)
XEN_ARGIFY_2(g_status_report_w, g_status_report)

#if (!SND_DISABLE_DEPRECATED)
  XEN_NARGIFY_1(g_save_macros_w, g_save_macros)
  XEN_ARGIFY_1(g_clear_minibuffer_w, g_clear_minibuffer)
  XEN_ARGIFY_3(g_report_in_minibuffer_w, g_report_in_minibuffer)
  XEN_ARGIFY_4(g_prompt_in_minibuffer_w, g_prompt_in_minibuffer)
#endif

#else

#define g_key_binding_w g_key_binding
#define g_bind_key_w g_bind_key
#define g_unbind_key_w g_unbind_key
#define g_key_w g_key
#define g_status_report_w g_status_report

#if (!SND_DISABLE_DEPRECATED)
  #define g_save_macros_w g_save_macros
  #define g_clear_minibuffer_w g_clear_minibuffer
  #define g_report_in_minibuffer_w g_report_in_minibuffer
  #define g_prompt_in_minibuffer_w g_prompt_in_minibuffer
#endif

#endif

void g_init_kbd(void)
{
  #define H_cursor_in_view     "The value for a " S_bind_key " function that moves the window so that the cursor is in the view"
  #define H_cursor_on_left     "The value for a " S_bind_key " function that moves the window so that the cursor is at the left edge"
  #define H_cursor_on_right    "The value for a " S_bind_key " function that moves the window so that the cursor is at the right edge"
  #define H_cursor_in_middle   "The value for a " S_bind_key " function that moves the window so that the cursor is in the middle"
  #define H_keyboard_no_action "The value for a " S_bind_key " function that does nothing upon return"

  XEN_DEFINE_CONSTANT(S_cursor_in_view,          CURSOR_IN_VIEW,                      H_cursor_in_view);
  XEN_DEFINE_CONSTANT(S_cursor_on_left,          CURSOR_ON_LEFT,                      H_cursor_on_left);
  XEN_DEFINE_CONSTANT(S_cursor_on_right,         CURSOR_ON_RIGHT,                     H_cursor_on_right);
  XEN_DEFINE_CONSTANT(S_cursor_in_middle,        CURSOR_IN_MIDDLE,                    H_cursor_in_middle);
  XEN_DEFINE_CONSTANT(S_keyboard_no_action,      KEYBOARD_NO_ACTION,                  H_keyboard_no_action);

  XEN_DEFINE_PROCEDURE(S_key_binding,            g_key_binding_w,            1, 2, 0, H_key_binding);
  XEN_DEFINE_PROCEDURE(S_bind_key,               g_bind_key_w,               3, 3, 0, H_bind_key);
  XEN_DEFINE_PROCEDURE(S_unbind_key,             g_unbind_key_w,             2, 1, 0, H_unbind_key);
  XEN_DEFINE_PROCEDURE(S_key,                    g_key_w,                    2, 2, 0, H_key);

  XEN_DEFINE_PROCEDURE(S_status_report,          g_status_report_w,          1, 1, 0, H_status_report);

#if (!SND_DISABLE_DEPRECATED)
  XEN_DEFINE_PROCEDURE("save-macros",            g_save_macros_w,            1, 0, 0, "obsolete");
  XEN_DEFINE_PROCEDURE("clear-minibuffer",       g_clear_minibuffer_w,       0, 1, 0, "obsolete");
  XEN_DEFINE_PROCEDURE("report-in-minibuffer",   g_report_in_minibuffer_w,   1, 2, 0, "obsolete");
  XEN_DEFINE_PROCEDURE("prompt-in-minibuffer",   g_prompt_in_minibuffer_w,   1, 3, 0, "obsolete");
#endif

#if HAVE_SCHEME
  {
    int i;
    for (i = 0; i < NUM_BUILT_IN_KEYS; i++)
      built_in_keys[i].func = XEN_FALSE;
  }
#endif
}
