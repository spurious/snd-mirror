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
  if (named_macro_ctr == named_macro_size)
    {
      int old_size;
      old_size = named_macro_size;
      named_macro_size += 16;
      if (!named_macros) named_macros = (named_macro **)CALLOC(named_macro_size, sizeof(named_macro *));
      else 
	{
	  int i;
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
  int i;
  nm = name_macro(name);
  nm->macro_size = macro_size;
  nm->cmds = (macro_cmd **)CALLOC(macro_size, sizeof(macro_cmd *));
  for (i = 0; i < macro_size; i++)
    {
      macro_cmd *mc;
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
		"  %s %d, %d\n", 
		S_key, (int)(mc->keysym), mc->state);
    }
  fprintf(fd, "end\n");
#endif
#if HAVE_SCHEME
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
#if HAVE_FORTH
  fprintf(fd, ": %s\n", nm->name);
  for (i = 0; i < nm->macro_size; i++)
    {
      mc = nm->cmds[i];
      if (mc->keysym != 0)
	fprintf(fd, 
		"  [char] %c %d %s drop\n", 
		(char)(mc->keysym), mc->state, S_key);
    }
  fprintf(fd, ";\n");
#endif
}

void save_macro_state (FILE *fd)
{
  int i;
  for (i = 0; i < named_macro_ctr; i++) 
    save_macro_1(named_macros[i], fd);
}

static bool execute_named_macro_1(chan_info *cp, const char *name, off_t count)
{
  int k;
  for (k = 0; k < named_macro_ctr; k++)
    {
      if ((named_macros[k]->name) && 
	  (strcmp(name, named_macros[k]->name) == 0))
	{
	  int i;
	  off_t j;
	  named_macro *nm;
	  nm = named_macros[k];
	  for (j = 0; j < count; j++)
	    for (i = 0; i < nm->macro_size; i++) 
	      {
		macro_cmd *mc;
		mc = nm->cmds[i];
		if (mc->keysym != 0)
		  {
		    if ((!(cp->active)) || (!(cp->sound))) return(1);
		    /* it's possible for a command in the macro sequence to close cp */
		    keyboard_command(cp, mc->keysym, mc->state);
		  }
	      }
	  return(true);
	}
    }
  return(false);
}

static void execute_named_macro(chan_info *cp, char *name, off_t count)
{
  if (!(execute_named_macro_1(cp, name, count)))
    /* not a macro...*/
    {
#if HAVE_EXTENSION_LANGUAGE
      int one_edit, i, loc, form_loc;
      XEN form, result = XEN_UNDEFINED;
      one_edit = cp->edit_ctr + 1;
      redirect_errors_to(errors_to_minibuffer, (void *)(cp->sound));
      form = string_to_form(name);
      redirect_errors_to(NULL, NULL);
      form_loc = snd_protect(form);
      for (i = 0; i < count; i++)
	result = snd_catch_any(eval_form_wrapper, (void *)form, name);
      loc = snd_protect(result);
      snd_report_result(result, name);
      snd_unprotect_at(loc);
      snd_unprotect_at(form_loc);
      if (cp->edit_ctr > one_edit)
	{
	  if (ss->deferred_regions > 0) 
	    sequester_deferred_regions(cp, one_edit - 1);
	  while (cp->edit_ctr > one_edit) backup_edit_list(cp);
	  if (cp->have_mixes) backup_mix_list(cp, one_edit);
	}
#else
      /* not sure it's possible to get here at all -- execute undefined named macro?? */
      snd_error("This version of Snd has no extension language, so there's no way to evaluate %s", name);
#endif
    }
}


/* ---------------- key bindings ---------------- */

typedef struct {
  int key; 
  int state; 
  int args; 
  XEN func; 
  bool cx_extended; /* Sun/Forte C defines "extended" somewhere */
  char *origin, *prefs_info;
  int gc_loc;
} key_entry; 

static key_entry *user_keymap = NULL;
static int keymap_size = 0;
static int keymap_top = 0;

int in_user_keymap(int key, int state, bool cx_extended)
{
  int i;
  if (keymap_top == 0) return(-1);
  for (i = 0; i < keymap_top; i++)
    if ((user_keymap[i].key == key) && 
	(user_keymap[i].state == state) &&
	(user_keymap[i].cx_extended == cx_extended) && 
	(XEN_BOUND_P(user_keymap[i].func)))
      return(i);
  return(-1);
}

#define NUM_BUILT_IN_KEY_BINDINGS 76
static key_entry built_in_key_bindings[NUM_BUILT_IN_KEY_BINDINGS] = {
  {snd_K_Down,    0, 0, XEN_FALSE, false, "zoom out",                                                    NULL, -1},
  {snd_K_Up,      0, 0, XEN_FALSE, false, "zoom in",                                                     NULL, -1},
  {snd_K_Left,    0, 0, XEN_FALSE, false, "move window left",                                            NULL, -1},
  {snd_K_Right,   0, 0, XEN_FALSE, false, "move window right",                                           NULL, -1},
  {snd_K_less,    0, 0, XEN_FALSE, false, "move cursor to sample 0",                                     NULL, -1},
  {snd_K_greater, 0, 0, XEN_FALSE, false, "move cursor to last sample",                                  NULL, -1},

  {snd_K_less,       snd_ControlMask, 0, XEN_FALSE, false, "move cursor to sample 0",                    NULL, -1},
  {snd_K_greater,    snd_ControlMask, 0, XEN_FALSE, false, "move cursor to last sample",                 NULL, -1},
  {snd_K_a,          snd_ControlMask, 0, XEN_FALSE, false, "move cursor to window start",                NULL, -1},
  {snd_K_b,          snd_ControlMask, 0, XEN_FALSE, false, "move cursor back one sample",                NULL, -1},
  {snd_K_d,          snd_ControlMask, 0, XEN_FALSE, false, "delete sample at cursor",                    NULL, -1},
  {snd_K_e,          snd_ControlMask, 0, XEN_FALSE, false, "move cursor to window end",                  NULL, -1},
  {snd_K_f,          snd_ControlMask, 0, XEN_FALSE, false, "move cursor ahead one sample",               NULL, -1},
  {snd_K_g,          snd_ControlMask, 0, XEN_FALSE, false, "abort current command",                      NULL, -1},
  {snd_K_h,          snd_ControlMask, 0, XEN_FALSE, false, "delete previous sample",                     NULL, -1},
  {snd_K_i,          snd_ControlMask, 0, XEN_FALSE, false, "display cursor info",                        NULL, -1},
  {snd_K_j,          snd_ControlMask, 0, XEN_FALSE, false, "goto mark",                                  NULL, -1},
  {snd_K_k,          snd_ControlMask, 0, XEN_FALSE, false, "delete one line's worth of samples",         NULL, -1},
  {snd_K_l,          snd_ControlMask, 0, XEN_FALSE, false, "position window so cursor is in the middle", NULL, -1},
  {snd_K_m,          snd_ControlMask, 0, XEN_FALSE, false, "place (or remove) mark at cursor location",  NULL, -1},
  {snd_K_n,          snd_ControlMask, 0, XEN_FALSE, false, "move cursor ahead one 'line'",               NULL, -1},
  {snd_K_o,          snd_ControlMask, 0, XEN_FALSE, false, "insert one zero sample at cursor",           NULL, -1},
  {snd_K_p,          snd_ControlMask, 0, XEN_FALSE, false, "move cursor back one 'line'",                NULL, -1},
  {snd_K_q,          snd_ControlMask, 0, XEN_FALSE, false, "play current channel starting at cursor",    "play-channel-from-cursor", -1},
  {snd_K_r,          snd_ControlMask, 0, XEN_FALSE, false, "search backwards",                           NULL, -1},
  {snd_K_s,          snd_ControlMask, 0, XEN_FALSE, false, "search forwards",                            NULL, -1},
  {snd_K_t,          snd_ControlMask, 0, XEN_FALSE, false, "stop playing",                               NULL, -1},
  {snd_K_u,          snd_ControlMask, 0, XEN_FALSE, false, "start arg count definition.",                NULL, -1},
  {snd_K_v,          snd_ControlMask, 0, XEN_FALSE, false, "move cursor to mid-window",                  NULL, -1},
  {snd_K_w,          snd_ControlMask, 0, XEN_FALSE, false, "delete selection",                           "delete-selection", -1},
  {snd_K_y,          snd_ControlMask, 0, XEN_FALSE, false, "insert selection.",                          "insert-selection", -1},
  {snd_K_z,          snd_ControlMask, 0, XEN_FALSE, false, "set sample at cursor to 0.0",                NULL, -1},
  {snd_K_underscore, snd_ControlMask, 0, XEN_FALSE, false, "undo",                                       NULL, -1},
  {snd_K_space,      snd_ControlMask, 0, XEN_FALSE, false, "start selection definition",                 NULL, -1},
  {snd_K_g,          snd_ControlMask | snd_MetaMask, 0, XEN_FALSE, false, "clear listener",              NULL, -1},
  {snd_K_less,       snd_MetaMask,    0, XEN_FALSE, false, "move cursor to sample 0",                    NULL, -1},
  {snd_K_greater,    snd_MetaMask,    0, XEN_FALSE, false, "move cursor to last sample",                 NULL, -1},

  {snd_K_a,          0, 0, XEN_FALSE, true, "apply envelope to selection",                               NULL, -1},
  {snd_K_b,          0, 0, XEN_FALSE, true, "position window so cursor is on left margin",               NULL, -1},
  {snd_K_c,          0, 0, XEN_FALSE, true, "define selection from cursor to nth mark",                  NULL, -1},
  {snd_K_d,          0, 0, XEN_FALSE, true, "set temp dir name",                                         NULL, -1},
  {snd_K_e,          0, 0, XEN_FALSE, true, "execute keyboard macro",                                    NULL, -1},
  {snd_K_f,          0, 0, XEN_FALSE, true, "position window so cursor is on right margin",              NULL, -1},
  {snd_K_i,          0, 0, XEN_FALSE, true, "insert region",                                             "insert-selection", -1},
  {snd_K_j,          0, 0, XEN_FALSE, true, "goto named mark",                                           NULL, -1},
  {snd_K_k,          0, 0, XEN_FALSE, true, "close file",                                                NULL, -1},
  {snd_K_l,          0, 0, XEN_FALSE, true, "position selection in mid-view",                            NULL, -1},
  {snd_K_o,          0, 0, XEN_FALSE, true, "move to next or previous graph",                            NULL, -1},
  {snd_K_p,          0, 0, XEN_FALSE, true, "play selection or region n",                                "play-selection", -1},
  {snd_K_q,          0, 0, XEN_FALSE, true, "mix in selection",                                          "mix-selection", -1},
  {snd_K_r,          0, 0, XEN_FALSE, true, "redo",                                                      NULL, -1},
  {snd_K_u,          0, 0, XEN_FALSE, true, "undo",                                                      NULL, -1},
  {snd_K_v,          0, 0, XEN_FALSE, true, "position window over selection",                            NULL, -1},
  {snd_K_w,          0, 0, XEN_FALSE, true, "save selection as file",                                    NULL, -1},
  {snd_K_z,          0, 0, XEN_FALSE, true, "smooth selection",                                          NULL, -1},
  {snd_K_slash,      0, 0, XEN_FALSE, true, "place named mark",                                          NULL, -1},
  {snd_K_openparen,  0, 0, XEN_FALSE, true, "begin keyboard macro definition",                           NULL, -1},
  {snd_K_closeparen, 0, 0, XEN_FALSE, true, "end keyboard macro definition",                             NULL, -1},

  {snd_K_a, snd_ControlMask, 0, XEN_FALSE, true, "apply envelope",                                       NULL, -1},
  {snd_K_b, snd_ControlMask, 0, XEN_FALSE, true, "set x window bounds (preceded by 1 arg)",              NULL, -1},
  {snd_K_c, snd_ControlMask, 0, XEN_FALSE, true, "hide control panel",                                   NULL, -1},
  {snd_K_d, snd_ControlMask, 0, XEN_FALSE, true, "print",                                                NULL, -1},
  {snd_K_e, snd_ControlMask, 0, XEN_FALSE, true, "give last keyboard macro a name",                      NULL, -1},
  {snd_K_f, snd_ControlMask, 0, XEN_FALSE, true, "open file",                                            NULL, -1},
  {snd_K_g, snd_ControlMask, 0, XEN_FALSE, true, "abort command",                                        NULL, -1},
  {snd_K_i, snd_ControlMask, 0, XEN_FALSE, true, "insert file",                                          NULL, -1},
  {snd_K_m, snd_ControlMask, 0, XEN_FALSE, true, "add named mark",                                       NULL, -1},
  {snd_K_o, snd_ControlMask, 0, XEN_FALSE, true, "show control panel",                                   NULL, -1},
  {snd_K_p, snd_ControlMask, 0, XEN_FALSE, true, "set window size (preceded by 1 arg)",                  NULL, -1},
  {snd_K_q, snd_ControlMask, 0, XEN_FALSE, true, "mix in file",                                          NULL, -1},
  {snd_K_r, snd_ControlMask, 0, XEN_FALSE, true, "redo",                                                 "save-sound", -1},
  {snd_K_s, snd_ControlMask, 0, XEN_FALSE, true, "save file",                                            NULL, -1},
  {snd_K_u, snd_ControlMask, 0, XEN_FALSE, true, "undo",                                                 NULL, -1},
  {snd_K_v, snd_ControlMask, 0, XEN_FALSE, true, "set window size as percentage of total",               NULL, -1},
  {snd_K_w, snd_ControlMask, 0, XEN_FALSE, true, "save current channel in file",                         NULL, -1},
  {snd_K_z, snd_ControlMask, 0, XEN_FALSE, true, "smooth using cosine",                                  NULL, -1},
};

void map_over_key_bindings(bool (*func)(int key, int state, bool cx, char *pinfo, XEN xf))
{
  int i;
  for (i = 0; i < keymap_top; i++)
    if ((XEN_BOUND_P(user_keymap[i].func)) &&
	((*func)(user_keymap[i].key, 
		 user_keymap[i].state, 
		 user_keymap[i].cx_extended, 
		 user_keymap[i].prefs_info, 
		 user_keymap[i].func)))
      return;
}

static key_info *make_key_info(key_entry k)
{
  key_info *ki;
  ki = (key_info *)CALLOC(1, sizeof(key_info));
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

key_info *find_prefs_key_binding(const char *prefs_name)
{
  int i;
  key_info *ki;
  for (i = 0; i < keymap_top; i++)
    if ((XEN_BOUND_P(user_keymap[i].func)) &&
	(user_keymap[i].prefs_info) &&
	(strcmp(user_keymap[i].prefs_info, prefs_name) == 0))
      return(make_key_info(user_keymap[i]));

  for (i = 0; i < NUM_BUILT_IN_KEY_BINDINGS; i++)
    if ((built_in_key_bindings[i].prefs_info) &&
	(strcmp(built_in_key_bindings[i].prefs_info, prefs_name) == 0))
      return(make_key_info(built_in_key_bindings[i]));

  ki = (key_info *)CALLOC(1, sizeof(key_info));
  ki->key = NULL;
  ki->c = false;
  ki->m = false;
  ki->x = false;
  return(ki);
}

char *key_binding_description(int key, int state, bool cx_extended)
{
  int pos;
  if ((key < MIN_KEY_CODE) || (key > MAX_KEY_CODE) ||
      (state < MIN_KEY_STATE) || (state > MAX_KEY_STATE))
    return(NULL);
  pos = in_user_keymap(key, state, cx_extended);
  if (pos < 0) pos = in_user_keymap(key, state, cx_extended);
  if (pos >= 0)
    {
#if HAVE_GUILE
      XEN value, help_text = XEN_FALSE;
      value = user_keymap[pos].func;
      help_text = XEN_PROCEDURE_HELP(value);            /* (procedure-property ...) */
      if (XEN_FALSE_P(help_text))
	{
	  help_text = XEN_PROCEDURE_SOURCE_HELP(value); /* (procedure-documentation ...) -- this is the first line of source if string */
	  if (XEN_FALSE_P(help_text))
	    help_text = XEN_PROCEDURE_NAME(value);
	}
      if (!(XEN_FALSE_P(help_text))) 
	{
	  if (XEN_STRING_P(help_text))
	    return(XEN_TO_C_STRING(help_text)); /* this is dangerous -- these are temp strings, but they're used as temps in snd-help */
	  return(XEN_AS_STRING(help_text));
	}
#endif
      if (user_keymap[pos].origin)
	return(user_keymap[pos].origin);
      return("something indescribable"); /* NULL would mean "no binding" */
    }
  for (pos = 0; pos < NUM_BUILT_IN_KEY_BINDINGS; pos++)
    if ((built_in_key_bindings[pos].key == key) && 
	(built_in_key_bindings[pos].state == state) && 
	(built_in_key_bindings[pos].cx_extended == cx_extended))
      return(built_in_key_bindings[pos].origin);
  return(NULL);
}

void set_keymap_entry(int key, int state, int args, XEN func, bool cx_extended, const char *origin, const char *prefs_info)
{
  int i;
  i = in_user_keymap(key, state, cx_extended);
  if (i == -1)
    {
      if (keymap_size == keymap_top)
	{
	  keymap_size += 16;
	  if (keymap_top == 0)
	    {
	      user_keymap = (key_entry *)CALLOC(keymap_size, sizeof(key_entry));
	      for (i = 0; i < keymap_size; i++) 
		user_keymap[i].func = XEN_UNDEFINED;
	    }
	  else 
	    {
	      user_keymap = (key_entry *)REALLOC(user_keymap, keymap_size * sizeof(key_entry));
	      for (i = keymap_top; i < keymap_size; i++) 
		{
		  user_keymap[i].key = 0; 
		  user_keymap[i].state = 0; 
		  user_keymap[i].func = XEN_UNDEFINED;
		  user_keymap[i].cx_extended = false;
		  user_keymap[i].origin = NULL;
		  user_keymap[i].prefs_info = NULL;
		  user_keymap[i].gc_loc = NOT_A_GC_LOC;
		}
	    }
	}
      user_keymap[keymap_top].key = key;
      user_keymap[keymap_top].state = state;
      user_keymap[keymap_top].cx_extended = cx_extended;
      check_menu_labels(key, state, cx_extended);
      i = keymap_top;
      keymap_top++;
    }
  else
    {
      if ((XEN_PROCEDURE_P(user_keymap[i].func)) &&
	  (user_keymap[i].gc_loc != NOT_A_GC_LOC))
	{
	  snd_unprotect_at(user_keymap[i].gc_loc);
	  user_keymap[i].gc_loc = NOT_A_GC_LOC;
	}
      if (user_keymap[i].origin)
	{
	  FREE(user_keymap[i].origin);
	  user_keymap[i].origin = NULL;
	}
      if (user_keymap[i].prefs_info)
	{
	  FREE(user_keymap[i].prefs_info);
	  user_keymap[i].prefs_info = NULL;
	}
    }
  user_keymap[i].origin = copy_string(origin);
  user_keymap[i].prefs_info = copy_string(prefs_info);
  user_keymap[i].args = args;
  user_keymap[i].func = func;
  if (XEN_PROCEDURE_P(func)) 
    user_keymap[i].gc_loc = snd_protect(func);
}

static void call_user_keymap(int hashedsym, int count)
{
  kbd_cursor_t res = KEYBOARD_NO_ACTION;
  /* if guile call the associated scheme code, else see if basic string parser can handle it */
  if (XEN_BOUND_P(user_keymap[hashedsym].func))
    {
      /* not _NO_CATCH here because the code is not protected at any higher level */
      if (user_keymap[hashedsym].args == 0)
	res = (kbd_cursor_t)XEN_TO_C_INT_OR_ELSE(XEN_CALL_0(user_keymap[hashedsym].func, 
							    user_keymap[hashedsym].origin), 
						 KEYBOARD_NO_ACTION);
      else res = (kbd_cursor_t)XEN_TO_C_INT_OR_ELSE(XEN_CALL_1(user_keymap[hashedsym].func, 
							       C_TO_XEN_INT(count), 
							       user_keymap[hashedsym].origin),
						    KEYBOARD_NO_ACTION);
    }
  handle_cursor(selected_channel(), res);
}

/* ---------------- minibuffer ---------------- */

void string_to_minibuffer(snd_info *sp, const char *buf)
{
  if ((sp->minibuffer_on == MINI_PROMPT) || 
      (sp->minibuffer_on == MINI_USER) ||
      (sp->minibuffer_on == MINI_FIND))
    display_minibuffer_error(sp, buf); /* leave the prompt alone */
  else
    {
      clear_minibuffer_prompt(sp);
      set_minibuffer_string(sp, (char *)buf, true);
      sp->minibuffer_on = MINI_REPORT;
    }
}

void report_in_minibuffer(snd_info *sp, const char *format, ...)
{
  char *buf;
  va_list ap;
  if ((!sp) || (!(sp->active)) || (!(sp->sgx)) || (sp->inuse != SOUND_NORMAL)) return;
  va_start(ap, format);
  buf = vstr(format, ap);
  va_end(ap);
  string_to_minibuffer(sp, buf);
  FREE(buf);
}

void clear_minibuffer(snd_info *sp)
{
  clear_minibuffer_prompt(sp);
  set_minibuffer_string(sp, NULL, true);
  sp->search_count = 0;
  sp->marking = 0;
  sp->filing = NOT_FILING;
  sp->printing = NOT_PRINTING;
  sp->minibuffer_on = MINI_OFF;
  sp->loading = false;
  sp->amp_count = 0;
  sp->macro_count = 0;
  sp->prompting = false;
}

void clear_minibuffer_prompt(snd_info *sp)
{
  make_minibuffer_label(sp, "     ");
}

static void prompt(snd_info *sp, char *msg, char *preload)
{
  if (preload)
    {
      set_minibuffer_string(sp, preload, true);
      set_minibuffer_cursor_position(sp, snd_strlen(preload));
    }
  else
    set_minibuffer_string(sp, NULL, true);
  make_minibuffer_label(sp, msg);
  sp->minibuffer_on = MINI_PROMPT;
  goto_minibuffer(sp);
}

static void get_amp_expression(snd_info *sp, int count, bool over_selection) 
{
  prompt(sp, _("env:"), NULL); 
  sp->amp_count = count; 
  sp->selectioning = over_selection;
}

static void prompt_named_mark(chan_info *cp) 
{
  snd_info *sp = cp->sound;
  clear_minibuffer(sp);
  make_minibuffer_label(sp, _("mark:"));
  sp->minibuffer_on = MINI_PROMPT;
  goto_minibuffer(sp);
  sp->marking = CURSOR(cp) + 1; /*  + 1 so it's not confused with 0 (if (sp->marking)...) */
}

void errors_to_minibuffer(const char *msg, void *data)
{
  display_minibuffer_error((snd_info *)data, msg);
}

void printout_to_minibuffer(const char *msg, void *data)
{
  string_to_minibuffer((snd_info *)data, msg);
}


#if HAVE_DIRENT_H
  #include <dirent.h>
#endif

void snd_minibuffer_activate(snd_info *sp, int keysym, bool with_meta)
{
  bool s_or_r = false;
  chan_info *active_chan;
  static char *str = NULL;
  if (str) /* leftover from previous call */
    {
#if USE_MOTIF
      XtFree(str);
#else
#if USE_GTK
      FREE(str);
#endif
#endif
      str = NULL;
    }
  if ((keysym == snd_K_s) || (keysym == snd_K_r)) s_or_r = true;
  if (sp != selected_sound()) select_channel(sp, 0);
  active_chan = any_selected_channel(sp);
  if (active_chan)
    {
      goto_graph(active_chan);
    }
  if ((keysym == snd_K_g) || (keysym == snd_K_G)) /* c-g => abort whatever we're doing and return */
    {
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
  /* sp->minibuffer_on = MINI_REPORT; */

#if HAVE_EXTENSION_LANGUAGE
  if (sp->search_count != 0)
    {
      /* it's the search expr request */
      /* if not nil, replace previous */
      if (!s_or_r)
	{
	  if ((str) && (*str))
	    {
	      XEN proc;
	      /* check for procedure as arg, or lambda form:
	       * (lambda (y) (> y .1)) 
	       * if returns #t, search stops
	       *
	       * if error in scheme, don't go ahead with the search!
	       */
	      clear_sound_search_procedure(sp, true);
	      sp->search_expr = copy_string(str);
	      redirect_errors_to(errors_to_minibuffer, (void *)sp);
	      proc = snd_catch_any(eval_str_wrapper, str, str);
	      if (XEN_PROCEDURE_P(proc)) /* redundant but avoids unwanted error message via snd_error */
		{
		  char *errmsg;
		  errmsg = procedure_ok(proc, 1, _("find"), _("find"), 1);
		  if (errmsg)
		    {
		      snd_error_without_format(errmsg);
		      FREE(errmsg);
		    }
		  else
		    {
		      sp->search_proc = proc;
		      sp->search_proc_loc = snd_protect(proc);
		    }
		}
	      else active_chan = NULL; /* don't try to search! */
	      redirect_errors_to(NULL, NULL);
	      if (active_chan) active_chan->last_search_result = SEARCH_OK;
	    }
	}

      if (active_chan)
	cursor_search(active_chan, sp->search_count);
      return;
    }
#endif
  sp->minibuffer_on = MINI_REPORT; 
  if ((sp->marking) || (sp->finding_mark))
    {
      if (sp->marking) 
	{
	  mark *m;
	  m = add_mark(sp->marking - 1, str, active_chan);
	  if (m)
	    {
	      report_in_minibuffer(sp, _("%s placed at sample " OFF_TD), str, sp->marking - 1);
	      display_channel_marks(active_chan);
	    }
	  else report_in_minibuffer(sp, _("There is already a mark at sample " OFF_TD), sp->marking - 1);
	  sp->marking = 0;
	}	
      else 
	{
	  goto_named_mark(active_chan, str);
	  sp->finding_mark = false;
	}
      return;
    }
  if (snd_strlen(str) != 0)
    {
      if (sp->printing)
	{
	  snd_print(str);
	  sp->printing = NOT_PRINTING;
	  clear_minibuffer(sp);
	  return;
	}
      if (sp->loading)
	{
	  snd_load_file(str);
	  sp->loading = false;
	  clear_minibuffer(sp);
	  return;
	}
      if (sp->filing != NOT_FILING)
	{
	  switch (sp->filing)
	    {

	      /* open file */
	    case INPUT_FILING:
	      if (str)
		{
		  char *filename;
		  snd_info *nsp;
		  filename = mus_expand_filename(str);
		  if (mus_file_probe(filename))
		    {
		      ss->open_requestor = FROM_KEYBOARD;
#if (!USE_NO_GUI)
		      ss->sgx->requestor_dialog = NULL;
#endif
		      ss->open_requestor_data = (void *)sp;
		      nsp = snd_open_file(str, FILE_READ_WRITE);
		    }
		  else
		    {
		      /* C-x C-f <name-of-nonexistent-file> -> open new sound */
		      nsp = snd_new_file(filename, 
					 default_output_header_type(ss),
					 default_output_data_format(ss),
					 default_output_srate(ss),
					 default_output_chans(ss),
					 NULL, 1); /* at least 1 sample needed for new sound data buffer creation */
		      /* now should this file be deleted upon exit?? */
		    }
		  FREE(filename);
		  if (nsp) 
		    {
		      select_channel(nsp, 0);
		      clear_minibuffer(sp);
		    }
		  else snd_warning("can't open %s!", str);
		}
	      /* C-x C-f <cr> is no-op (emacs) */
	      break;

	      /* save selection */
	    case DOIT_SELECTION_FILING: /* if user responded to prompt about overwriting */
	      /* clear prompt and text widget without clearing all the fields (like sp->filing!) */
	      clear_minibuffer_prompt(sp);
	      set_minibuffer_string(sp, NULL, true);
	      sp->minibuffer_on = MINI_OFF;
	      if (STRCMP(str, "yes") != 0)
		{
		  if (sp->filing_filename)
		    {
		      FREE(sp->filing_filename);
		      sp->filing_filename = NULL;
		    }
		  string_to_minibuffer(sp, _("selection not saved"));
		  sp->filing = NOT_FILING;
		  return;
		}
	      /* else fall through... */
	    case SELECTION_FILING:
	      if (selection_is_active())
		{
		  io_error_t io_err;
		  char *filename = NULL;
		  if (sp->filing == SELECTION_FILING)
		    {
		      clear_minibuffer_prompt(sp);
		      set_minibuffer_string(sp, NULL, true);
		      sp->minibuffer_on = MINI_OFF;
		      filename = mus_expand_filename(str);
		      if ((ask_before_overwrite(ss)) && 
			  (mus_file_probe(filename)))
			{
			  /* ask user whether to go on. */
			  char *ques;
			  ques = mus_format(_("%s exists: overwrite?"), str);
			  prompt(sp, ques, NULL);
			  FREE(ques);
			  sp->filing_filename = filename;
			  sp->filing = DOIT_SELECTION_FILING;
			  return;
			}
		    }
		  else 
		    {
		      filename = sp->filing_filename;
		      sp->filing_filename = NULL;
		    }
		  io_err = save_selection(filename,
					  default_output_header_type(ss), 
					  default_output_data_format(ss), 
					  SND_SRATE(sp), NULL, SAVE_ALL_CHANS);
		  if (io_err == IO_NO_ERROR)
		    report_in_minibuffer(sp, "selection saved as %s", filename);
		  else report_in_minibuffer(sp, "selection not saved: %s %s", 
					    io_error_name(io_err), 
					    filename);
		  FREE(filename);
		}
	      else string_to_minibuffer(sp, _("no selection to save"));
	      sp->filing = NOT_FILING;
	      break;

	      /* save channel -- the same loop as selection case above */
	    case DOIT_CHANNEL_FILING:
	      clear_minibuffer_prompt(sp);
	      set_minibuffer_string(sp, NULL, true);
	      sp->minibuffer_on = MINI_OFF;
	      if (STRCMP(str, "yes") != 0)
		{
		  if (sp->filing_filename)
		    {
		      FREE(sp->filing_filename);
		      sp->filing_filename = NULL;
		    }
		  string_to_minibuffer(sp, _("channel not saved"));
		  sp->filing = NOT_FILING;
		  return;
		}
	      /* else fall through... */
	    case CHANNEL_FILING:
	      {
		io_error_t io_err;
		char *filename = NULL;
		if (sp->filing == CHANNEL_FILING)
		  {
		    clear_minibuffer_prompt(sp);
		    set_minibuffer_string(sp, NULL, true);
		    sp->minibuffer_on = MINI_OFF;
		    filename = mus_expand_filename(str);
		    if ((ask_before_overwrite(ss)) && 
			(mus_file_probe(filename)))
		      {
			/* ask user whether to go on. */
			char *ques;
			ques = mus_format(_("%s exists: overwrite?"), str);
			prompt(sp, ques, NULL);
			FREE(ques);
			sp->filing_filename = filename;
			sp->filing = DOIT_CHANNEL_FILING;
			return;
		      }
		  }
		else 
		  {
		    filename = sp->filing_filename;
		    sp->filing_filename = NULL;
		  }
		io_err = save_channel_edits(active_chan, filename, AT_CURRENT_EDIT_POSITION);
		if (io_err == IO_NO_ERROR)
		  report_in_minibuffer(sp, _("channel %d saved as %s"), 
				       active_chan->chan,
				       filename);
		else string_to_minibuffer(sp, _("channel not saved"));
		if (filename) FREE(filename);
		sp->filing = NOT_FILING;
	      }
	      break;

#if HAVE_OPENDIR
	      /* set temp-dir */
	    case TEMP_FILING:
	      {
		DIR *dp;
		char *newdir;
		newdir = copy_string(str);
		clear_minibuffer(sp);
		dp = opendir(newdir);
		if (dp) 
		  {
		    closedir(dp);
		    if (temp_dir(ss)) FREE(temp_dir(ss));
		    set_temp_dir(newdir);
		  }
		else 
		  {
		    char *msg;
		    msg = mus_format(_("can't access %s! temp dir is unchanged"), newdir);
		    display_minibuffer_error(sp, msg);
		    FREE(msg);
		    if (newdir) FREE(newdir);
		  }
	      }
	      break;
#endif

	      /* mix file */
	    case CHANGE_FILING:
	      {
		int id_or_error;
		clear_minibuffer(sp);
		redirect_errors_to(errors_to_minibuffer, (void *)sp);
		id_or_error = mix_complete_file_at_cursor(sp, str, with_mix_tags(ss), 0);
		redirect_errors_to(NULL, NULL);
		if (id_or_error >= 0)
		  report_in_minibuffer(sp, _("%s mixed in at cursor"), str);
	      }
	      break;

	      /* insert file */
	    case INSERT_FILING:
	      {
		int err;
		clear_minibuffer(sp);
		redirect_errors_to(errors_to_minibuffer, (void *)sp);
		err = insert_complete_file_at_cursor(sp, str);
		redirect_errors_to(NULL, NULL);
		if (err == 0)
		  report_in_minibuffer(sp, _("%s inserted at cursor"), str);
	      }
	      break;

	      /* execute macro */
	    case MACRO_FILING: 
	      if ((macro_cmds) && (macro_size > 0))
		{
		  name_last_macro(str); 
		  clear_minibuffer(sp); 
		}
	      else string_to_minibuffer(sp, _("no previous macro"));
	      break;
	    case SAVE_EDITS_FILING:
	      if ((str[0] == 'y') ||
		  (str[0] == 'Y'))
		{
		  sp->need_update = false;
		  clear_minibuffer_error(sp);
		  save_edits_and_update_display(sp);
		}
	      clear_minibuffer(sp);
	      break;
	    default:
	      break;
	    }
	  sp->filing = NOT_FILING;
	  return;
	}
      if (sp->amp_count != 0)
	{
	  env *e;
	  if (!active_chan) active_chan = sp->chans[0];
	  redirect_errors_to(errors_to_minibuffer, (void *)sp);
	  e = string_to_env(str);
	  if (e)
	    {
	      if (sp->amp_count != 1)
		apply_env(active_chan, e, CURSOR(active_chan), 
			  sp->amp_count, sp->selectioning, NOT_FROM_ENVED,
			  (char *)((sp->selectioning) ? "C-x a" : "C-x C-a"), NULL,
			  C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0);
	      else apply_env(active_chan, e, 0, CURRENT_SAMPLES(active_chan),
			     sp->selectioning, NOT_FROM_ENVED,
			     (char *)((sp->selectioning) ? "C-x a" : "C-x C-a"), NULL,
			     C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 0);
	      e = free_env(e);
	    }
	  redirect_errors_to(NULL, NULL);
	  sp->selectioning = false;
	  sp->amp_count = 0;
	  clear_minibuffer(sp);
	  return;
	}
#if HAVE_EXTENSION_LANGUAGE
      if (sp->macro_count)
	{
	  redirect_snd_print_to(printout_to_minibuffer, (void *)sp);
	  redirect_errors_to(errors_to_minibuffer, (void *)sp);
	  execute_named_macro(active_chan, str, sp->macro_count);
	  /* if this is a close command from the current minibuffer, the sound may not exist when we return */
	  redirect_everything_to(NULL, NULL);
	  if (sp == NULL) return;
	  sp->macro_count = 0;
	  return;
	}
#endif
    }
#if HAVE_EXTENSION_LANGUAGE
  /* strlen can be 0 here if <cr> response to prompt */
  if (sp->prompting)
    {
      int loc;
      if (snd_strlen(str) > 0)
	{
	  XEN proc;
	  redirect_snd_print_to(printout_to_minibuffer, (void *)sp);
	  redirect_errors_to(errors_to_minibuffer, (void *)sp);
	  if (sp->raw_prompt)
	    proc = C_TO_XEN_STRING(str);
	  else proc = snd_catch_any(eval_str_wrapper, str, str);
	  if (XEN_PROCEDURE_P(sp->prompt_callback))
	    {
	      loc = snd_protect(proc);
	      XEN_CALL_1(sp->prompt_callback, proc, "prompt callback func");
	      snd_unprotect_at(loc);
	    }
	  redirect_everything_to(NULL, NULL);
	}
      sp->prompting = false;
      sp->minibuffer_on = MINI_REPORT;
      clear_minibuffer_prompt(sp);
      return;
    }
#endif
  if (snd_strlen(str) > 0)
    {
      redirect_snd_print_to(printout_to_minibuffer, (void *)sp);
      redirect_errors_to(errors_to_minibuffer, (void *)sp);
      snd_report_result(snd_catch_any(eval_str_wrapper, (void *)str, str), str);
      redirect_everything_to(NULL, NULL);
      sp->selectioning = false;
    }
  else clear_minibuffer(sp);
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
#if USE_MOTIF
  equalize_sound_panes(ncp->sound, ncp, false);
#endif
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
#if USE_MOTIF
  equalize_sound_panes(ncp->sound, ncp, false);
#endif
  return(ncp);
}

void save_edits_with_prompt(snd_info *sp)
{
  io_error_t err;
  redirect_everything_to(printout_to_minibuffer, (void *)sp);
  err = save_edits(sp); 
  redirect_everything_to(NULL, NULL);
  if (err == IO_NEED_WRITE_CONFIRMATION)
    {
      prompt(sp, _("file has changed; overwrite anyway?"), NULL); 
      sp->filing = SAVE_EDITS_FILING; 
    }
}


/* ---------------- key response ---------------- */

static off_t get_count_1(char *number_buffer, int number_ctr, bool dot_seen, chan_info *cp)
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
      return((off_t)(f * SND_SRATE(cp->sound)));
    }
  if (!(sscanf(number_buffer, "%d", &i)))
    {
      snd_error("invalid number: %s", number_buffer);
      return(0);
    }
  return(i);
}

static off_t get_count(char *number_buffer, int number_ctr, bool dot_seen, chan_info *cp, bool mark_wise)
{
  off_t val, old_cursor;
  val = get_count_1(number_buffer, number_ctr, dot_seen, cp);
  if (!mark_wise) return(val);
  old_cursor = CURSOR(cp);
  if (!(goto_mark(cp, val)))
    string_to_minibuffer(cp->sound, _("no such mark"));
  val = CURSOR(cp) - old_cursor; /* will be 0 if no relevant marks */
  CURSOR(cp) = old_cursor;
  return(val);
}

#define NUMBER_BUFFER_SIZE 12

static Float state_amount(int state)
{
  Float amount;
  amount = 1.0;
  if (state & snd_ControlMask) amount *= 0.5;
  if (state & snd_MetaMask) amount *= 0.5;
  if (state & snd_ShiftMask) amount *= 0.5;
  return(amount);
}


static bool stop_selecting(int keysym, int state)
{
  return(((state & snd_ControlMask) == 0) ||
	 (keysym == snd_K_D) || (keysym == snd_K_d) ||
	 (keysym == snd_K_H) || (keysym == snd_K_h) ||
	 (keysym == snd_K_Y) || (keysym == snd_K_y));
}

static char *key_to_name(int keysym) 
{
  if (keysym) 
    {
      char *str;
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
  number_ctr = 0; 
  counting = false; 
  dot_seen = false; 
  extended_mode = false;
  if (selection_is_active()) deactivate_selection();
  defining_macro = false;
  clear_stdin();
  redirect_everything_to(NULL, NULL);
  if ((ss->checking_explicitly) || (play_in_progress())) ss->stopped_explicitly = true; 
  /* this tries to break out of long filter/src computations (and perhaps others) */
  /*   but, as in other such cases, it leaves this flag set so all subsequent uses of it need to clear it first */
  stop_playing_all_sounds(PLAY_C_G); /* several scm files assume hooks called upon C-g -- could be region play, etc */
  if (sp)
    {
      if (sp->applying) stop_applying(sp);
      for_each_sound_chan(sp, stop_fft_in_progress);
      clear_minibuffer(sp);
    }
  ss->error_lock = false;
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
  static off_t count = 1;
  static bool got_count = false;
  static bool m = false;
  int shift = 0;
  bool dont_clear_minibuffer = false, cursor_searching = false, clear_search = true;
  int hashloc, i, state;
  off_t loc;
  static off_t ext_count = 1;
  static bool got_ext_count = false;
  snd_info *sp;
  axis_info *ap;
  /* fprintf(stderr, "(%s %d%s) ", KEY_TO_NAME(keysym), unmasked_state, (extended_mode) ? " (c-x)" : ""); */
  if ((!cp) || (!(cp->sound)) || (!(cp->active))) return;
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
  if ((keysym != snd_K_X) && (keysym != snd_K_x))
    {
      got_count = false;
      if (count == 0) return;
    }
#if HAVE_EXTENSION_LANGUAGE
  if ((state & snd_MetaMask) && 
      ((keysym == snd_K_X) || (keysym == snd_K_x)))
    {
      /* named macros invoked and saved here */
      prompt(sp, "M-x:", NULL);
      sp->macro_count = count;
      return;
    }
  hashloc = in_user_keymap(keysym, state, extended_mode);
  if (hashloc != -1)                       /* found user-defined key */
    {
      extended_mode = false;
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
	      cp->cursor_on = true; 
	      loc = (off_t)(ap->x0 * SND_SRATE(sp)); 
	      if ((loc + 1) == ap->losamp) loc = ap->losamp; /* handle dumb rounding problem */
	      cursor_moveto(cp, loc); 
	      break;
	    case snd_K_B: case snd_K_b: 
	      cp->cursor_on = true; 
	      cursor_move(cp, -count); 
	      break;
	    case snd_K_D: case snd_K_d: 
	      cp->cursor_on = true; 
	      cursor_delete(cp, count); 
	      break;
	    case snd_K_E: case snd_K_e:
	      cp->cursor_on = true; 
	      loc = (off_t)(ap->x1 * (double)SND_SRATE(sp));
	      if ((loc + 1) == ap->hisamp) loc = ap->hisamp;
	      cursor_moveto(cp, loc); 
	      break;
	    case snd_K_F: case snd_K_f:
	      cp->cursor_on = true; 
	      cursor_move(cp, count); 
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
	      dont_clear_minibuffer = true; 
	      break;
	    case snd_K_J: case snd_K_j: 
	      cp->cursor_on = true; 
	      if (!(goto_mark(cp, count)))
		string_to_minibuffer(cp->sound, _("no such mark"));
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
		      report_in_minibuffer(cp->sound, _("no mark at sample " OFF_TD), CURSOR(cp));
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
				report_in_minibuffer(cp->sound, _("no mark at sample " OFF_TD), CURSOR(cp));
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
	    case snd_K_R: case snd_K_r: 
	      cp->cursor_on = true; 
	      cursor_search(cp, -count); 
	      dont_clear_minibuffer = true; 
	      cursor_searching = true; 
	      break;
	    case snd_K_S: case snd_K_s: 
	      cp->cursor_on = true; 
	      cursor_search(cp, count); 
	      dont_clear_minibuffer = true; 
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
	      cursor_moveto(cp, (off_t)((ap->losamp + ap->hisamp) / 2));
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
		  report_in_minibuffer(sp, _("selection starts at " OFF_TD), CURSOR(cp));
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
	    case snd_keypad_Left: 
	      set_spectro_y_angle(spectro_y_angle(ss) - 1.0);
	      reflect_spectro(); 
	      break;
	    case snd_keypad_Right: 
	      set_spectro_y_angle(spectro_y_angle(ss) + 1.0);
	      reflect_spectro(); 
	      break;
	    case snd_keypad_Down: 
	      set_spectro_x_angle(spectro_x_angle(ss) - 1.0);
	      reflect_spectro(); 
	      break;
	    case snd_keypad_Up: 
	      set_spectro_x_angle(spectro_x_angle(ss) + 1.0);
	      reflect_spectro(); 
	      break;
	    default:
	      report_in_minibuffer(sp, _("C-%s undefined"), key_to_name(keysym));
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
	    case snd_K_A: case snd_K_a: 
	      get_amp_expression(sp, ext_count, OVER_SOUND);
	      dont_clear_minibuffer = true; 
	      break;
	    case snd_K_B: case snd_K_b: 
	      set_window_bounds(cp, ext_count); 
	      break;
	    case snd_K_C: case snd_K_c: 
	      hide_controls(sp); 
	      break;
	    case snd_K_D: case snd_K_d: 
	      prompt(sp, _("eps file:"), NULL); 
	      sp->printing = ((ext_count != 0) ? PRINTING : NOT_PRINTING);
	      dont_clear_minibuffer = true; 
	      break;
	    case snd_K_E: case snd_K_e: 
	      if (macro_size == 0)
		string_to_minibuffer(sp, _("no macro active?"));
	      else
		{
		  prompt(sp, _("macro name:"), NULL); 
		  sp->filing = MACRO_FILING; 
		  dont_clear_minibuffer = true; 
		}
	      break;
	    case snd_K_F: case snd_K_f: 
	      prompt(sp, _("file:"), NULL); 
	      sp->filing = INPUT_FILING; 
	      dont_clear_minibuffer = true; 
	      break;
	    case snd_K_G: case snd_K_g: 
	      control_g(sp);
	      break;
	    case snd_K_I: case snd_K_i: 
	      prompt(sp, _("insert file:"), NULL); 
	      sp->filing = INSERT_FILING; 
	      dont_clear_minibuffer = true; 
	      break;
	    case snd_K_J: case snd_K_j:
	      cp->cursor_on = true; 
	      goto_mix(cp, ext_count); 
	      break;
	    case snd_K_L: case snd_K_l: 
	      prompt(sp, _("load:"), NULL); 
	      sp->loading = true;
	      dont_clear_minibuffer = true; 
	      break;
	    case snd_K_M: case snd_K_m:
	      cp->cursor_on = true; 
	      prompt_named_mark(cp);
	      set_show_marks(true); 
	      dont_clear_minibuffer = true; 
	      break;
	    case snd_K_O: case snd_K_o: 
	      /* this doesn't change the View:Controls menu label because (sigh...) it's specific to the currently selected sound */
	      show_controls(sp); 
	      break;
	    case snd_K_P: case snd_K_p: 
	      set_window_size(cp, ext_count); 
	      break;
	    case snd_K_Q: case snd_K_q: 
	      prompt(sp, _("mix file:"), NULL); 
	      sp->filing = CHANGE_FILING; 
	      dont_clear_minibuffer = true; 
	      break;
	    case snd_K_R: case snd_K_r: 
	      redo_edit_with_sync(cp, ext_count); 
	      break;
	    case snd_K_S: case snd_K_s: 
	      save_edits_with_prompt(sp);
	      dont_clear_minibuffer = true; 
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
	      prompt(sp, _("file:"), NULL); 
	      sp->filing = CHANNEL_FILING; 
	      dont_clear_minibuffer = true; 
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
	      report_in_minibuffer(sp, _("C-x C-%s undefined"), key_to_name(keysym));
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
	      else deactivate_selection();
	      break;
	    case snd_keypad_Up:
	      set_spectro_z_scale(spectro_z_scale(ss) + .01);
	      reflect_spectro();
	      break;
	    case snd_keypad_Down:
	      set_spectro_z_scale(spectro_z_scale(ss) - .01);
	      reflect_spectro();
	      break;
	    case snd_keypad_Left:
	      set_spectro_z_angle(spectro_z_angle(ss) - 1.0);
	      reflect_spectro();
	      break;
	    case snd_keypad_Right:
	      set_spectro_z_angle(spectro_z_angle(ss) + 1.0);
	      reflect_spectro(); 
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
		  if (wavo_trace(ss)>1) 
		    set_wavo_trace(wavo_trace(ss) - 1);
		} 
	      else 
		{
		  if (spectro_hop(ss)>1) 
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
	    case snd_keypad_PageDown:
	      set_spectro_cutoff(spectro_cutoff(ss) * .95); 
	      reflect_spectro(); 
	      break;
	    case snd_keypad_PageUp:
	      if (spectro_cutoff(ss) < 1.0) 
		set_spectro_cutoff(spectro_cutoff(ss) / .95); 
	      reflect_spectro(); 
	      break;
	    case snd_keypad_Enter: 
	      reset_spectro(); 
	      reflect_spectro(); 
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
		else 
		  {
		    prompt(sp, "M-x:", buf);
		    sp->macro_count = count;
		    clear_search = false;
		  }
	      }
#else
	      report_in_minibuffer(sp, _("key %s%s undefined"), (state & snd_MetaMask) ? "M-" : "", key_to_name(keysym));
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
		case snd_K_A: case snd_K_a: 
		  if (selection_is_active_in_channel(cp)) 
		    {
		      get_amp_expression(sp, (!got_ext_count) ? 1 : ext_count, OVER_SELECTION); 
		      dont_clear_minibuffer = true; 
		    } 
		  else string_to_minibuffer(sp, _("no active selection"));
		  break;
		case snd_K_B: case snd_K_b: 
		  cp->cursor_on = true; 
		  handle_cursor(cp, CURSOR_ON_LEFT);
		  break;
		case snd_K_C: case snd_K_c: 
		  if (!(mark_define_region(cp, (!got_ext_count) ? 1 : ext_count)))
		    string_to_minibuffer(cp->sound, _("no such mark"));
		  break;
		case snd_K_D: case snd_K_d: 
		  prompt(sp, _("temp dir:"), NULL); 
		  sp->filing = TEMP_FILING; 
		  dont_clear_minibuffer = true; 
		  break;
		case snd_K_E: case snd_K_e: 
		  if (defining_macro) 
		    {
		      string_to_minibuffer(sp, _("can't call macro while it's being defined"));
		      defining_macro = false;
		      macro_size = 0; /* so subsequent M-x e doesn't get something silly */
		    }
		  else
		    {
		      execute_last_macro(cp, (!got_ext_count) ? 1 : ext_count);
		      if ((cp) && (cp->sound) && (cp->active)) handle_cursor(cp, cursor_decision(cp)); else return;
		    }
		  break;
		case snd_K_F: case snd_K_f: 
		  cp->cursor_on = true; 
		  handle_cursor(cp, CURSOR_ON_RIGHT); 
		  break;
		case snd_K_I: case snd_K_i: 
		  insert_selection_or_region((!got_ext_count) ? 0 : ext_count, cp);
		  break;
		case snd_K_J: case snd_K_j: 
		  prompt(sp, _("mark:"), NULL); 
		  sp->finding_mark = true; 
		  dont_clear_minibuffer = true; 
		  break;
		case snd_K_K: case snd_K_k: 
		  snd_close_file(sp); 
		  break;
		case snd_K_L: case snd_K_l: 
		  cp->cursor_on = true;
		  if (selection_is_active_in_channel(cp))
		    cursor_moveto(cp, (off_t)(selection_beg(cp) + 0.5 * selection_len()));
		  else string_to_minibuffer(sp, _("no active selection"));
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
		  else string_to_minibuffer(sp, _("no active selection"));
		  break;
		case snd_K_W: case snd_K_w:
		  prompt(sp, _("file:"), NULL); 
		  sp->filing = SELECTION_FILING; 
		  dont_clear_minibuffer = true;
		  break;
		case snd_K_Z: case snd_K_z: 
		  if (selection_is_active_in_channel(cp))
		    cos_smooth(cp, CURSOR(cp), (!got_ext_count) ? 1 : ext_count, OVER_SELECTION); 
		  else string_to_minibuffer(sp, _("no active selection"));
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
		    string_to_minibuffer(sp, _("macro definition already in progress"));
		  else
		    {
		      start_defining_macro(); 
		      string_to_minibuffer(sp, _("defining macro...")); 
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
		case snd_K_slash: 
		  cp->cursor_on = true;
		  prompt_named_mark(cp); 
		  set_show_marks(true); 
		  dont_clear_minibuffer = true; 
		  break;
		default:
		  report_in_minibuffer(sp, _("C-x %s undefined"), key_to_name(keysym));
		  break;
		}
	    }
	  else
	    {
	      report_in_minibuffer(sp, _("C-x M-%s undefined"), key_to_name(keysym));
	    }
	}
    }
  if (!extended_mode) {got_ext_count = false; ext_count = 1;}
  if ((sp) && (clear_search))
    {
      if ((sp->minibuffer_on == MINI_FIND) && (!dont_clear_minibuffer))
	clear_minibuffer(sp);
      else 
	if (!cursor_searching) 
	  sp->search_count = 0;
    }
}


/* ---------------- Xen kbd bindings ---------------- */

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
  return((int)XStringToKeysym(XEN_TO_C_STRING(key)));  /* these are the X/Gtk names: not "+" but "plus" etc */
#else
  #if USE_GTK
  return((int)gdk_keyval_from_name(XEN_TO_C_STRING(key)));
  #else
  {
    static bool already_warned = false;
    if (!already_warned)
      {
	snd_error("%s: can't translate a key name to a key in this version of Snd.", caller);
	already_warned = true;
      }
  }
  #endif
#endif
  return(0);
}

static XEN check_for_key_error(int k, int s, const char *caller)
{
  if ((k < MIN_KEY_CODE) || (k > MAX_KEY_CODE) ||
      (s < MIN_KEY_STATE) || (s > MAX_KEY_STATE))
    XEN_ERROR(XEN_ERROR_TYPE("no-such-key"),
	      XEN_LIST_3(C_TO_XEN_STRING(caller),
			 C_TO_XEN_STRING("key: ~A, state: ~A"),
			 XEN_LIST_2(C_TO_XEN_INT(k),
				    C_TO_XEN_INT(s))));
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
  i = in_user_keymap(k, s, XEN_TRUE_P(cx_extended));
  if (i >= 0) 
    return(user_keymap[i].func);
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
      char *comment = NULL, *prefs = NULL;
      args = XEN_REQUIRED_ARGS(code);
      if (args > 1)
	{
	  XEN errmsg;
	  char *errstr;
	  errstr = mus_format(_(S_bind_key " function arg should take either zero or one args, not %d"), args);
	  errmsg = C_TO_XEN_STRING(errstr);
	  FREE(errstr);
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
  #define H_bind_key "(" S_bind_key " key modifiers func :optional extended origin prefs-info: \
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

static XEN g_save_macros(XEN file)
{
  #define H_save_macros "(" S_save_macros " :optional (file \"~/.snd\")): save keyboard macros file"
  FILE *fd = NULL;
  char *name;
  XEN_ASSERT_TYPE(XEN_STRING_P(file), file, XEN_ONLY_ARG, S_save_macros, "a string");
  name = XEN_TO_C_STRING(file);
  fd = FOPEN(name, "a");
  if (fd) 
    {
      save_macro_state(fd);
      snd_fclose(fd, name);
    }
  else
    {
      XEN_ERROR(CANNOT_SAVE,
		XEN_LIST_3(C_TO_XEN_STRING(S_save_macros),
			   file,
			   C_TO_XEN_STRING(snd_io_strerror())));
    }
  return(file);
}

/* this doesn't display the full prompt in motif, but I can't find any way to fix it */

static XEN g_prompt_in_minibuffer(XEN msg, XEN callback, XEN snd_n, XEN raw)
{
  #if HAVE_SCHEME
    #define prompt_example "(prompt-in-minibuffer \"what?\" (lambda (response) (snd-print response)))"
  #endif
  #if HAVE_RUBY
    #define prompt_example "prompt_in_minibuffer(\"what?\", lambda do | response | snd_print(response) end)"
  #endif
  #if HAVE_FORTH
    #define prompt_example "\"what?\" lambda: <{ response }> response snd-print ; prompt-in-minibuffer"
  #endif

  #define H_prompt_in_minibuffer "(" S_prompt_in_minibuffer " msg :optional callback snd raw): post msg in snd's minibuffer \
then when the user eventually responds, invoke the function callback, if any, with the response.  If 'raw' is " PROC_TRUE ", the response is \
passed as a string to the prompt callback function; otherwise it is evaluated first as Scheme code.\n  " prompt_example

  snd_info *sp;
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, XEN_ARG_1, S_prompt_in_minibuffer, "a string");
  XEN_ASSERT_TYPE((XEN_NOT_BOUND_P(callback)) || (XEN_BOOLEAN_P(callback)) || XEN_PROCEDURE_P(callback), 
		  callback, XEN_ARG_2, S_prompt_in_minibuffer, PROC_FALSE " or a procedure");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(raw), raw, XEN_ARG_4, S_prompt_in_minibuffer, "a boolean");
  ASSERT_SOUND(S_prompt_in_minibuffer, snd_n, 3);
  sp = get_sp(snd_n, NO_PLAYERS);
  if ((sp == NULL) || (sp->inuse != SOUND_NORMAL))
    return(snd_no_such_sound_error(S_prompt_in_minibuffer, snd_n));
  if (XEN_PROCEDURE_P(sp->prompt_callback))
    {
      snd_unprotect_at(sp->prompt_callback_loc);
      sp->prompt_callback_loc = NOT_A_GC_LOC;
    }
  sp->prompt_callback = XEN_FALSE; /* just in case something goes awry */
  if (XEN_BOUND_P(raw)) sp->raw_prompt = XEN_TO_C_BOOLEAN(raw); else sp->raw_prompt = false;
  if (XEN_PROCEDURE_P(callback))
    {
      char *errstr;
      XEN errmsg;
      errstr = procedure_ok(callback, 1, S_prompt_in_minibuffer, "callback", 2);
      if (errstr)
	{
	  errmsg = C_TO_XEN_STRING(errstr);
	  FREE(errstr);
	  return(snd_bad_arity_error(S_prompt_in_minibuffer, 
				     errmsg,
				     callback));
	}
      sp->prompt_callback_loc = snd_protect(callback);  
    }
  sp->prompt_callback = callback;
  make_minibuffer_label(sp, XEN_TO_C_STRING(msg));
  sp->minibuffer_on = MINI_USER;
  sp->prompting = true;
#if USE_MOTIF
  goto_minibuffer(sp); /* in gtk this somehow calls activate in the text widget, clearing our prompt? */
#endif
  return(callback);
}

static XEN g_report_in_minibuffer(XEN msg, XEN snd_n, XEN as_error)
{
  #define H_report_in_minibuffer "(" S_report_in_minibuffer " msg :optional snd as-error): display msg in snd's minibuffer. \
If 'as-error' is " PROC_TRUE ", place the message in the minibuffer's error label."
  snd_info *sp;
  XEN_ASSERT_TYPE(XEN_STRING_P(msg), msg, XEN_ARG_1, S_report_in_minibuffer, "a string");
  XEN_ASSERT_TYPE(XEN_BOOLEAN_IF_BOUND_P(as_error), as_error, XEN_ARG_2, S_report_in_minibuffer, "a boolean");
  ASSERT_SOUND(S_report_in_minibuffer, snd_n, 2);
  sp = get_sp(snd_n, NO_PLAYERS);
  if ((sp == NULL) || (sp->inuse != SOUND_NORMAL))
    return(snd_no_such_sound_error(S_report_in_minibuffer, snd_n));
  if (XEN_TRUE_P(as_error))
    display_minibuffer_error(sp, XEN_TO_C_STRING(msg));
  else string_to_minibuffer(sp, XEN_TO_C_STRING(msg));
  return(msg);
}

static XEN g_clear_minibuffer(XEN snd)
{
  #define H_clear_minibuffer "(" S_clear_minibuffer " :optional snd) clears snd's minibuffer (erasing any \
error message as well)."
  snd_info *sp;
  ASSERT_SOUND(S_clear_minibuffer, snd, 1);
  sp = get_sp(snd, NO_PLAYERS);
  if ((sp == NULL) || (sp->inuse != SOUND_NORMAL))
    return(snd_no_such_sound_error(S_clear_minibuffer, snd));
  clear_minibuffer(sp);
  return(XEN_FALSE);
}

static XEN g_control_g_x(void)
{
  #define H_control_g_x "(" S_c_g_x "): simulate C-g"
  control_g(any_selected_sound());
  return(XEN_FALSE);
}

#define S_snd_simulate_keystroke "snd-simulate-keystroke"
static XEN g_snd_simulate_keystroke(XEN snd, XEN chn, XEN key, XEN state)
{
  /* intended for testing */
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(key), key, XEN_ARG_3, S_snd_simulate_keystroke, "key number (int)");
  XEN_ASSERT_TYPE(XEN_INTEGER_P(state), state, XEN_ARG_4, S_snd_simulate_keystroke, "key state (int)");
  ASSERT_CHANNEL(S_snd_simulate_keystroke, snd, chn, 1);
  cp = get_cp(snd, chn, S_snd_simulate_keystroke);
  if (!cp) return(XEN_FALSE);
  keyboard_command(cp, XEN_TO_C_INT(key), XEN_TO_C_INT(state));
  return(key);
}

#ifdef XEN_ARGIFY_1
XEN_ARGIFY_3(g_key_binding_w, g_key_binding)
XEN_ARGIFY_6(g_bind_key_w, g_bind_key)
XEN_ARGIFY_3(g_unbind_key_w, g_unbind_key)
XEN_ARGIFY_4(g_key_w, g_key)
XEN_NARGIFY_1(g_save_macros_w, g_save_macros)
XEN_NARGIFY_0(g_control_g_x_w, g_control_g_x)
XEN_ARGIFY_1(g_clear_minibuffer_w, g_clear_minibuffer)
XEN_ARGIFY_3(g_report_in_minibuffer_w, g_report_in_minibuffer)
XEN_ARGIFY_4(g_prompt_in_minibuffer_w, g_prompt_in_minibuffer)
XEN_NARGIFY_4(g_snd_simulate_keystroke_w, g_snd_simulate_keystroke)
#else
#define g_key_binding_w g_key_binding
#define g_bind_key_w g_bind_key
#define g_unbind_key_w g_unbind_key
#define g_key_w g_key
#define g_save_macros_w g_save_macros
#define g_control_g_x_w g_control_g_x
#define g_clear_minibuffer_w g_clear_minibuffer
#define g_report_in_minibuffer_w g_report_in_minibuffer
#define g_prompt_in_minibuffer_w g_prompt_in_minibuffer
#define g_snd_simulate_keystroke_w g_snd_simulate_keystroke
#endif

void g_init_kbd(void)
{
  #define H_cursor_in_view     "The value for a " S_bind_key " function that causes it to shift the window so that the cursor is in the view"
  #define H_cursor_on_left     "The value for a " S_bind_key " function that causes it to shift the window so that the cursor is at the left edge"
  #define H_cursor_on_right    "The value for a " S_bind_key " function that causes it to shift the window so that the cursor is at the right edge"
  #define H_cursor_in_middle   "The value for a " S_bind_key " function that causes it to shift the window so that the cursor is in the middle"
  #define H_keyboard_no_action "The value for a " S_bind_key " function that causes it do nothing upon return"

  XEN_DEFINE_CONSTANT(S_cursor_in_view,          CURSOR_IN_VIEW,                      H_cursor_in_view);
  XEN_DEFINE_CONSTANT(S_cursor_on_left,          CURSOR_ON_LEFT,                      H_cursor_on_left);
  XEN_DEFINE_CONSTANT(S_cursor_on_right,         CURSOR_ON_RIGHT,                     H_cursor_on_right);
  XEN_DEFINE_CONSTANT(S_cursor_in_middle,        CURSOR_IN_MIDDLE,                    H_cursor_in_middle);
  XEN_DEFINE_CONSTANT(S_keyboard_no_action,      KEYBOARD_NO_ACTION,                  H_keyboard_no_action);

  XEN_DEFINE_PROCEDURE(S_key_binding,            g_key_binding_w,            1, 2, 0, H_key_binding);
  XEN_DEFINE_PROCEDURE(S_bind_key,               g_bind_key_w,               3, 3, 0, H_bind_key);
  XEN_DEFINE_PROCEDURE(S_unbind_key,             g_unbind_key_w,             2, 1, 0, H_unbind_key);
  XEN_DEFINE_PROCEDURE(S_key,                    g_key_w,                    2, 2, 0, H_key);
  XEN_DEFINE_PROCEDURE(S_save_macros,            g_save_macros_w,            1, 0, 0, H_save_macros);
  XEN_DEFINE_PROCEDURE(S_c_g_x,                  g_control_g_x_w,            0, 0, 0, H_control_g_x);  
  XEN_DEFINE_PROCEDURE(S_clear_minibuffer,       g_clear_minibuffer_w,       0, 1, 0, H_clear_minibuffer);
  XEN_DEFINE_PROCEDURE(S_report_in_minibuffer,   g_report_in_minibuffer_w,   1, 2, 0, H_report_in_minibuffer);
  XEN_DEFINE_PROCEDURE(S_prompt_in_minibuffer,   g_prompt_in_minibuffer_w,   1, 3, 0, H_prompt_in_minibuffer);
  XEN_DEFINE_PROCEDURE(S_snd_simulate_keystroke, g_snd_simulate_keystroke_w, 4, 0, 0, "internal testing function");
}
