#include "snd.h"

typedef struct {int n; int direction; int chans; int inc; chan_info **cps; snd_fd **fds;} gfd;

static int prepare_global_search (chan_info *cp, void *g0)
{
  gfd *g = (gfd *)g0;
  int direction;
  direction = g->direction;
  g->cps[g->n] = cp;
  g->fds[g->n] = init_sample_read((direction == READ_FORWARD) ? (cp->cursor + 1) : (cp->cursor-1), cp, direction);
  if (g->fds[g->n] != NULL) g->n++;
  return(0);
}

static int run_global_search (snd_state *ss, gfd *g)
{
  /* return 0 until success or all eof */
  /* if success, n = winner (as aref index), if eofs, n=-1 */
  int i, j, k;
  Float samp;
  SCM res;
  snd_fd *sf;
  if (PROCEDURE_P(ss->search_proc))
    {
      for (i = 0; i < g->chans; i++)
	{
	  if (g->cps[i])
	    {
	      if (!((g->cps[i])->sound)) return(-1);
	      sf = g->fds[i]; 
	      if (g->direction == READ_FORWARD)
		samp = next_sample_to_float(sf);
	      else samp = previous_sample_to_float(sf);
	      res = CALL1(ss->search_proc, TO_SCM_DOUBLE((double)(samp)), "global search func");
	      if (TRUE_P(res))
		{
		  g->n = i;
		  return(1);
		}
	      if (read_sample_eof(sf))
		{
		  free_snd_fd(sf);
		  g->fds[i] = NULL;
		  g->cps[i] = NULL;
		  k = 0;
		  for (j = 0; j < g->chans; j++) 
		    if (g->cps[i]) 
		      {
			k = 1;
			break;
		      }
		  if (k == 0) /* all at eof */
		    {
		      g->n = -1;
		      return(1);
		    }
		}
	    }
	}
    }
  g->inc++;
  return(0);
}

static char search_no_luck[PRINT_BUFFER_SIZE];

char *global_search(snd_state *ss, int direction)
{
  /* set up snd_fd for each active channel, 
   * tick each one forward until a match is found, 
   * update cursor/graph and report success (if any) in associated info window
   * subsequent runs (if no new text) repeat the search from the current locations
   */
  int chans, i, redisplay, passes = 0;
  gfd *fd;
  chan_info *cp;
  if (ss->search_in_progress) 
    {
      mus_snprintf(search_no_luck, PRINT_BUFFER_SIZE, "search in progress");
      return(search_no_luck);
    }
  ss->search_in_progress = 1;
  chans = active_channels(ss, WITH_VIRTUAL_CHANNELS);
  search_no_luck[0] = '\0';
  if (chans > 0)
    {
      fd = (gfd *)CALLOC(1, sizeof(gfd));
      fd->n = 0;
      fd->inc = 1;
      fd->direction = direction;
      fd->chans = chans;
      fd->fds = (snd_fd **)CALLOC(chans, sizeof(snd_fd *));
      fd->cps = (chan_info **)CALLOC(chans, sizeof(chan_info *));
      map_over_chans(ss, prepare_global_search, (void *)fd);
      fd->n = -1;
      while (!(run_global_search(ss, fd)))
	{
	  passes++;
	  if (passes >= 100)
	    {
	      check_for_event(ss);
	      passes = 0;
	      fd->n = -1;
	    }
	  if (ss->stopped_explicitly) break;
	}
      if (fd->n == -1)
	{
	  if (ss->stopped_explicitly)
	    mus_snprintf(search_no_luck, PRINT_BUFFER_SIZE, "search stopped");
	  else mus_snprintf(search_no_luck, PRINT_BUFFER_SIZE, "%s: not found", ss->search_expr);
	  /* printed by find_ok_callback in snd-xmenu.c */
	}
      else
	{
	  /* fd->n is winner, fd->inc is how far forward we searched from current cursor loc */
	  cp = fd->cps[fd->n];
	  if (direction == READ_FORWARD)
	    cp->cursor += fd->inc;
	  else cp->cursor -= fd->inc;
	  /* now in its own info window show find state, and update graph if needed */
          cp->cursor_on = 1;
	  show_cursor_info(cp);
	  if ((cp->cursor >= (cp->axis)->losamp) && (cp->cursor <= (cp->axis)->hisamp))
	    redisplay  = CURSOR_IN_VIEW;
	  else redisplay = CURSOR_IN_MIDDLE;
	  handle_cursor(cp, redisplay);
	}
      ss->stopped_explicitly = 0;
      for (i = 0; i < chans; i++) 
	if (fd->cps[i]) 
	  free_snd_fd(fd->fds[i]);
      FREE(fd->fds);
      FREE(fd->cps);
      FREE(fd);
    }
  ss->search_in_progress = 0;
  return(search_no_luck);
}

static int cursor_find_forward(snd_info *sp, chan_info *cp, int count)
{
  int i = 0, end, passes = 0;
  snd_fd *sf = NULL;
  snd_state *ss;
  SCM res = SCM_BOOL_F;
  ss = sp->state;
  if (ss->search_in_progress) 
    {
      report_in_minibuffer(sp, "search in progress");
      return(-1);
    }
  ss->search_in_progress = 1;
  sf = init_sample_read(cp->cursor + 1, cp, READ_FORWARD);
  if (!sf)
    {
      ss->search_in_progress = 0;
      return(-1);
    }
  sf->direction = READ_FORWARD;
  end = current_ed_samples(cp);
  for (i = cp->cursor + 1, passes = 0; i < end; i++, passes++)
    {
      res = CALL1(sp->search_proc, 
		  TO_SCM_DOUBLE((double)(next_sample_to_float(sf))), 
		  "local search func");
      if (NOT_FALSE_P(res)) 
	{
	  count--; 
	  if (count == 0) break;
	}
      if (passes >= 100)
	{
	  check_for_event(ss);
	  /* if user types C-s during an active search, we risk stomping on our current pointers */
	  if (!(sp->active)) break;
	  passes = 0;
	}
      if (ss->stopped_explicitly) break;
    }
  ss->stopped_explicitly = 0;
  free_snd_fd(sf);
  ss->search_in_progress = 0;
  if (count != 0) return(-1); /* impossible sample number, so => failure */
  return(i);
}

static int cursor_find_backward(snd_info *sp, chan_info *cp, int count)
{
  int i = 0, passes = 0;
  snd_fd *sf = NULL;
  snd_state *ss;
  SCM res = SCM_BOOL_F;
  ss = sp->state;
  if (ss->search_in_progress) 
    {
      report_in_minibuffer(sp, "search in progress");
      return(-1);
    }
  ss->search_in_progress = 1;
  sf = init_sample_read(cp->cursor - 1, cp, READ_BACKWARD);
  if (!sf)
    {
      ss->search_in_progress = 0;
      return(-1);
    }
  sf->direction = READ_BACKWARD;
  for (i = cp->cursor - 1, passes = 0; i >= 0; i--, passes++)
    {
      res = CALL1(sp->search_proc, 
		  TO_SCM_DOUBLE((double)(previous_sample_to_float(sf))), 
		  "local search func");
      if (NOT_FALSE_P(res)) 
	{
	  count--; 
	  if (count == 0) break;
	}
      if (passes >= 100)
	{
	  check_for_event(ss);
	  /* if user types C-s during an active search, we risk stomping on our current pointers */
	  if (!(sp->active)) break;
	  passes = 0;
	}
      if (ss->stopped_explicitly) break;
    }
  ss->stopped_explicitly = 0;
  free_snd_fd(sf);
  ss->search_in_progress = 0;
  if (count != 0) return(-1); /* impossible sample number, so => failure */
  return(i);
}


static void get_find_expression(snd_info *sp, int count)
{
  /* clear previous ? */
  set_minibuffer_string(sp, NULL);
  make_minibuffer_label(sp, "find:");
  sp->minibuffer_on = 1;
  goto_minibuffer(sp);
  sp->searching = count;
}

int cursor_search(chan_info *cp, int count)
{
  int samp;
  snd_info *sp;
  char *s1, *s2;
  snd_state *ss;
  ss = cp->state;
  sp = cp->sound;
  if (ss->search_in_progress) 
    {
      report_in_minibuffer(sp, "search in progress");
      return(KEYBOARD_NO_ACTION);
    }
  if (sp->searching)
    {
      if (!(PROCEDURE_P(sp->search_proc))) return(CURSOR_IN_VIEW); /* no search expr */
      if (count > 0)
	samp = cursor_find_forward(sp, cp, count);
      else samp = cursor_find_backward(sp, cp, -count);
      if (samp == -1) 
	{ 
	  report_in_minibuffer(sp, "%s: not found", sp->search_expr);
	  return(CURSOR_IN_VIEW);
	}
      else
	{
	  report_in_minibuffer(sp, "%s: y = %s at %s (%d)",
			       sp->search_expr,
			       s1 = prettyf(sample(samp, cp), 2),
			       s2 = prettyf((double)samp / (double)SND_SRATE(sp), 2),
			       samp);
	  FREE(s1);
	  FREE(s2);
	}
      cursor_moveto(cp, samp);
      if ((cp->cursor >= (cp->axis)->losamp) && 
	  (cp->cursor <= (cp->axis)->hisamp))
	return(CURSOR_IN_VIEW);
      else return(CURSOR_IN_MIDDLE);
    }
  else get_find_expression(sp, count);
  return(CURSOR_IN_VIEW);
}

static SCM g_search_procedure(SCM snd)
{
  #define H_search_procedure "(" S_search_procedure " &optional index) -> global or sound-local search function"
  snd_state *ss;
  snd_info *sp;
  if (BOUND_P(snd))
    {
      SND_ASSERT_SND(S_search_procedure, snd, 1);
      sp = get_sp(snd);
      if (sp)
	return(sp->search_proc);
      else return(SCM_BOOL_F);
    }
  ss = get_global_state();
  return(ss->search_proc);
}

static SCM g_set_search_procedure(SCM snd, SCM proc)
{
  snd_state *ss;
  snd_info *sp;
  char *error = NULL;
  SCM errstr;
  if (INTEGER_P(snd)) /* could be the proc arg if no snd */
    {
      SND_ASSERT_SND("set-" S_search_procedure, snd, 1);
      sp = get_sp(snd);
      if (sp)
	{
	  if (PROCEDURE_P(sp->search_proc))
	    snd_unprotect(sp->search_proc);
	  sp->search_proc = SCM_UNDEFINED;
	  error = procedure_ok(proc, 1, "find", "find", 1);
	  if (error == NULL)
	    {
	      sp->search_proc = proc;
	      snd_protect(proc);
	      if (sp->search_expr) free(sp->search_expr);
	      sp->search_expr = g_print_1(proc, __FUNCTION__);
	      return(proc);
	    }
	  else 
	    {
	      errstr = TO_SCM_STRING(error);
	      FREE(error);
	      return(snd_bad_arity_error("set-" S_search_procedure, errstr, proc));
	    }
	}
      else
	return(snd_no_such_sound_error("set-" S_search_procedure, snd));
    }
  else 
    {
      ss = get_global_state();
      if (PROCEDURE_P(ss->search_proc))
	snd_unprotect(ss->search_proc);
      ss->search_proc = SCM_UNDEFINED;
      error = procedure_ok(snd, 1, "find", "find", 1);
      if (error == NULL)
	{
	  ss->search_proc = snd;
	  snd_protect(snd);
	  if (ss->search_expr) free(ss->search_expr);
	  ss->search_expr = g_print_1(snd, __FUNCTION__);
	}
      else 
	{
	  errstr = TO_SCM_STRING(error);
	  FREE(error);
	  return(snd_bad_arity_error("set-" S_search_procedure, errstr, proc));
	}
    }
  return(snd);
}

void g_init_find(SCM local_doc)
{
  define_procedure_with_setter(S_search_procedure, SCM_FNC g_search_procedure, H_search_procedure,
			       "set-" S_search_procedure, SCM_FNC g_set_search_procedure, local_doc, 0, 1, 1, 1);
}
