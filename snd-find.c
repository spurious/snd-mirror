#include "snd.h"

/* TODO: lambda -> run to precompile simple forms
 */

static int search_in_progress = 0;
typedef struct {int n; int direction; int chans; int inc; chan_info **cps; snd_fd **fds;} gfd;

static int prepare_global_search (chan_info *cp, void *g0)
{
  gfd *g = (gfd *)g0;
  int direction;
  direction = g->direction;
  g->cps[g->n] = cp;
  g->fds[g->n] = init_sample_read((direction == READ_FORWARD) ? (cp->cursor + 1) : (cp->cursor - 1), cp, direction);
  if (g->fds[g->n] != NULL) g->n++;
  return(0);
}

static int run_global_search (snd_state *ss, gfd *g)
{
  /* return 0 until success or all eof */
  /* if success, n = winner (as aref index), if eofs, n=-1 */
  int i, j, k;
  Float samp;
  XEN res;
  snd_fd *sf;
  if (XEN_PROCEDURE_P(ss->search_proc))
    {
      for (i = 0; i < g->chans; i++)
	{
	  if (g->cps[i])
	    {
	      if (!((g->cps[i])->sound)) return(-1);
	      sf = g->fds[i]; 
	      samp = read_sample_to_float(sf);
	      res = XEN_CALL_1(ss->search_proc,
			       C_TO_XEN_DOUBLE((double)(samp)), 
			       "global search func");
	      if (XEN_TRUE_P(res))
		{
		  g->n = i;
		  return(1);
		}
	      else
		{
		  if (XEN_INTEGER_P(res))
		    {
		      g->n = i; /* channel number */
		      if (g->direction == READ_FORWARD)
			g->inc += XEN_TO_C_INT(res);
		      else g->inc -= XEN_TO_C_INT(res);
		      return(1);
		    }
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

static char search_message[PRINT_BUFFER_SIZE];

char *global_search(snd_state *ss, int direction)
{
  /* set up snd_fd for each active channel, 
   * tick each one forward until a match is found, 
   * update cursor/graph and report success (if any) in associated info window
   * subsequent runs (if no new text) repeat the search from the current locations
   */
  int chans, i, passes = 0;
  gfd *fd;
  chan_info *cp;
  if (search_in_progress) 
    {
      mus_snprintf(search_message, PRINT_BUFFER_SIZE, "search in progress");
      return(search_message);
    }

  /* here and elsewhere when a new search is begun, even if using the previous search expression, the
   *   expression needs to be re-evaluated, since otherwise in a search like:
   *   (define (zero+)
   *     (let ((lastn 0.0))
   *       (lambda (n)
   *         (let ((rtn (and (< lastn 0.0)
   *                         (>= n 0.0)
   *                         -1)))
   *           (set! lastn n)
   *           rtn))))
   *   the notion of the previous sample will never be cleared.
   * But we do know that the expression is ok otherwise.
   * (This causes one redundant evaluation the first time around)
   */
  if (ss->search_expr)
    {
      /* search_expr can be null if user set search_proc directly */
      snd_unprotect(ss->search_proc);
      ss->search_proc = snd_catch_any(eval_str_wrapper, ss->search_expr, ss->search_expr);
      snd_protect(ss->search_proc);
    }
  search_in_progress = 1;
  chans = active_channels(ss, WITH_VIRTUAL_CHANNELS);
  search_message[0] = '\0';
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
	    mus_snprintf(search_message, PRINT_BUFFER_SIZE, "search stopped");
	  else mus_snprintf(search_message, PRINT_BUFFER_SIZE, "%s: not found", ss->search_expr);
	  /* printed by find_ok_callback in snd-xmenu.c */
	}
      else
	{
	  /* fd->n is winner, fd->inc is how far forward we searched from current cursor loc */
	  cp = fd->cps[fd->n];
          cp->cursor_on = 1;
	  if (direction == READ_FORWARD)
	    cursor_move(cp, fd->inc);
	  else cursor_move(cp, -fd->inc);
	  /* now in its own info window show find state, and update graph if needed */
	  show_cursor_info(cp);
	}
      ss->stopped_explicitly = 0;
      for (i = 0; i < chans; i++) 
	if (fd->cps[i]) 
	  free_snd_fd(fd->fds[i]);
      FREE(fd->fds);
      FREE(fd->cps);
      FREE(fd);
    }
  search_in_progress = 0;
  return(search_message);
}

static int cursor_find_forward(snd_info *sp, chan_info *cp, int count)
{
  int i = 0, end, start, passes = 0;
  snd_fd *sf = NULL;
  snd_state *ss;
  XEN res = XEN_FALSE;
  ss = sp->state;
  if (search_in_progress) 
    {
      report_in_minibuffer(sp, "search in progress");
      return(-1);
    }
  search_in_progress = 1;
  if (cp->last_search_result == SEARCH_OK)
    start = cp->cursor + 1;
  else start = 0;
  sf = init_sample_read(start, cp, READ_FORWARD);
  if (!sf)
    {
      search_in_progress = 0;
      return(-1);
    }
  end = current_ed_samples(cp);
  for (i = start, passes = 0; i < end; i++, passes++)
    {
      res = XEN_CALL_1(sp->search_proc, 
		       C_TO_XEN_DOUBLE((double)(read_sample_to_float(sf))), 
		       "local search func");
      if (XEN_NOT_FALSE_P(res)) 
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
  search_in_progress = 0;
  if (count != 0) return(-1); /* impossible sample number, so => failure */
  if (XEN_INTEGER_P(res))
    return(i + XEN_TO_C_INT(res));
  return(i);
}

static int cursor_find_backward(snd_info *sp, chan_info *cp, int count)
{
  int i = 0, start, passes = 0;
  snd_fd *sf = NULL;
  snd_state *ss;
  XEN res = XEN_FALSE;
  ss = sp->state;
  if (search_in_progress) 
    {
      report_in_minibuffer(sp, "search in progress");
      return(-1);
    }
  search_in_progress = 1;
  if (cp->last_search_result == SEARCH_OK)
    start = cp->cursor - 1;
  else start = cp->samples[cp->edit_ctr] - 1;
  sf = init_sample_read(start, cp, READ_BACKWARD);
  if (!sf)
    {
      search_in_progress = 0;
      return(-1);
    }
  for (i = start, passes = 0; i >= 0; i--, passes++)
    {
      res = XEN_CALL_1(sp->search_proc, 
		       C_TO_XEN_DOUBLE((double)(read_sample_to_float(sf))), 
		       "local search func");
      if (XEN_NOT_FALSE_P(res)) 
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
  search_in_progress = 0;
  if (count != 0) return(-1); /* impossible sample number, so => failure */
  if (XEN_INTEGER_P(res))
    return(i - XEN_TO_C_INT(res));
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

void cursor_search(chan_info *cp, int count)
{
  int samp;
  snd_info *sp;
  char *s1, *s2;
  sp = cp->sound;
  if (search_in_progress) 
    report_in_minibuffer(sp, "search in progress");
  else
    {
      if (sp->searching)
	{
	  if (!(XEN_PROCEDURE_P(sp->search_proc))) return; /* no search expr */
	  
	  if (sp->search_expr)
	    {
	      /* see note above about closures */
	      snd_unprotect(sp->search_proc);
	      sp->search_proc = snd_catch_any(eval_str_wrapper, sp->search_expr, sp->search_expr);
	      snd_protect(sp->search_proc);
	    }

	  if (count > 0)
	    samp = cursor_find_forward(sp, cp, count);
	  else samp = cursor_find_backward(sp, cp, -count);
	  if (samp == -1) 
	    { 
	      report_in_minibuffer(sp, "%s: not found%s", sp->search_expr, (cp->last_search_result == SEARCH_FAILED) ? " (wrapped)" : "");
	      cp->last_search_result = SEARCH_FAILED;
	    }
	  else
	    {
	      report_in_minibuffer(sp, "%s%sy = %s at %s (%d)",
				   (sp->search_expr) ? sp->search_expr : "",
				   (sp->search_expr) ? ": " : "",
				   s1 = prettyf(chn_sample(samp, cp, cp->edit_ctr), 2),
				   s2 = prettyf((double)samp / (double)SND_SRATE(sp), 2),
				   samp);
	      cp->last_search_result = SEARCH_OK;
	      FREE(s1);
	      FREE(s2);
	      cursor_moveto(cp, samp);
	    }
	}
      else get_find_expression(sp, count);
    }
}

static XEN g_search_procedure(XEN snd)
{
  #define H_search_procedure "(" S_search_procedure " &optional index) -> global or sound-local search function"
  snd_state *ss;
  snd_info *sp;
  if (XEN_BOUND_P(snd))
    {
      ASSERT_SOUND(S_search_procedure, snd, 1);
      sp = get_sp(snd);
      if (sp)
	return(sp->search_proc);
      else return(XEN_FALSE);
    }
  ss = get_global_state();
  return(ss->search_proc);
}

static XEN g_set_search_procedure(XEN snd, XEN proc)
{
  snd_state *ss;
  snd_info *sp;
  char *error = NULL, *expr = NULL;
  XEN errstr;
  if (XEN_INTEGER_P(snd)) /* could be the proc arg if no snd */
    {
      ASSERT_SOUND("set-" S_search_procedure, snd, 1);
      sp = get_sp(snd);
      if (sp)
	{
	  if (XEN_PROCEDURE_P(sp->search_proc))
	    snd_unprotect(sp->search_proc);
	  sp->search_proc = XEN_UNDEFINED;
	  if (XEN_STRING_P(proc))
	    {
	      expr = copy_string(XEN_TO_C_STRING(proc));
	      proc = snd_catch_any(eval_str_wrapper, expr, expr);
	    }
	  error = procedure_ok(proc, 1, "find", "find", 1);
	  if (error == NULL)
	    {
	      sp->search_proc = proc;
	      snd_protect(proc);
	      if (sp->search_expr) FREE(sp->search_expr);
	      sp->search_expr = expr;
	      return(proc);
	    }
	  else 
	    {
	      errstr = C_TO_XEN_STRING(error);
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
      if (XEN_PROCEDURE_P(ss->search_proc))
	snd_unprotect(ss->search_proc);
      ss->search_proc = XEN_UNDEFINED;
      if (XEN_STRING_P(snd))
	{
	  expr = copy_string(XEN_TO_C_STRING(snd));
	  snd = snd_catch_any(eval_str_wrapper, expr, expr);
	}
      error = procedure_ok(snd, 1, "find", "find", 1);
      if (error == NULL)
	{
	  ss->search_proc = snd;
	  snd_protect(snd);
	  if (ss->search_expr) FREE(ss->search_expr);
	  ss->search_expr = expr;
	}
      else 
	{
	  errstr = C_TO_XEN_STRING(error);
	  FREE(error);
	  return(snd_bad_arity_error("set-" S_search_procedure, errstr, snd));
	}
    }
  return(snd);
}

#ifdef XEN_ARGIFY_1
XEN_ARGIFY_1(g_search_procedure_w, g_search_procedure)
XEN_ARGIFY_2(g_set_search_procedure_w, g_set_search_procedure)
#else
#define g_search_procedure_w g_search_procedure
#define g_set_search_procedure_w g_set_search_procedure
#endif

void g_init_find(void)
{
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_search_procedure, g_search_procedure_w, H_search_procedure,
				   "set-" S_search_procedure, g_set_search_procedure_w,  0, 1, 1, 1);
}
