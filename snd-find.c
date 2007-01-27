#include "snd.h"

static bool search_in_progress = false;
typedef struct 
{
  int n; 
  read_direction_t direction; 
  int chans; 
  off_t inc, dur; 
  chan_info **cps; 
  snd_fd **fds;
} gfd;

#define MANY_PASSES 10000

static void prepare_global_search(chan_info *cp, void *g0)
{
  gfd *g = (gfd *)g0;
  off_t this_dur;
  read_direction_t direction;
  direction = g->direction;
  g->cps[g->n] = cp;
  if (direction == READ_FORWARD)
    {
      g->fds[g->n] = init_sample_read(CURSOR(cp) + 1, cp, direction);
      this_dur = CURRENT_SAMPLES(cp) - CURSOR(cp);
    }
  else
    {
      g->fds[g->n] = init_sample_read(CURSOR(cp) - 1, cp, direction);
      this_dur = CURSOR(cp);
    }
  if (this_dur > g->dur) g->dur = this_dur;
  if (g->fds[g->n] != NULL) g->n++;
}

#define KEEP_SEARCHING false
#define STOP_SEARCHING true

static bool run_global_search(gfd *g)
{
  if ((XEN_PROCEDURE_P(ss->search_proc)) || (ss->search_tree))
    {
      int i, j, k;
      for (i = 0; i < g->chans; i++)
	{
	  if (g->cps[i])
	    {
	      Float samp;
	      XEN res;
	      snd_fd *sf;
	      if (!((g->cps[i])->sound)) return(STOP_SEARCHING);
	      sf = g->fds[i]; 
	      samp = read_sample_to_float(sf);
	      if (ss->search_tree)
		{
		  if (evaluate_ptree_1f2b(ss->search_tree, samp))
		    {
		      g->n = i;
		      return(STOP_SEARCHING);
		    }
		}
	      else
		{
		  res = XEN_CALL_1(ss->search_proc,
				   C_TO_XEN_DOUBLE((double)(samp)), 
				   "global search func");
		  if (ss->stopped_explicitly)
		    return(STOP_SEARCHING);
		  if (XEN_TRUE_P(res))
		    {
		      g->n = i;
		      return(STOP_SEARCHING);
		    }
		  else
		    {
		      if (XEN_INTEGER_P(res))
			{
			  g->n = i; /* channel number */
			  if (g->direction == READ_FORWARD)
			    g->inc += XEN_TO_C_INT(res);
			  else g->inc -= XEN_TO_C_INT(res);
			  return(STOP_SEARCHING);
			}
		    }
		}
	      if (sf->at_eof)
		{
		  sf = free_snd_fd(sf);
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
		      return(STOP_SEARCHING);
		    }
		}
	    }
	}
    }
  g->inc++;
  return(KEEP_SEARCHING);
}

static char search_message[PRINT_BUFFER_SIZE];

char *global_search(read_direction_t direction)
{
  /* set up snd_fd for each active channel, 
   * tick each one forward until a match is found, 
   * update cursor/graph and report success (if any) in associated info window
   * subsequent runs (if no new text) repeat the search from the current locations
   */
  int chans, i, passes = 0, report_passes = 0;
  bool reporting = false;
  gfd *fd;
  chan_info *cp;
  if (search_in_progress) 
    {
      mus_snprintf(search_message, PRINT_BUFFER_SIZE, _("search in progress"));
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
   *    perhaps its should save the form, and evaluate that?
   */
  if (ss->search_tree == NULL)
    {
      if (ss->search_expr)
	{
	  /* search_expr can be null if user set search_proc directly */
	  clear_global_search_procedure(false);
	  ss->search_proc = snd_catch_any(eval_str_wrapper, ss->search_expr, ss->search_expr);
	  ss->search_proc_loc = snd_protect(ss->search_proc);
	}
    }
  search_in_progress = true;
  chans = active_channels(WITH_VIRTUAL_CHANNELS);
  search_message[0] = '\0';
  if (chans > 0)
    {
      fd = (gfd *)CALLOC(1, sizeof(gfd));
      fd->n = 0;
      fd->inc = 1;
      fd->direction = direction;
      fd->dur = 0;
      fd->chans = chans;
      fd->fds = (snd_fd **)CALLOC(chans, sizeof(snd_fd *));
      fd->cps = (chan_info **)CALLOC(chans, sizeof(chan_info *));
      for_each_normal_chan_with_void(prepare_global_search, (void *)fd);
      fd->n = -1;
      ss->stopped_explicitly = false;
      reporting = (fd->dur >= (REPORTING_SIZE * 10));
      if (reporting) set_find_dialog_label("0%");
      while (run_global_search(fd) == KEEP_SEARCHING)
	{
	  passes++;
	  if (passes >= MANY_PASSES)
	    {
	      check_for_event();
	      passes = 0;
	      fd->n = -1;
	      if (reporting)
		{
		  report_passes += MANY_PASSES;
		  if (report_passes > REPORTING_SIZE)
		    {
		      char buf[8];
		      mus_snprintf(buf, 8, "%d%%", (int)(100 * (double)(fd->inc) / (double)(fd->dur)));
		      set_find_dialog_label(buf);
		      report_passes = 0;
		    }
		}
	    }
	  if (ss->stopped_explicitly) break;
	}
      if (ss->stopped_explicitly)
	mus_snprintf(search_message, PRINT_BUFFER_SIZE, _("search stopped"));
      else
	{
	  if (fd->n == -1)
	    mus_snprintf(search_message, PRINT_BUFFER_SIZE, _("%s: not found"), ss->search_expr);
	  else
	    {
	      /* fd->n is winner, fd->inc is how far forward we searched from current cursor loc */
	      cp = fd->cps[fd->n];
	      set_find_dialog_label("");
	      cp->cursor_on = true;
	      if (direction == READ_FORWARD)
		cursor_move(cp, fd->inc);
	      else cursor_move(cp, -fd->inc);
	      /* now in its own info window show find state, and update graph if needed */
	      show_cursor_info(cp);
	      mus_snprintf(search_message, PRINT_BUFFER_SIZE, "found at " OFF_TD, CURSOR(cp));
	    }
	}
      ss->stopped_explicitly = false;
      for (i = 0; i < chans; i++) 
	free_snd_fd(fd->fds[i]);
      FREE(fd->fds);
      FREE(fd->cps);
      FREE(fd);
    }
  search_in_progress = false;
  return(search_message);
}

#define REPORT_TICKS 10
#define TREE_REPORT_TICKS 100

static bool find_eval_error_p = false;
static void send_find_errors_to_minibuffer(const char *msg, void *data)
{
  find_eval_error_p = true;
  display_minibuffer_error((snd_info *)data, msg);
  ss->stopped_explicitly = true;
}

static off_t cursor_find_forward(snd_info *sp, chan_info *cp, int count)
{
  int passes = 0, tick = 0;
  off_t i = 0, end, start;
  snd_fd *sf = NULL;
  XEN res = XEN_FALSE;
  bool progress_displayed = false;
  if (search_in_progress) 
    {
      display_minibuffer_error(sp, _("search already in progress"));
      return(-1);
    }
  search_in_progress = true;
  if (cp->last_search_result == SEARCH_OK)
    start = CURSOR(cp) + 1;
  else start = 0;
  sf = init_sample_read(start, cp, READ_FORWARD);
  if (!sf)
    {
      search_in_progress = false;
      return(-1);
    }
  end = CURRENT_SAMPLES(cp);
  ss->stopped_explicitly = false;
  if (sp->search_tree)
    {

      for (i = start; i < end; i++, tick++)
	{
	  if (evaluate_ptree_1f2b(sp->search_tree, read_sample_to_float(sf)))
	    {
	      count--; 
	      if (count == 0) break;
	    }
	  else
	    {
	      if (tick > (MANY_PASSES * TREE_REPORT_TICKS))
		{
		  char *msg;
		  check_for_event();
		  if (ss->stopped_explicitly) break;
		  tick = 0;
		  msg = mus_format("search at minute %d", (int)floor(i / (SND_SRATE(sp) * 60)));
		  display_minibuffer_error(sp, msg);
		  FREE(msg);
		  progress_displayed = true;
		}
	    }
	}
    }
  else
    {
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
	  if (passes >= MANY_PASSES)
	    {
	      check_for_event();
	      tick++;
	      if ((tick > REPORT_TICKS) && (!(ss->stopped_explicitly)))
		{
		  char *msg;
		  tick = 0;
		  msg = mus_format("search at minute %d", (int)floor(i / (SND_SRATE(sp) * 60)));
		  display_minibuffer_error(sp, msg);
		  FREE(msg);
		  progress_displayed = true;
		}
	      /* if user types C-s during an active search, we risk stomping on our current pointers */
	      if (!(sp->active)) break;
	      passes = 0;
	    }
	  if (ss->stopped_explicitly) break;
	}
    }
  ss->stopped_explicitly = false;
  if ((progress_displayed) &&
      (!find_eval_error_p))
    clear_minibuffer_error(sp);
  free_snd_fd(sf);
  search_in_progress = false;
  if (count != 0) return(-1); /* impossible sample number, so => failure */
  if (XEN_INTEGER_P(res))
    return(i + XEN_TO_C_INT(res));
  return(i);
}

static off_t cursor_find_backward(snd_info *sp, chan_info *cp, int count)
{
  off_t i = 0, start, tick = 0;
  int passes = 0;
  snd_fd *sf = NULL;
  XEN res = XEN_FALSE;
  bool progress_displayed = false;
  if (search_in_progress) 
    {
      display_minibuffer_error(sp, _("search already in progress"));
      return(-1);
    }
  search_in_progress = true;
  if (cp->last_search_result == SEARCH_OK)
    start = CURSOR(cp) - 1;
  else start = CURRENT_SAMPLES(cp) - 1;
  sf = init_sample_read(start, cp, READ_BACKWARD);
  if (!sf)
    {
      search_in_progress = false;
      return(-1);
    }
  ss->stopped_explicitly = false;
  if (sp->search_tree)
    {
      for (i = start; i >= 0; i--, tick++)
	{
	  if (evaluate_ptree_1f2b(sp->search_tree, read_sample_to_float(sf)))
	    {
	      count--; 
	      if (count == 0) break;
	    }
	  else
	    {
	      if (tick > (MANY_PASSES * TREE_REPORT_TICKS))
		{
		  char *msg;
		  check_for_event();
		  if (ss->stopped_explicitly) break;
		  tick = 0;
		  msg = mus_format("search at minute %d", (int)floor(i / (SND_SRATE(sp) * 60)));
		  display_minibuffer_error(sp, msg);
		  FREE(msg);
		  progress_displayed = true;
		}
	    }
	}
    }
  else
    {
      for (i = start, passes = 0; i >= 0; i--, passes++)
	{
	  /* sp search proc as ptree */
	  res = XEN_CALL_1(sp->search_proc, 
			   C_TO_XEN_DOUBLE((double)(read_sample_to_float(sf))), 
			   "local search func");
	  if (XEN_NOT_FALSE_P(res)) 
	    {
	      count--; 
	      if (count == 0) break;
	    }
	  if (passes >= MANY_PASSES)
	    {
	      check_for_event();
	      /* if user types C-s during an active search, we risk stomping on our current pointers */
	      tick++;
	      if (tick > REPORT_TICKS)
		{
		  char *msg;
		  tick = 0;
		  msg = mus_format("search at minute %d", (int)floor(i / (SND_SRATE(sp) * 60)));
		  display_minibuffer_error(sp, msg);
		  FREE(msg);
		  progress_displayed = true;
		}
	      if (!(sp->active)) break;
	      passes = 0;
	    }
	  if (ss->stopped_explicitly) break;
	}
    }
  ss->stopped_explicitly = false;
  if ((progress_displayed) &&
      (!find_eval_error_p))
    clear_minibuffer_error(sp);
  free_snd_fd(sf);
  search_in_progress = false;
  if (count != 0) return(-1); /* impossible sample number, so => failure */
  if (XEN_INTEGER_P(res))
    return(i - XEN_TO_C_INT(res));
  return(i);
}


static void get_find_expression(snd_info *sp, int count)
{
  set_minibuffer_string(sp, NULL, true);
  make_minibuffer_label(sp, "find:");
  sp->minibuffer_on = MINI_FIND;
  goto_minibuffer(sp);
  sp->search_count = count;
}

void cursor_search(chan_info *cp, int count)
{
  /* called only in snd-kbd so all errors should go to minibuffer, and search should be stopped upon error */
  snd_info *sp;
  sp = cp->sound;
  if (search_in_progress) 
    display_minibuffer_error(sp, _("search already in progress"));
  else
    {
      if (sp->search_count != 0)
	{
	  off_t samp;
	  if ((!(XEN_PROCEDURE_P(sp->search_proc))) && 
	      (sp->search_tree == NULL)) 
	    {
	      /* I decided not to check for a global search procedure; it's much harder
	       *   to implement than it looks, and I'm not sure it would "do the right thing"
	       *   anyway, if say the user types c-g to clear the current to ask for the next --
	       *   a fall-back then would be annoying.  And then, in the global searcher (edit:find)
	       *   to be consistent wouldn't we need a check for a local searcher?
	       *
	       * perhaps the right thing is to remove these procedures altogether
	       */
	      sp->search_count = 0;
	      clear_minibuffer_prompt(cp->sound);
	      return; /* no search expr */
	    }
	  if (sp->search_expr)
	    {
	      /* see note above about closures */
	      clear_sound_search_procedure(sp, false);
#if WITH_RUN
	      if (optimization(ss) > 0)
		sp->search_tree = form_to_ptree_1_b_without_env(C_STRING_TO_XEN_FORM(sp->search_expr));
#endif
	      if (sp->search_tree == NULL)
		{
		  redirect_errors_to(errors_to_minibuffer, (void *)sp);
		  sp->search_proc = snd_catch_any(eval_str_wrapper, sp->search_expr, sp->search_expr);
		  redirect_errors_to(NULL, NULL);
		  if (XEN_PROCEDURE_P(sp->search_proc))
		    sp->search_proc_loc = snd_protect(sp->search_proc);
		  else return;
		}
	    }
	  redirect_errors_to(send_find_errors_to_minibuffer, (void *)sp);
	  if (count > 0)
	    samp = cursor_find_forward(sp, cp, count);
	  else samp = cursor_find_backward(sp, cp, -count);
	  redirect_errors_to(NULL, NULL);
	  if (find_eval_error_p)
	    {
	      find_eval_error_p = false;
	      sp->search_count = 0;
	    }
	  else
	    {
	      if (samp == -1) 
		{ 
		  char *msg;
		  msg = mus_format("not found%s", 
				   (cp->last_search_result == SEARCH_FAILED) ? " (wrapped)" : "");
		  display_minibuffer_error(sp, msg);
		  FREE(msg);
		  cp->last_search_result = SEARCH_FAILED;
		}
	      else
		{
		  char *s1, *s2, *msg;
		  s1 = prettyf(chn_sample(samp, cp, cp->edit_ctr), 2);
		  s2 = x_axis_location_to_string(cp, (double)samp / (double)SND_SRATE(sp));
		  msg = mus_format("%s at %s (" OFF_TD ")", s1, s2, samp);
		  display_minibuffer_error(sp, msg);
		  FREE(s1);
		  FREE(s2);
		  FREE(msg);
		  cp->last_search_result = SEARCH_OK;
		  cursor_moveto_without_verbosity(cp, samp);
		}
	    }
	}
      else get_find_expression(sp, count);
    }
}

void clear_sound_search_procedure(snd_info *sp, bool clear_expr_too)
{
  if (XEN_PROCEDURE_P(sp->search_proc)) 
    {
      snd_unprotect_at(sp->search_proc_loc);
      sp->search_proc_loc = NOT_A_GC_LOC;
    }
  sp->search_proc = XEN_UNDEFINED;
  if (clear_expr_too)
    {
      if (sp->search_expr) FREE(sp->search_expr);
      sp->search_expr = NULL;
    }
  if (sp->search_tree)
    {
      free_ptree(sp->search_tree);
      sp->search_tree = NULL;
    }
}

void clear_global_search_procedure(bool clear_expr_too)
{
  if (XEN_PROCEDURE_P(ss->search_proc)) 
    {
      snd_unprotect_at(ss->search_proc_loc);
      ss->search_proc_loc = NOT_A_GC_LOC;
    }
  ss->search_proc = XEN_UNDEFINED;
  if (clear_expr_too)
    {
      if (ss->search_expr) FREE(ss->search_expr);
      ss->search_expr = NULL;
    }
  if (ss->search_tree) 
    {
      free_ptree(ss->search_tree);
      ss->search_tree = NULL;
    }
}

static XEN g_search_procedure(XEN snd)
{
  #define H_search_procedure "(" S_search_procedure " :optional snd): global (if no 'snd' specified) or sound-local search function"
  if (XEN_BOUND_P(snd))
    {
      snd_info *sp;
      ASSERT_SOUND(S_search_procedure, snd, 1);
      sp = get_sp(snd, NO_PLAYERS);
      if (sp)
	return(sp->search_proc);
      else return(XEN_FALSE);
    }
  return(ss->search_proc);
}

static XEN g_set_search_procedure(XEN snd, XEN proc)
{
  snd_info *sp;
  char *error = NULL;
  XEN errstr;
  /* (set! (search-procedure) (lambda (y) #t)) -> #<procedure #f ((n) #t)> as "proc" */
  /*   why is this different from ptree-channel's proc arg? */
  if (XEN_INTEGER_P(snd)) /* could be the proc arg if no snd */
    {
      ASSERT_SOUND(S_setB S_search_procedure, snd, 1);
      XEN_ASSERT_TYPE(XEN_PROCEDURE_P(proc) || XEN_FALSE_P(proc), proc, XEN_ARG_1, S_setB S_search_procedure, "a procedure or " PROC_FALSE);
      sp = get_sp(snd, NO_PLAYERS);

      if (sp)
	{
	  error = procedure_ok(proc, 1, S_setB S_search_procedure, "proc", 1);
	  if (error == NULL)
	    {
	      clear_sound_search_procedure(sp, true);
	      if (XEN_PROCEDURE_P(proc))
		{
		  sp->search_proc = proc;
		  sp->search_proc_loc = snd_protect(proc);
#if WITH_RUN
		  if (optimization(ss) > 0)
		    sp->search_tree = form_to_ptree_1_b(XEN_LIST_2(XEN_PROCEDURE_SOURCE(proc), proc));
#endif
		}
	      return(proc);
	    }
	  else 
	    {
	      errstr = C_TO_XEN_STRING(error);
	      FREE(error);
	      return(snd_bad_arity_error(S_setB S_search_procedure, errstr, proc));
	    }
	}
      else
	return(snd_no_such_sound_error(S_setB S_search_procedure, snd));
    }
  else 
    {
      XEN_ASSERT_TYPE(XEN_PROCEDURE_P(snd) || XEN_FALSE_P(snd), snd, XEN_ARG_1, S_setB S_search_procedure, "a procedure or " PROC_FALSE);
      error = procedure_ok(snd, 1, S_setB S_search_procedure, "proc", 1);
      if (error == NULL)
	{
	  clear_global_search_procedure(true);
	  if (XEN_PROCEDURE_P(snd))
	    {
	      ss->search_proc = snd;
	      ss->search_proc_loc = snd_protect(snd);
#if WITH_RUN
	      if (optimization(ss) > 0)
		ss->search_tree = form_to_ptree_1_b(XEN_LIST_2(XEN_PROCEDURE_SOURCE(snd), snd));
#endif
	    }
	}
      else 
	{
	  errstr = C_TO_XEN_STRING(error);
	  FREE(error);
	  return(snd_bad_arity_error(S_setB S_search_procedure, errstr, snd));
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
				   S_setB S_search_procedure, g_set_search_procedure_w,  0, 1, 1, 1);
}
