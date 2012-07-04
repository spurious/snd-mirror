#include "snd.h"

static void clear_search_state(void)
{
  if (XEN_PROCEDURE_P(ss->search_proc)) 
    {
      snd_unprotect_at(ss->search_proc_loc);
      ss->search_proc_loc = NOT_A_GC_LOC;
    }
  ss->search_proc = XEN_UNDEFINED;

  if (ss->search_expr) free(ss->search_expr);
  ss->search_expr = NULL;
}


#if (!USE_NO_GUI)

static void find_report(chan_info *cp, const char *msg)
{
  if (cp)
    status_report(cp->sound, msg);
  find_dialog_set_label(msg);
}


static bool search_in_progress = false;
static chan_info *previous_channel = NULL;
#define MANY_PASSES 100000


static mus_long_t channel_find_forward(chan_info *cp, bool repeating)
{
  bool reported = false;
  mus_long_t i, end, start, passes;
  snd_fd *sf = NULL;
  XEN res = XEN_FALSE;

  end = CURRENT_SAMPLES(cp);
  start = CURSOR(cp) + 1;
  if (start >= end)
    start = 0;

  sf = init_sample_read(start, cp, READ_FORWARD);
  if (!sf)
    return(-1);

  ss->stopped_explicitly = false;
  for (i = start, passes = 0; i < end; i++, passes++)
    {
      res = XEN_CALL_1(ss->search_proc, 
		       C_TO_XEN_DOUBLE((double)(read_sample(sf))), 
		       "search function");
      if (XEN_NOT_FALSE_P(res)) 
	break;
      if (passes >= MANY_PASSES)
	{
	  passes = 0;
	  check_for_event();
	  if (!(ss->stopped_explicitly))
	    {
	      char *msg;
	      msg = mus_format("search at minute %d", (int)floor(i / (SND_SRATE(cp->sound) * 60)));
	      find_report(cp, msg);
	      free(msg);
	      reported = true;
	    }
	  /* if user types C-s during an active search, we risk stomping on our current pointers */
	  if (!(cp->sound->active)) break;
	}
      if (ss->stopped_explicitly) break;
    }

  ss->stopped_explicitly = false;
  if (reported) find_report(cp, NULL);
  free_snd_fd(sf);

  if (i < end)
    return(i);
  return(-1);
}


static mus_long_t channel_find_backward(chan_info *cp, bool repeating)
{
  bool reported = false;
  mus_long_t i, start, passes;
  snd_fd *sf = NULL;
  XEN res = XEN_FALSE;

  start = CURSOR(cp) - 1;
  if (start < 0)
    start = CURRENT_SAMPLES(cp) - 1;

  sf = init_sample_read(start, cp, READ_BACKWARD);
  if (!sf)
    return(-1);

  ss->stopped_explicitly = false;
  for (i = start, passes = 0; i >= 0; i--, passes++)
    {
      res = XEN_CALL_1(ss->search_proc, 
		       C_TO_XEN_DOUBLE((double)(read_sample(sf))), 
		       "search function");
      if (XEN_NOT_FALSE_P(res)) 
	break;
      if (passes >= MANY_PASSES)
	{
	  passes = 0;
	  check_for_event();
	  if (!(ss->stopped_explicitly))
	    {
	      char *msg;
	      msg = mus_format("search at minute %d", (int)floor(i / (SND_SRATE(cp->sound) * 60)));
	      find_report(cp, msg);
	      free(msg);
	      reported = true;
	    }
	  /* if user types C-s during an active search, we risk stomping on our current pointers */
	  if (!(cp->sound->active)) break;
	}
      if (ss->stopped_explicitly) break;
    }

  ss->stopped_explicitly = false;
  if (reported) find_report(cp, NULL);
  free_snd_fd(sf);

  if (i >= 0)
    return(i);
  return(-1);
}


static char *channel_search(chan_info *cp, read_direction_t direction, bool repeating)
{
  mus_long_t samp;
  char *s1, *s2, *msg;

  if (direction == READ_FORWARD)
    samp = channel_find_forward(cp, repeating);
  else samp = channel_find_backward(cp, repeating);
  
  previous_channel = cp;

  if (samp == -1)
    return(NULL);

  s1 = prettyf(chn_sample(samp, cp, cp->edit_ctr), 2);
  s2 = x_axis_location_to_string(cp, (double)samp / (double)SND_SRATE(cp->sound));
  msg = mus_format("%s at %s (%lld)", s1, s2, samp);
  cursor_moveto_without_verbosity(cp, samp);
  free(s1);
  free(s2);

  return(msg);
}


static char *global_search(read_direction_t direction, bool repeating)
{
  int i, j;

  if ((repeating) &&
      ((!previous_channel) ||
       (!(previous_channel->sound)) ||
       (!(previous_channel->sound->active))))
    repeating = false;
       
  for (i = 0; i < ss->max_sounds; i++)
    {
      snd_info *sp;
      chan_info *cp;

      sp = ss->sounds[i];
      if ((sp) &&
	  (sp->inuse == SOUND_NORMAL))
	for (j = 0; j < sp->nchans; j++)
	  {
	    cp = (chan_info *)(sp->chans[j]);
	    if ((!repeating) ||
		(cp == previous_channel))
	      {
		char *msg;
		repeating = false; /* after we find the channel, look at everything */
		msg = channel_search(cp, direction, repeating);
		if (msg)
		  return(msg);
	      }
	  }
    }
  return(NULL);
}


#if HAVE_EXTENSION_LANGUAGE
void find_dialog_find(char *str, read_direction_t direction, chan_info *cp)
{
  XEN proc;
  bool repeating_search = false;

  if (search_in_progress) 
    {
      find_report(cp, "search already in progress");
      return;
    }

  proc = XEN_FALSE;

  /* str can be null, or equal to the previous call's str -- in this case use
   *   the current search procedure if possible, else complain.
   * if str not null, make a new (local?) search-procedure
   */

  if ((!str) || 
      (!(*str)) ||
      (mus_strcmp(str, ss->search_expr)))
    {
      proc = ss->search_proc;
      if (!(XEN_PROCEDURE_P(proc)))
	return;
      repeating_search = true;
    }
  else
    {
      char *buf = NULL;

      redirect_errors_to(errors_to_find_text, NULL);
      proc = snd_catch_any(eval_str_wrapper, str, str);
      redirect_errors_to(NULL, NULL);

      if ((!(XEN_PROCEDURE_P(proc))) ||
	  (!(procedure_arity_ok(proc, 1))))
	return;

      clear_search_state(); /* free previous, if any */
      repeating_search = false;

      ss->search_proc = proc;
      ss->search_expr = mus_strdup(str);
      ss->search_proc_loc = snd_protect(proc);

      buf = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
      mus_snprintf(buf, PRINT_BUFFER_SIZE, "%s %s", I_find, str);
      find_dialog_set_label(buf);
      free(buf);
    }

  /* now we have a search procedure, possibly optimized */

  search_in_progress = true;
  find_dialog_stop_label(true);
  redirect_xen_error_to(stop_search_if_error, NULL);
  if (cp)
    str = channel_search(cp, direction, repeating_search);
  else str = global_search(direction, repeating_search);
  redirect_xen_error_to(NULL, NULL);
  find_dialog_stop_label(false);
  search_in_progress = false;

  if ((str) && (*str)) 
    {
      find_report(cp, str);
      free(str);
    }
  else find_report(cp, "not found");
}
#endif
#endif
/* end no gui */



/* -------------------------------------------------------------------------------- */

static XEN g_search_procedure(void)
{
  #define H_search_procedure "(" S_search_procedure "): the function used by the find dialog or C-s if none is otherwise specified."
  return(ss->search_proc);
}


static XEN g_set_search_procedure(XEN proc)
{
  char *error = NULL;
  XEN errstr;

  /* (set! (search-procedure) (lambda (y) #t)) -> #<procedure #f ((n) #t)> as "proc" */
  
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(proc) || XEN_FALSE_P(proc), proc, XEN_ONLY_ARG, S_setB S_search_procedure, "a procedure or " PROC_FALSE);

  error = procedure_ok(proc, 1, S_setB S_search_procedure, "proc", 1);
  if (!error)
    {
      clear_search_state();
      if (XEN_PROCEDURE_P(proc))
	{
	  ss->search_proc = proc;
	  ss->search_proc_loc = snd_protect(proc);
	}
    }
  else 
    {
      errstr = C_TO_XEN_STRING(error);
      free(error);
      return(snd_bad_arity_error(S_setB S_search_procedure, errstr, proc));
    }
  return(proc);
}


#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_search_procedure_w, g_search_procedure)
XEN_NARGIFY_1(g_set_search_procedure_w, g_set_search_procedure)
#else
#define g_search_procedure_w g_search_procedure
#define g_set_search_procedure_w g_set_search_procedure
#endif

void g_init_find(void)
{
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_search_procedure, g_search_procedure_w, H_search_procedure,
				   S_setB S_search_procedure, g_set_search_procedure_w,  0, 0, 1, 0);
}
