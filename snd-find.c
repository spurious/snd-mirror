#include "snd.h"

typedef struct {int n; int direction; int chans; int inc; chan_info **cps; snd_fd **fds;} gfd;

static int prepare_global_search (chan_info *cp, void *g0)
{
  gfd *g = (gfd *)g0;
  int direction;
  direction = g->direction;
  g->cps[g->n] = cp;
  g->fds[g->n] = init_sample_read((direction == READ_FORWARD) ? (cp->cursor+1) : (cp->cursor-1), cp, direction);
  g->n++;
  return(0);
}

static int run_global_search (snd_state *ss, gfd *g)
{
  /* return 0 until success or all eof */
  /* if success, n = winner (as aref index), if eofs, n=-1 */
#if HAVE_GUILE
  int i, j, k;
  Float samp;
  SCM res;
  snd_fd *sf;
  if (gh_procedure_p(ss->search_proc))
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
	      res = g_call1(ss->search_proc, TO_SCM_DOUBLE((double)(samp)), "global search func");
	      if (SCM_SYMBOLP(res))
		{
		  g->n = -1;
		  return(1);
		}
	      if (SCM_NFALSEP(res))
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
		    {
		      if (g->cps[i]) 
			{
			  k = 1;
			  break;
			}
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
#endif
  return(0);
}

static char search_no_luck[128];

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
      sprintf(search_no_luck, "search in progress");
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
      while (!(run_global_search(ss, fd)))
	{
	  passes++;
	  if (passes >= 10000)
	    {
	      check_for_event(ss);
	      if (ss->stopped_explicitly) break;
	      passes = 0;
	      fd->n = -1;
	    }
	}
      if (fd->n == -1)
	{
	  if (ss->stopped_explicitly)
	    sprintf(search_no_luck, "search stopped");
	  else sprintf(search_no_luck, "%s: not found", ss->search_expr);
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

static int cursor_find(snd_info *sp, chan_info *cp, int count, int end_sample)
{
  /* count > 0 -> search forward, else back */
#if HAVE_GUILE
  int i, c, inc, passes = 0;
  Float samp;
  snd_fd *sf;
  snd_state *ss;
  SCM res;
  ss = sp->state;
  if (ss->search_in_progress) 
    {
      report_in_minibuffer(sp, "search in progress");
      return(-1);
    }
  c = count;
  if (count > 0) 
    {
      i = cp->cursor + 1; 
      inc = 1;
    }
  else 
    {
      i = cp->cursor-1;
      c = -c;
      inc = -1;
      end_sample--;
    }
  ss->search_in_progress = 1;
  sf = init_sample_read(i, cp, (count > 0) ? READ_FORWARD : READ_BACKWARD);
  if (!sf)
    {
      ss->search_in_progress = 0;
      return(-1);
    }
  if (count > 0) sf->direction = READ_FORWARD; else sf->direction = READ_BACKWARD;
  while ((c > 0) && (i != end_sample) && (!read_sample_eof(sf)))
    {
      if (count > 0)
	samp = next_sample_to_float(sf);
      else samp = previous_sample_to_float(sf);
      res = g_call1(sp->search_proc, TO_SCM_DOUBLE((double)samp), "local search func");
      if (SCM_SYMBOLP(res)) break;
      if (SCM_NFALSEP(res)) 
	{
	  c--; 
	  if (c == 0) break;
	}
      i += inc;
      passes++;
      if (passes >= 10000)
	{
	  check_for_event(ss);
	  /* if user types C-s during an active search, we risk stomping on our current pointers */
	  if ((ss->stopped_explicitly) || (!(sp->inuse))) break;
	  passes = 0;
	}
    }
  ss->stopped_explicitly = 0;
  free_snd_fd(sf);
  ss->search_in_progress = 0;
  if (c != 0) return(-1); /* impossible sample number, so => failure */
  return(i);
#else
  return(-1);
#endif
}

static void get_find_expression(snd_info *sp, int count)
{
  /* clear previous ? */
  search_no_luck[0] = '\0';
  set_minibuffer_string(sp, search_no_luck);
  make_minibuffer_label(sp, "find:");
  sp->minibuffer_on = 1;
  goto_minibuffer(sp);
  sp->searching = count;
}

int cursor_search(chan_info *cp, int count)
{
#if HAVE_GUILE
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
      if (!(gh_procedure_p(sp->search_proc))) return(CURSOR_IN_VIEW); /* no search expr */
      if (count > 0)
	samp = cursor_find(sp, cp, count, current_ed_samples(cp));
      else samp = cursor_find(sp, cp, count, 0);
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
		  s2 = prettyf((double)samp/(double)SND_SRATE(sp), 2),
		  samp);
	  FREE(s1);
	  FREE(s2);
	}
      cursor_moveto(cp, samp);
      if ((cp->cursor >= (cp->axis)->losamp) && (cp->cursor <= (cp->axis)->hisamp))
	return(CURSOR_IN_VIEW);
      else return(CURSOR_IN_MIDDLE);
    }
  else get_find_expression(sp, count);
#endif
  return(CURSOR_IN_VIEW);
}
