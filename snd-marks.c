#include "snd.h"

/* to handle undo/redo cleanly, we keep the mark list as an array (indexed to edit_ctr)
 * of arrays of pointers to marks.  Any mark-related operation follows cp->edit_ctr's
 * array within the outer array.  This way, marks come and go in an intuitively clean
 * manner without endless list operations in this file.  The downside is that the
 * mark associated with a given id is actually a list of marks, and which one is
 * "current" can change at any time.
 */

static int sync_max = 0;
static int mark_id_counter = 0;

int mark_sync_max(void) 
{
  return(sync_max);
}

void set_mark_sync(mark *m, int val) 
{
  m->sync = val; 
  if (val > sync_max) 
    sync_max = val; 
}

static mark *make_mark_1(off_t samp, const char *name, int id, int sc)
{
  mark *mp;
  mp = (mark *)CALLOC(1, sizeof(mark));
  if (name) mp->name = copy_string(name); else mp->name = NULL;
  mp->samp = samp;
  mp->id = id;
  set_mark_sync(mp, sc);
  return(mp);
}

static mark *make_mark(off_t samp, const char *name) 
{
  return(make_mark_1(samp, name, mark_id_counter++, 0));
}

static mark *copy_mark(mark *m) 
{
  return(make_mark_1(m->samp, m->name, m->id, m->sync));
}

static mark *free_mark(mark *mp)
{
  if (mp)
    {
      if (mp->name) FREE(mp->name);
      FREE(mp);
    }
  return(NULL);
}

static mark *map_over_marks(chan_info *cp, mark *(*func)(chan_info *ncp, mark *mp1, void *p1), void *m, read_direction_t direction)
{
  if (cp->marks)
    {
      int marks, pos;
      mark **mps;
      pos = cp->edit_ctr;
      mps = cp->marks[pos];
      marks = cp->mark_ctr[pos];
      if (mps)
	{
	  mark *mp;
	  int i;
	  if (direction == READ_FORWARD)
	    {
	      for (i = 0; i <= marks; i++) 
		if (mps[i]) /* can be null if we're running delete_marks at a higher level and draw-mark-hook is active */
		  {
		    mp = (*func)(cp, mps[i], m);
		    if (mp) return(mp);
		  }
	    }
	  else
	    {
	      for (i = marks; i >= 0; i--) 
		if (mps[i])
		  {
		    mp = (*func)(cp, mps[i], m);
		    if (mp) return(mp);
		  }
	    }
	}
    }
  return(NULL);
}

static mark *map_over_marks_with_int(chan_info *cp, mark *(*func)(chan_info *ncp, mark *mp1, int val1), int value, read_direction_t direction)
{
  if (cp->marks)
    {
      int marks, pos;
      mark **mps;
      pos = cp->edit_ctr;
      mps = cp->marks[pos];
      marks = cp->mark_ctr[pos];
      if (mps)
	{
	  mark *mp;
	  int i;
	  if (direction == READ_FORWARD)
	    {
	      for (i = 0; i <= marks; i++) 
		if (mps[i]) /* can be null if we're running delete_marks at a higher level and draw-mark-hook is active */
		  {
		    mp = (*func)(cp, mps[i], value);
		    if (mp) return(mp);
		  }
	    }
	  else
	    {
	      for (i = marks; i >= 0; i--) 
		if (mps[i])
		  {
		    mp = (*func)(cp, mps[i], value);
		    if (mp) return(mp);
		  }
	    }
	}
    }
  return(NULL);
}

static mark *find_mark_id_1(chan_info *cp, mark *mp, int id)
{
  if (mp->id == id)
    return(mp); 
  return(NULL);
}

static mark *find_mark_from_id(int id, chan_info **cps, int pos)
{
  int i;
  for (i = 0; i < ss->max_sounds; i++)
    {
      chan_info *cp;
      snd_info *sp;
      int j;
      sp = ss->sounds[i];
      if ((sp) && (sp->inuse == SOUND_NORMAL))
	for (j = 0; j<(sp->nchans); j++)
	  if ((cp = ((chan_info *)(sp->chans[j]))))
	    {
	      if (pos < cp->marks_size) /* pos can be -1 */
		{
		  int old_pos;
		  mark *mp;
		  old_pos = cp->edit_ctr;
		  if (pos >= 0) cp->edit_ctr = pos;
		  /* memoization would have to be done here where we know cp->edit_ctr */
		  mp = map_over_marks_with_int(cp, find_mark_id_1, id, READ_FORWARD);
		  cp->edit_ctr = old_pos;
		  if (mp) 
		    {
		      if (cps) cps[0] = cp; 
		      return(mp);
		    }
		}
	      }
	  }
  return(NULL);
}

off_t mark_id_to_sample(int id)
{
  mark *m;
  m = find_mark_from_id(id, NULL, AT_CURRENT_EDIT_POSITION);
  if (m)
    return(m->samp);
  return(-1);
}

static mark *find_named_mark_1(chan_info *cp, mark *mp, void *uname)
{
  char *name = (char *)uname;
  if ((mp->name) && (name) && (strcmp(mp->name, name) == 0)) return(mp);
  else return(NULL);
}

static mark *find_named_mark(chan_info *cp, const char *name)
{
  return(map_over_marks(cp, find_named_mark_1, (void *)name, READ_FORWARD));
}

static mark *find_previous_mark_1(chan_info *cp, mark *mp, void *m)
{
  if (mp->samp < (*((off_t *)m))) 
    return(mp); 
  return(NULL);
}

static mark *find_previous_mark(off_t current_sample, chan_info *cp)
{
  return(map_over_marks(cp, find_previous_mark_1, (void *)(&current_sample), READ_BACKWARD));
}

static mark *find_next_mark_1(chan_info *cp, mark *mp, void *m)
{
  if (mp->samp > (*((off_t *)m))) 
    return(mp); 
  return(NULL);
}

static mark *find_next_mark(off_t current_sample, chan_info *cp)
{
  return(map_over_marks(cp, find_next_mark_1, (void *)(&current_sample), READ_FORWARD));
}

static mark* marks_off_1(chan_info *cp, mark *mp, void *ignore)
{
  mp->visible = false;
  return(NULL);
}

void marks_off(chan_info *cp)
{
  map_over_marks(cp, marks_off_1, NULL, READ_FORWARD);
}


static XEN draw_mark_hook;

#define PLAY_ARROW_SIZE 10

#if USE_MOTIF
  #define STRING_Y_OFFSET 6
#else
  #define STRING_Y_OFFSET -6
#endif

static void draw_mark_1(chan_info *cp, axis_info *ap, mark *mp, bool show)
{
  /* fields are samp and name */
  int len, top, cx, y0, y1;
  axis_context *ax;
  if (!(cp->graph_time_p)) return;
  if (XEN_HOOKED(draw_mark_hook))
    {
      XEN res = XEN_FALSE;
      res = run_progn_hook(draw_mark_hook,
			   XEN_LIST_1(C_TO_XEN_INT(mp->id)),
			   S_draw_mark_hook);
      if (XEN_TRUE_P(res))
	{
	  mp->visible = show;
	  return;
	}
    }
  top = ap->y_axis_y1;
  y1 = top;
  y0 = ap->y_axis_y0;
  if (mp->name) top += 10;
  cx = grf_x((double)(mp->samp) / (double)SND_SRATE(cp->sound), ap);
#if USE_MOTIF
  ax = mark_context(cp);
#else
  ax = copy_context(cp);
#endif
  if (mp->name)
    {
#if USE_MOTIF
      ax->current_font = ss->sgx->peaks_fontstruct->fid;
      XSetFont(ax->dp, ax->gc, ss->sgx->peaks_fontstruct->fid);
#else
  #if USE_GTK
      ax->current_font = PEAKS_FONT(ss);
  #endif
#endif
      len = mark_name_width(mp->name);
#if USE_GTK
      if (!show) /* erase mark */
	{
	  ax = erase_context(cp);
	  fill_rectangle(ax, (int)(cx - 0.5 * len), top - 13, len + 1, 12); /* this should dependent on TINY_FONT height */
	}
      else
	{
	  draw_string(ax, (int)(cx - 0.5 * len), y1 + STRING_Y_OFFSET, mp->name, strlen(mp->name));
	}
#else
      draw_string(ax, (int)(cx - 0.5 * len), y1 + STRING_Y_OFFSET, mp->name, strlen(mp->name));
#endif
    }
#if USE_GTK
  ax = mark_context(cp);
#endif
  fill_rectangle(ax,
		 cx - mark_tag_width(ss), top,
		 2 * mark_tag_width(ss), mark_tag_height(ss));
  draw_line(ax, cx, top + 4, cx, y0);
  fill_polygon(ax, 4,
	       cx,                   y0,
	       cx + PLAY_ARROW_SIZE, y0 +     PLAY_ARROW_SIZE,
	       cx,                   y0 + 2 * PLAY_ARROW_SIZE,
	       cx,                   y0);

  mp->visible = show;
}

static void draw_play_triangle(chan_info *cp, Locus x)
{
  int y0;
  y0 = ((axis_info *)(cp->axis))->y_axis_y0;
  draw_polygon(mark_context(cp), 4,
	       x,                   y0,
	       x + PLAY_ARROW_SIZE, y0 +     PLAY_ARROW_SIZE,
	       x,                   y0 + 2 * PLAY_ARROW_SIZE,
	       x,                   y0);
}

static void draw_mark(chan_info *cp, axis_info *ap, mark *mp)
{
  if (!(mp->visible)) draw_mark_1(cp, ap, mp, true);
}

static void erase_mark(chan_info *cp, axis_info *ap, mark *mp)
{
  if (mp->visible) draw_mark_1(cp, ap, mp, false);
}


typedef struct {
  int x, y;
  mark *all_done;
} mdata;

static mark *hit_mark_1(chan_info *cp, mark *mp, void *m)
{
  int mx;
  mdata *md = (mdata *)m;
  axis_info *ap;
  ap = cp->axis;
  if (mp->samp < ap->losamp) return(NULL);
  if (mp->samp > ap->hisamp) return(md->all_done); /* grf_x clips so we can be confused by off-screen marks */
  mx = grf_x((double)(mp->samp) / (double)SND_SRATE(cp->sound), cp->axis);
  if (mx > (md->x + mark_tag_width(ss))) return(md->all_done); /* past it */
  if (mx < (md->x - mark_tag_width(ss))) return(NULL);         /* before it */
  if (mp->name == NULL)                                    /* check y if unnamed */
    {
      if ((md->y >= ap->y_axis_y1) && 
	  (md->y <= (ap->y_axis_y1 + mark_tag_height(ss)))) 
	return(mp); 
      else return(NULL);
    }
  else return(mp);
}

static mark *hit_triangle_1(chan_info *cp, mark *mp, void *m)
{
  /* m->samp = raw x mouse position */
  /* we're going left to right, so after passing x, give up */
  int mx, y;
  mdata *md = (mdata *)m;
  axis_info *ap;
  ap = cp->axis;
  if (mp->samp < ap->losamp) return(NULL);
  if (mp->samp > ap->hisamp) return(md->all_done); /* grf_x clips so we can be confused by off-screen marks */
  mx = grf_x((double)(mp->samp) / (double)SND_SRATE(cp->sound), cp->axis);
  if (mx > md->x) return(md->all_done);
  if ((mx + PLAY_ARROW_SIZE) < md->x) return(NULL);
  y = md->y - ap->y_axis_y0 - PLAY_ARROW_SIZE;
  if (y < 0) y = -y;
  if ((mx + PLAY_ARROW_SIZE - y) >= md->x) return(mp);
  /* the last is assuming the triangle shape for hit detection */
  return(NULL);
}

mark *hit_triangle(chan_info *cp, int x, int y)
{
  if (cp->marks)
    {
      axis_info *ap;
      ap = cp->axis;
      /* first check that we're in the bottom portion of the graph where the mark triangles are */
      if ((y >= ap->y_axis_y0) && 
	  (y <= (ap->y_axis_y0 + 2 * PLAY_ARROW_SIZE)))
	{
	  mark *mp;
	  mdata *md;
	  md = (mdata *)CALLOC(2, sizeof(mdata));
	  md->x = x;
	  md->y = y;
	  md->all_done = (mark *)1;
	  mp = map_over_marks(cp, hit_triangle_1, (void *)md, READ_FORWARD);
	  if (mp == (mark *)1) mp = NULL;
	  FREE(md);
	  return(mp);
	}
    }
  return(NULL);
}


static XEN mark_drag_hook;
static XEN mark_hook; /* add, delete, move */

static bool watching_mouse = false; /* this is tracking axis moves */
static int last_mouse_x = 0;
static mark *moving_mark = NULL; /* used only while "off-screen" during axis moves */

static void move_axis_to_track_mark(chan_info *cp);
static Cessator watch_mouse_button = 0;
static Cessate WatchMouse(Indicium cp)
{
  if (watch_mouse_button)
    {
      move_axis_to_track_mark((chan_info *)cp);
      return((Cessate)BACKGROUND_CONTINUE);
    }
  else return((Cessate)BACKGROUND_QUIT);
}

static void start_mark_watching(chan_info *cp, mark *mp)
{
  moving_mark = mp;
  watch_mouse_button = BACKGROUND_ADD(WatchMouse, cp);
  watching_mouse = true;
}

static void cancel_mark_watch(chan_info *cp)
{
  if (watch_mouse_button) BACKGROUND_REMOVE(watch_mouse_button);
  watch_mouse_button = 0;
  watching_mouse = false;
  moving_mark = NULL;
}

static bool move_mark_1(chan_info *cp, mark *mp, int x)
{
  axis_info *ap;
  int nx;
  off_t samps;
  bool redraw;
  ap = cp->axis;
  redraw = (!watching_mouse);
  if ((x > ap->x_axis_x1) || (x < ap->x_axis_x0)) 
    {
      if (watching_mouse)
	{
	  if (((x < ap->x_axis_x0) && (ap->x0 == ap->xmin)) ||
	      ((x > ap->x_axis_x1) && (ap->x1 == ap->xmax)))
	    return(false);
	}
      nx = move_axis(cp, ap, x);
      if (!watching_mouse) start_mark_watching(cp, mp);
    }
  else 
    {
      erase_mark(cp, ap, mp);
      nx = x;
      if (watching_mouse) 
	{
	  cancel_mark_watch(cp); 
	  redraw = false;
	}
    }
  mp->samp = (off_t)(ungrf_x(ap, nx) * SND_SRATE(cp->sound));
  if (mp->samp < 0) mp->samp = 0;
  samps = CURRENT_SAMPLES(cp);
  if (mp->samp > samps) mp->samp = samps;
  if (XEN_HOOKED(mark_drag_hook))
    run_hook(mark_drag_hook,
	     XEN_LIST_1(C_TO_XEN_INT(mp->id)),
	     S_mark_drag_hook);
  return(redraw);
}

static int compare_mark_samps(const void *mp1, const void *mp2)
{
  mark *m1, *m2;
  m1 = (mark *)(*((mark **)mp1));
  m2 = (mark *)(*((mark **)mp2));
  if (m1->samp < m2->samp) return(-1);
  if (m1->samp == m2->samp) return(0);
  return(1);
}

static void sort_marks(chan_info *cp)
{
  mark **mps;
  int ed;
  ed = cp->edit_ctr;
  mps = cp->marks[ed];
  qsort((void *)mps, cp->mark_ctr[ed] + 1, sizeof(mark *), compare_mark_samps);
}


static Locus prev_cx = -1;

off_t move_play_mark(chan_info *cp, off_t *mc, Locus cx)
{
  /* mc = mouse loc sampwise return samps updating mc */
  off_t cur_mc;
  axis_info *ap;
  ap = cp->axis;
  if (prev_cx > 0) draw_play_triangle(cp, prev_cx);
  prev_cx = cx;
  draw_play_triangle(cp, cx);
  cur_mc = (*mc);
  (*mc) = (off_t)(ungrf_x(ap, cx) * SND_SRATE(cp->sound));
  return((*mc) - cur_mc);
}

void finish_moving_play_mark(chan_info *cp)
{
  snd_info *sp;
  sp = cp->sound;
  draw_play_triangle(cp, prev_cx);
  prev_cx = -1;
  sp->speed_control = 1.0;
}

typedef enum {MARK_ADD, MARK_DELETE, MARK_MOVE, MARKS_DELETE, MARK_RELEASE} mark_hook_reason_t;

static void run_mark_hook(chan_info *cp, int id, mark_hook_reason_t reason)
{
  /* called after the mark list has been made consistent */
  if (XEN_HOOKED(mark_hook))
    run_hook(mark_hook,
	     XEN_LIST_4(C_TO_XEN_INT(id),
			C_TO_XEN_INT(cp->sound->index),
			C_TO_XEN_INT(cp->chan),
			C_TO_XEN_INT((int)reason)),
	     S_mark_hook);
  run_watchers();
}

static void allocate_marks(chan_info *cp, int edit_ctr)
{
  int i;
  cp->marks_size = edit_ctr + 16;
  cp->marks = (mark ***)CALLOC(cp->marks_size, sizeof(mark **));
  cp->mark_size = (int *)CALLOC(cp->marks_size, sizeof(int));
  cp->mark_ctr = (int *)CALLOC(cp->marks_size, sizeof(int));
  for (i = 0; i < cp->marks_size; i++) cp->mark_ctr[i] = -1;
}

#define MARKS_ALLOC_SIZE 16

mark *add_mark(off_t samp, const char *name, chan_info *cp)
{
  int i, ed, med;
  mark **mps;
  if (!(cp->marks)) allocate_marks(cp, cp->edit_ctr);
  /* our current mark list is cp->edit_ctr (it starts at 0 -- see snd-chn.c and snd-edits.c) */
  ed = cp->edit_ctr;
  cp->mark_ctr[ed]++;
  if (cp->mark_ctr[ed] >= cp->mark_size[ed])
    {
      cp->mark_size[ed] += MARKS_ALLOC_SIZE;
      if (!cp->marks[ed]) 
	cp->marks[ed] = (mark **)CALLOC(cp->mark_size[ed], sizeof(mark *));
      else 
	{
	  cp->marks[ed] = (mark **)REALLOC(cp->marks[ed], cp->mark_size[ed] * sizeof(mark *));
	  for (i = cp->mark_size[ed] - MARKS_ALLOC_SIZE; i < cp->mark_size[ed]; i++) cp->marks[ed][i] = NULL;
	}
    }
  mps = cp->marks[ed];
  med = cp->mark_ctr[ed];
  if (med == 0)
    {
      if (mps[0]) free_mark(mps[0]);
      mps[0] = make_mark(samp, name);
      run_mark_hook(cp, mps[0]->id, MARK_ADD);
      return(mps[0]);
    }
  else
    {
      for (i = 0; i < med; i++) /* not <= because we pre-incremented above */
	{
	  mark *mp;
	  mp = mps[i];
	  if (samp < mp->samp)
	    {
	      int j;
	      if (mps[med]) free_mark(mps[med]);
	      for (j = med; j > i; j--)
		mps[j] = mps[j - 1];
	      mps[i] = make_mark(samp, name);
	      run_mark_hook(cp, mps[i]->id, MARK_ADD);
	      return(mps[i]);
	    }
	}
      /* insert at end */
      if (mps[med]) free_mark(mps[med]);
      mps[med] = make_mark(samp, name);
      run_mark_hook(cp, mps[med]->id, MARK_ADD);
      return(mps[med]);
    }
}

bool delete_mark_samp(off_t samp, chan_info *cp)
{
  if ((cp) && (cp->marks))
    {
      int i, ed;
      mark **mps;
      ed = cp->edit_ctr;
      mps = cp->marks[ed];
      if (mps)
	{
	  int edm;
	  edm = cp->mark_ctr[ed];
	  for (i = 0; i <= edm; i++)
	    {
	      mark *mp;
	      mp = mps[i];
	      if (mp->samp == samp)
		{
		  axis_info *ap;
		  int id = -1;
		  ap = cp->axis;
		  if ((mp->samp >= ap->losamp) && (mp->samp <= ap->hisamp)) erase_mark(cp, ap, mp); 
		  id = mp->id;
		  free_mark(mp);
		  mps[i] = NULL;
		  if (i < edm)
		    {
		      int j;
		      for (j = i; j < edm; j++) mps[j] = mps[j + 1];
		      mps[edm] = NULL;
		    }
		  cp->mark_ctr[ed]--;
		  run_mark_hook(cp, id, MARK_DELETE);
		  return(true);
		}
	    }
	}
    }
  return(false);
}

static bool delete_mark_id(int id, chan_info *cp)
{
  if ((cp) && (cp->marks))
    {
      int ed;
      mark **mps;
      ed = cp->edit_ctr;
      mps = cp->marks[ed];
      if (mps)
	{
	  int i, edm;
	  edm = cp->mark_ctr[ed];
	  for (i = 0; i <= edm; i++)
	    {
	      mark *mp;
	      mp = mps[i];
	      if (mp->id == id)
		{
		  axis_info *ap;
		  ap = cp->axis;
		  if ((mp->samp >= ap->losamp) && (mp->samp <= ap->hisamp)) erase_mark(cp, ap, mp); 
		  free_mark(mp);
		  mps[i] = NULL;
		  if (i < edm)
		    {
		      int j;
		      for (j = i; j < edm; j++) mps[j] = mps[j + 1];
		      mps[edm] = NULL;
		    }
		  cp->mark_ctr[ed]--;
		  run_mark_hook(cp, id, MARK_DELETE);
		  return(true);
		}
	    }
	}
    }
  return(false);
}

static void delete_marks(chan_info *cp)
{
  if ((cp) && (cp->marks))
    {
      int i, ed;
      mark **mps;
      ed = cp->edit_ctr;
      mps = cp->marks[ed];
      if (mps)
	{
	  for (i = 0; i < cp->mark_size[ed]; i++)
	    {
	      mark *mp;
	      mp = mps[i];
	      if (mp) 
		{
		  axis_info *ap;
		  ap = cp->axis;
		  if ((mp->samp >= ap->losamp) && (mp->samp <= ap->hisamp)) erase_mark(cp, ap, mp); 
		  free_mark(mp);
		  mps[i] = NULL;
		}
	    }
	  cp->mark_ctr[ed] = -1;
	  run_mark_hook(cp, -1, MARKS_DELETE);
	}
    }
}

void free_mark_list(chan_info *cp, int ignore)
{
  if (cp->marks)
    {
      int i;
      for (i = 0; i < cp->marks_size; i++) 
	{
	  if (i != ignore)
	    {
	      mark **mps;
	      mps = cp->marks[i];
	      if (mps)
		{
		  int j;
		  for (j = 0; j < cp->mark_size[i]; j++)
		    {
		      mark *mp;
		      mp = mps[j];
		      if (mp) free_mark(mp);
		      mps[j] = NULL;
		    }
		  FREE(mps);
		}
	    }
	}
      FREE(cp->marks);
      cp->marks = NULL;
      cp->marks_size = 0;
      FREE(cp->mark_ctr);
      cp->mark_ctr = NULL;
      FREE(cp->mark_size);
      cp->mark_size = NULL;
    }
}

void backup_mark_list(chan_info *cp, int cur)
{
  if (cp->marks)
    {
      release_pending_marks(cp, cur - 1); /* frees mark list at cur - 1, but not the top pointer */
      if (cp->marks[cur - 1]) FREE(cp->marks[cur - 1]); /* not freed by release_pending_marks */
      cp->marks[cur - 1] = cp->marks[cur];
      cp->marks[cur] = NULL;
      cp->mark_ctr[cur - 1] = cp->mark_ctr[cur];
      cp->mark_ctr[cur] = -1;
      cp->mark_size[cur - 1] = cp->mark_size[cur]; /* oops!! this was omitted until 19-Dec-06 */
      cp->mark_size[cur] = 0;
      /* if mark size is not reset, a large as-one-edit backup involving more than 16 marks can ripple
       *   to a state where the current marks entry thinks ctr > size!  This causes a subsequent ripple_marks
       *   or free_mark_list to run off the end of the marks array freeing random pointers!
       */
#if MUS_DEBUGGING
      if (cp->mark_size[cur - 1] < cp->mark_ctr[cur - 1]) 
	fprintf(stderr, "mark array size: %d, ctr: %d\n", cp->mark_size[cur - 1], cp->mark_ctr[cur - 1]);
#endif
    }
}

void collapse_marks(snd_info *sp)
{
  /* in all channels, move current edit_ctr mark list to 0, freeing all the rest */
  int i;
  for (i = 0; i < sp->nchans; i++)
    {
      chan_info *cp;
      cp = sp->chans[i];
      if ((cp) && (cp->marks))
	{
	  int ed;
	  mark **mps;
	  ed = cp->edit_ctr;
	  mps = cp->marks[ed];
	  /* this is the one to save */
	  if (mps)
	    {
	      int len, size;
	      len = cp->mark_ctr[ed];
	      size = cp->mark_size[ed];
	      free_mark_list(cp, ed);
	      allocate_marks(cp, 0);
	      cp->marks[0] = mps;
	      cp->mark_ctr[0] = len;
	      cp->mark_size[0] = size;
	    }
	}
    }
}


/* save and restore across update-sound */

typedef struct {
  mark **marks;
  int ctr;
  int size;
} mark_info;

typedef struct {
  mark_info **ms;
  int size;
} marks_info;

void *sound_store_marks(snd_info *sp)
{
  /* in all channels, move current edit_ctr mark list to 0, freeing all the rest */
  int i;
  mark_info **res = NULL;
  marks_info *rtn = NULL;
  res = (mark_info **)CALLOC(sp->nchans, sizeof(mark_info *));
  rtn = (marks_info *)CALLOC(1, sizeof(marks_info));
  rtn->ms = res;
  rtn->size = sp->nchans;
  for (i = 0; i < sp->nchans; i++)
    {
      chan_info *cp;
      cp = sp->chans[i];
      if ((cp) && (cp->marks))
	{
	  int ed;
	  mark **mps;
	  ed = cp->edit_ctr;
	  mps = cp->marks[ed];
	  if ((mps) && (cp->mark_ctr[ed] >= 0))
	    {
	      int j;
	      res[i] = (mark_info *)CALLOC(1, sizeof(mark_info));
	      res[i]->marks = (mark **)CALLOC(cp->mark_size[ed], sizeof(mark *)); 
	      for (j = 0; j <= cp->mark_ctr[ed]; j++)
		res[i]->marks[j] = copy_mark(mps[j]);
	      res[i]->ctr = cp->mark_ctr[ed];
	      res[i]->size = cp->mark_size[ed];
	    }
	}
    }
  return((void *)rtn);
}

void sound_restore_marks(snd_info *sp, void *mrk)
{
  marks_info *mrks = (marks_info *)mrk;
  if (mrks)
    {
      int i, lim;
      mark_info **marks;
      marks = mrks->ms;
      lim = mrks->size;
      if (sp->nchans < lim) lim = sp->nchans; /* update can change channel number either way */
      for (i = 0; i < lim; i++)
	{
	  if (marks[i])
	    {
	      chan_info *cp;
	      cp = sp->chans[i];
	      allocate_marks(cp, 0);
	      cp->marks[0] = marks[i]->marks;
	      cp->mark_ctr[0] = marks[i]->ctr;
	      cp->mark_size[0] = marks[i]->size;
	    }
	}
      for (i = 0; i < mrks->size; i++)
	if (marks[i]) FREE(marks[i]);
      /* possible memleak here if chan num has lessened */
      FREE(marks);
      FREE(mrks);
    }
}


static mark *find_nth_mark(chan_info *cp, int count)
{
  int i, c;
  off_t samp;
  mark *mp = NULL;
  if ((!cp) || (!cp->marks)) return(NULL);
  if (count > 0) c = count; else c = -count;
  samp = CURSOR(cp);
  for (i = 0; i < c; i++)
    {
      if (count > 0) mp = find_next_mark(samp, cp);
      else mp = find_previous_mark(samp, cp);
      if (!mp) break;
      samp = mp->samp;
    }
  return(mp);
}

bool goto_mark(chan_info *cp, int count)
{
  mark *mp;
  if ((!cp) || (!cp->marks)) return(false);
  mp = find_nth_mark(cp, count);
  if (!mp) return(false);
  cursor_moveto(cp, mp->samp);
  return(true);
}

void goto_named_mark(chan_info *cp, const char *name)
{
  mark *mp;
  mp = find_named_mark(cp, name);
  if (mp) cursor_moveto(cp, mp->samp);
}

static mark *active_mark_1(chan_info *cp, mark *mp, void *ignore)
{
  axis_info *ap;
  ap = cp->axis;
  if ((mp->samp >= ap->losamp) && (mp->samp <= ap->hisamp)) return(mp); 
  return(NULL);
}

mark *active_mark(chan_info *cp)
{
  return(map_over_marks(cp, active_mark_1, NULL, READ_FORWARD));
}

off_t mark_beg(chan_info *cp)
{
  /* called only in snd-chn.c for active zoom */
  mark *mp;
  mp = active_mark(cp);
  if (mp) return(mp->samp);
  return(-1);
}

typedef struct {
  off_t last_samp;
} dpy_last;

static mark *display_channel_marks_1(chan_info *cp, mark *mp, void *m)
{
  axis_info *ap;
  dpy_last *ls = (dpy_last *)m;
  ap = cp->axis;
  if (mp->samp > ap->hisamp) return(mp); /* terminates loop */
  if (mp->samp == ls->last_samp)
    return(NULL); 
  /* actually this should notice how wide in samples the mark stem is and avoid any redraw until we get to the next clear pixel */
  else ls->last_samp = mp->samp;         /* avoid drawing twice at same point == erase */
  if ((mp->samp >= ap->losamp) && 
      (mp->samp <= ap->hisamp) && 
      (mp != moving_mark))
    draw_mark(cp, ap, mp);
  return(NULL);
}

void display_channel_marks(chan_info *cp)
{
  if ((cp->marks) && (cp->show_marks))
    {
      dpy_last ls;
      ls.last_samp = -1;
      map_over_marks(cp, display_channel_marks_1, (void *)(&ls), READ_FORWARD);
    }
}

void release_pending_marks(chan_info *cp, int edit_ctr)
{
  /* free the mark list at edit_ctr */
  if ((cp) && (cp->marks))
    {
      mark **mps;
      mps = cp->marks[edit_ctr];
      if (mps)
	{
	  int j;
	  for (j = 0; j < cp->mark_size[edit_ctr]; j++) /* was <= cp->mark_ctr[edit_ctr] */
	    {
	      mark *mp;
	      mp = mps[j];
	      if (mp) free_mark(mp);
	      mps[j] = NULL;
	    }
	  cp->mark_ctr[edit_ctr] = -1;
	}
    }
}

void ripple_marks(chan_info *cp, off_t beg, off_t change)
{
  /* if change = 0, just set ptr, else copy and fixup with deletions */
  /* this is called after the tree has been pushed forward, so edit_ctr is ahead of us */
  /* but we don't do anything if no marks */

  if ((cp) && (cp->marks))
    {
      int old_m, new_m, i;
      mark **mps;
      if (cp->edit_ctr == 0) return;
      old_m = cp->edit_ctr - 1;
      new_m = cp->edit_ctr;
      if (new_m >= cp->marks_size) /* groan -- we have to realloc the base array of array of pointers! */
	{
	  int old_size;
	  old_size = cp->marks_size;
	  cp->marks_size += MARKS_ALLOC_SIZE;
	  if (new_m >= cp->marks_size) cp->marks_size = new_m + MARKS_ALLOC_SIZE;
	  cp->marks = (mark ***)REALLOC(cp->marks, cp->marks_size * sizeof(mark **));
	  cp->mark_size = (int *)REALLOC(cp->mark_size, cp->marks_size * sizeof(int));
	  cp->mark_ctr = (int *)REALLOC(cp->mark_ctr, cp->marks_size * sizeof(int));
	  for (i = old_size; i < cp->marks_size; i++) 
	    {
	      cp->mark_ctr[i] = -1;
	      cp->mark_size[i] = 0;
	      cp->marks[i] = NULL;
	    }
	}

      /* release current */
      if (cp->marks[new_m])
	{
	  mps = cp->marks[new_m];
	  for (i = 0; i < cp->mark_size[new_m]; i++)
	    if (mps[i])
	      {
		free_mark(mps[i]);
		mps[i] = NULL;
	      }
	  FREE(mps);
	  cp->marks[new_m] = NULL;
	}

      /* copy old with position change */
      cp->mark_ctr[new_m] = cp->mark_ctr[old_m];
      cp->mark_size[new_m] = cp->mark_size[old_m];

      if (cp->mark_size[new_m] > 0)
	{
	  cp->marks[new_m] = (mark **)CALLOC(cp->mark_size[new_m], sizeof(mark *));
	  if (cp->mark_ctr[new_m] >= 0)
	    {
	      mark **mpo;
	      mark *mp;
	      mps = cp->marks[new_m];
	      mpo = cp->marks[old_m];
	      for (i = 0; i <= cp->mark_ctr[new_m]; i++)
		mps[i] = copy_mark(mpo[i]);
	      if (change < 0)
		{
		  off_t end;
		  /* if (change < 0) and any marks are between beg and beg+change, they must be deleted */
		  end = beg - change - 1;
		  i = 0;
		  while (i <= cp->mark_ctr[new_m])
		    {
		      mp = mps[i];
		      if ((mp->samp >= beg) && (mp->samp <= end)) /* was mp->samp > beg, ditto end, beg can = end */
			delete_mark_samp(mp->samp, cp);           /* changes cp->mark_ctr, hence the while loop?  */
		      else 
			{
			  if (mp->samp > beg)                     /* don't change marks that precede the point of the change */
			    mp->samp += change;
			  i++;
			}
		    }
		}
	      else
		{
		  if (change > 0)
		    for (i = 0; i <= cp->mark_ctr[new_m]; i++)
		      {
			mp = mps[i];
			if (mp->samp > beg) mp->samp += change;
		      }
		}
	    }
	}
    }
}

bool mark_define_region(chan_info *cp, int count)
{
  if ((cp) && (max_regions(ss) > 0))
    {
      if (cp->marks)
	{
	  off_t beg;
	  mark *mp;
	  beg = CURSOR(cp);
	  mp = find_nth_mark(cp, count);
	  if (mp)
	    {
	      off_t end;
	      end = mp->samp;
	      if (end != beg)
		{
		  off_t ends[1];
		  sync_info *si;
		  int i;
		  ends[0] = end;
		  if (end < beg) 
		    {
		      ends[0] = beg;
		      beg = end;
		    }
		  deactivate_selection();
		  si = sync_to_chan(cp);
		  si->begs[0] = beg;
		  define_region(si, ends);
		  for (i = 0; i < si->chans; i++)
		    {
		      reactivate_selection(si->cps[i], beg, ends[0]);
		      update_graph(si->cps[i]);
		    }
		  si = free_sync_info(si);
		  return(true);
		}
	    }
	}
    }
  return(false);
}

static mark *reverse_mark_1(chan_info *cp, mark *mp, void *um)
{
  mark *m = (mark *)um;
  mp->samp = m->samp - mp->samp;
  return(NULL);
}

void reverse_marks(chan_info *cp, off_t beg, off_t dur) /* beg -1 for full sound */
{
  mark *m;
  mark **mps;
  int ed;
  if (cp->marks == NULL) return;
  ed = cp->edit_ctr;
  mps = cp->marks[ed];
  if (mps == NULL) return;
  if (beg == -1)
    {
      m = make_mark_1(CURRENT_SAMPLES(cp) - 1, NULL, 0, 0);
      map_over_marks(cp, reverse_mark_1, (void *)m, READ_FORWARD);
      free_mark(m);
    }
  else
    {
      off_t end;
      int i, marks;
      end = beg + dur - 1;
      marks = cp->mark_ctr[ed];
      for (i = 0; i <= marks; i++) 
	{
	  m = mps[i];
	  if ((m->samp >= beg) && (m->samp <= end))
	    m->samp = end - (m->samp - beg);
	}
    }
  if (cp->mark_ctr[ed] >= 0)
    qsort((void *)mps, cp->mark_ctr[ed] + 1, sizeof(mark *), compare_mark_samps);
}

void src_marks(chan_info *cp, Float ratio, off_t old_samps, off_t new_samps, off_t beg, bool over_selection)
{
  if (cp->marks)
    {		
      int i, marks, pos;
      mark *m;
      mark **mps;
      pos = cp->edit_ctr;
      mps = cp->marks[pos];
      marks = cp->mark_ctr[pos];
      if ((mps) && (marks >= 0))
	{
	  if (!over_selection)
	    {
	      for (i = 0; i <= marks; i++) 
		{
		  m = mps[i];
		  if (ratio > 0.0)
		    m->samp = (off_t)(m->samp / ratio);
		  else m->samp = (off_t)((old_samps - 1 - m->samp) / (-ratio)); /* ratio < 0 here */
		}
	    }
	  else
	    {
	      off_t end;
	      end = beg + old_samps - 1;
	      for (i = 0; i <= marks; i++) 
		{
		  m = mps[i];
		  if ((m->samp >= beg) && (m->samp <= end))
		    {
		      if (ratio > 0.0)
			m->samp = beg + (off_t)((m->samp - beg) / ratio);
		      else m->samp = beg + (off_t)((old_samps - 1 - (m->samp - beg)) / (-ratio));
		    }
		  else
		    {
		      if (m->samp > end)
			m->samp += (new_samps - old_samps);
		    }
		}
	    }
	  if (ratio < 0.0) qsort((void *)mps, marks + 1, sizeof(mark *), compare_mark_samps);
	}
    }
}

void reset_marks(chan_info *cp, int cur_marks, off_t *samps, off_t end, off_t extension, bool over_selection)
{
  if (cp->marks)
    {		
      int marks, pos;
      mark **mps;
      pos = cp->edit_ctr;
      mps = cp->marks[pos];
      marks = cp->mark_ctr[pos];
      if ((mps) && (marks >= 0))
	{
	  int i;
	  mark *m;
	  if (over_selection)
	    for (i = 0; i <= marks; i++) 
	      {
		m = mps[i];
		if (m->samp > end) m->samp += extension;
	      }
	  for (i = 0; (i <= marks) && (i < cur_marks); i++) 
	    {
	      m = mps[i];
	      if (samps[i] >= 0) m->samp = samps[i];
	    }
	  qsort((void *)mps, marks + 1, sizeof(mark *), compare_mark_samps);
	}
    }
}

void ripple_trailing_marks(chan_info *cp, off_t beg, off_t old_len, off_t new_len)
{
  if (cp->marks)
    {		
      int marks, pos;
      mark **mps;
      ripple_marks(cp, 0, 0);
      pos = cp->edit_ctr;
      mps = cp->marks[pos];
      marks = cp->mark_ctr[pos];
      if ((mps) && (marks >= 0))
	{
	  int i;
	  for (i = 0; i <= marks; i++) 
	    {
	      mark *m;
	      m = mps[i];
	      if (m->samp > (beg + old_len)) m->samp += (new_len - old_len);
	    }
	}
    }
}

void swap_marks(chan_info *cp0, chan_info *cp1)
{
  if ((cp0->marks) || (cp1->marks))
    {
      mark **mps0 = NULL, **mps1 = NULL;
      int ctr0 = -1, ctr1 = -1;
      int size0 = 0, size1 = 0;
      int pos0, pos1;
      pos0 = cp0->edit_ctr;
      pos1 = cp1->edit_ctr;
      if (cp0->marks)
	{
	  mps0 = cp0->marks[pos0];
	  ctr0 = cp0->mark_ctr[pos0];
	  size0 = cp0->mark_size[pos0];
	}
      if (cp1->marks)
	{
	  mps1 = cp1->marks[pos1];
	  ctr1 = cp1->mark_ctr[pos1];
	  size1 = cp1->mark_size[pos1];
	}
      if ((cp0->marks) && (cp1->marks == NULL))
	allocate_marks(cp1, pos1);
      if ((cp1->marks) && (cp0->marks == NULL))
	allocate_marks(cp0, pos0);
      cp0->marks[pos0] = mps1;
      cp0->mark_ctr[pos0] = ctr1;
      cp0->mark_size[pos0] = size1;
      cp1->marks[pos1] = mps0;
      cp1->mark_ctr[pos1] = ctr0;
      cp1->mark_size[pos1] = size0;
    }
}


/* -------------------------------- SYNCD AND DRAGGED MARKS -------------------------------- */

typedef struct {
  mark **marks;
  chan_info **chans;
  int marks_size;
  int mark_ctr;
  int sync;
  off_t *initial_samples;
} syncdata;

static syncdata *make_syncdata(int sync)
{
  syncdata *sd;
  sd = (syncdata *)CALLOC(1, sizeof(syncdata));
  sd->sync = sync;
  sd->mark_ctr = 0;
  sd->marks_size = 8;
  sd->marks = (mark **)CALLOC(sd->marks_size, sizeof(mark *));
  sd->chans = (chan_info **)CALLOC(sd->marks_size, sizeof(chan_info *));
  sd->initial_samples = (off_t *)CALLOC(sd->marks_size, sizeof(off_t));
  return(sd);
}

static void add_syncd_mark(syncdata *sd, mark *mp, chan_info *cp)
{
  sd->marks[sd->mark_ctr] = mp;
  sd->initial_samples[sd->mark_ctr] = mp->samp;
  sd->chans[sd->mark_ctr++] = cp;
  if (sd->mark_ctr == sd->marks_size)
    {
      int i;
      sd->marks = (mark **)REALLOC(sd->marks, sd->marks_size * 2 * sizeof(mark *));
      sd->chans = (chan_info **)REALLOC(sd->chans, sd->marks_size * 2 * sizeof(chan_info *));
      /* why was initial_samples missing? 2-May-02 */
      sd->initial_samples = (off_t *)REALLOC(sd->initial_samples, sd->marks_size * 2 * sizeof(off_t));
      for (i = sd->marks_size; i < sd->marks_size * 2; i++) {sd->marks[i] = NULL; sd->chans[i] = NULL;}
      sd->marks_size *= 2;
    }
}

static mark *gather_local_syncd_marks(chan_info *cp, mark *mp, void *usd)
{
  syncdata *sd = (syncdata *)usd;
  if (sd->sync == mp->sync)
    add_syncd_mark(sd, mp, cp);
  return(NULL);
}

static void gather_chan_syncd_marks(chan_info *cp, void *sd)
{
  map_over_marks(cp, gather_local_syncd_marks, sd, READ_FORWARD);
}

static syncdata *gather_syncd_marks(int sync)
{
  syncdata *sd;
  sd = make_syncdata(sync);
  for_each_normal_chan_with_void(gather_chan_syncd_marks, (void *)sd);
  return(sd);
}

static syncdata *free_syncdata(syncdata *sd)
{
  if (sd)
    {
      if (sd->marks) FREE(sd->marks);
      if (sd->initial_samples) FREE(sd->initial_samples);
      if (sd->chans) FREE(sd->chans);
      FREE(sd);
    }
  return(NULL);
}

static bool mark_control_clicked = false; /* C-click of mark -> drag data as mark is dragged */
static off_t mark_initial_sample = 0;
static syncdata *mark_sd = NULL;
static mix_context **mark_movers = NULL;

static void initialize_md_context(int size, chan_info **cps)
{
  int i;
  mark_movers = (mix_context **)CALLOC(size, sizeof(mix_context *));
  for (i = 0; i < size; i++)
    {
      mix_context *ms;
      mark_movers[i] = make_mix_context(cps[i]);
      ms = mark_movers[i];
      ms->lastpj = make_graph(cps[i]); 
      mix_save_graph(ms, ms->lastpj);
    }
}

static void finalize_md_context(int size)
{
  if (mark_movers)
    {
      int i;
      for (i = 0; i < size; i++) 
	if (mark_movers[i]) 
	  free_mix_context(mark_movers[i]);
      FREE(mark_movers);
      mark_movers = NULL;
    }
}

mark *hit_mark(chan_info *cp, int x, int y, int key_state)
{
  mark *mp = NULL;
  if (cp->marks)
    {
      axis_info *ap;
      ap = cp->axis;
      /* first check that we're in the top portion of the graph where the mark tabs are */
      if ((y >= ap->y_axis_y1) && 
	  (y <= (ap->y_axis_y1 + mark_tag_height(ss) + 10)))               /*  + 10 for named marks -- checked again later */
	{
	  mdata *md;
	  md = (mdata *)CALLOC(1, sizeof(mdata));
	  md->x = x;
	  md->y = y;
	  md->all_done = (mark *)1;
	  mp = map_over_marks(cp, hit_mark_1, (void *)md, READ_FORWARD);
	  if (mp == (mark *)1) mp = NULL;
	  FREE(md);
	  if (mp)
	    {
	      mark_control_clicked = (key_state & snd_ControlMask);
	      if (mp->sync != 0) 
		{
		  if (mark_sd) mark_sd = free_syncdata(mark_sd);
		  mark_sd = gather_syncd_marks(mp->sync);
		}
	      if (mark_control_clicked)
		{
		  mark_initial_sample = mp->samp;
		  if (mark_sd) 
		    {
		      if ((mark_sd->mark_ctr > 1) &&
			  (mark_sd->marks[0] != mp))
			{
			  mark *tm;
			  int loc = 1;
			  off_t ts;
			  chan_info *tc;
			  for (loc = 1; loc < mark_sd->mark_ctr; loc++)
			    if (mark_sd->marks[loc] == mp) break;
			  if (loc < mark_sd->mark_ctr)
			    {
			      tm = mark_sd->marks[0];
			      ts = mark_sd->initial_samples[0];
			      tc = mark_sd->chans[0];
			      mark_sd->marks[0] = mark_sd->marks[loc];
			      mark_sd->initial_samples[0] = mark_sd->initial_samples[loc];
			      mark_sd->chans[0] = mark_sd->chans[loc];
			      mark_sd->marks[loc] = tm;
			      mark_sd->initial_samples[loc] = ts;
			      mark_sd->chans[loc] = tc;
			    }
			}
		      initialize_md_context(mark_sd->mark_ctr, mark_sd->chans);
		    }
		  else initialize_md_context(1, &cp);
		}
	    }
	}
    }
  return(mp);
}

static void make_mark_graph(chan_info *cp, off_t initial_sample, off_t current_sample, int which);

static bool move_syncd_mark(chan_info *cp, mark *m, int x)
{
  off_t old_samp, diff;
  bool redraw;
  old_samp = m->samp;
  redraw = move_mark_1(cp, m, x);
  diff = m->samp - old_samp;
  if (diff != 0)
    {
      if ((mark_sd) && (mark_sd->mark_ctr > 1))
	{
	  int i;
	  for (i = 0; i < mark_sd->mark_ctr; i++)
	    {
	      mark *mp;
	      mp = mark_sd->marks[i];
	      if (mp != m)
		{
		  axis_info *ap;
		  off_t samps;
		  chan_info *ncp;
		  ncp = mark_sd->chans[i];
		  ap = ncp->axis;
		  if ((mp->samp >= ap->losamp) && 
		      (mp->samp <= ap->hisamp)) 
		    erase_mark(ncp, ap, mp);
		  mp->samp += diff;
		  if (mp->samp < 0) mp->samp = 0;
		  samps = CURRENT_SAMPLES(ncp);
		  if (mp->samp > samps) mp->samp = samps;
		  if (mark_control_clicked)
		    make_mark_graph(ncp, mark_sd->initial_samples[i], mp->samp, i);
		  if ((mp->samp >= ap->losamp) && 
		      (mp->samp <= ap->hisamp)) 
		    draw_mark(ncp, ap, mp);
		}
	    }
	}
    }
  return(redraw);
}

static void move_axis_to_track_mark(chan_info *cp)
{
  if (moving_mark)
    {
      bool redraw;
      if (moving_mark->sync)
	redraw = move_syncd_mark(cp, moving_mark, last_mouse_x);
      else redraw = move_mark_1(cp, moving_mark, last_mouse_x);
      if (redraw) draw_mark(cp, cp->axis, moving_mark);
    }
}

void move_mark(chan_info *cp, mark *mp, int x) /* from mouse drag callback in snd-chn.c, called whenever mark is visible */
{
  bool redraw;
  last_mouse_x = x;
  if (mp->sync)
    redraw = move_syncd_mark(cp, mp, x);
  else redraw = move_mark_1(cp, mp, x);
  if (mark_control_clicked)
    make_mark_graph(cp, mark_initial_sample, mp->samp, 0);
  if (redraw) draw_mark(cp, cp->axis, mp);
}

static void edit_dragged_mark(chan_info *cp, mark *m, off_t initial_sample)
{
  /* edit -- initial_sample is where we were when the drag started, ended at m->samp */
  off_t num, mark_final_sample;
  int id;
  mark *new_m;
  mark_final_sample = m->samp;
  num = mark_final_sample - initial_sample;
  m->samp = initial_sample;
  id = m->id;
  if (num > 0)
    extend_with_zeros(cp, initial_sample, num, cp->edit_ctr);
      /* at this point, old mark pointer is irrelevant (it lives in the previous edit history list) */
      /*   but since the ripple didn't touch it, we need to move it forward to reflect the insertion */
  else 
    if (num < 0)
      {
	new_m = map_over_marks_with_int(cp, find_mark_id_1, id, READ_FORWARD);
	new_m->samp = initial_sample;
	delete_samples(mark_final_sample, -num, cp, cp->edit_ctr);
      }
  if (num != 0) 
    {
      new_m = map_over_marks_with_int(cp, find_mark_id_1, id, READ_FORWARD);
      new_m->samp = mark_final_sample;
      update_graph(cp);
    }
}

void finish_moving_mark(chan_info *cp, mark *m) /* button release called from snd-chn.c */
{
  if (watching_mouse) cancel_mark_watch(cp);
  if ((m->sync != 0) && (mark_sd))
    {
      int i;
      if (XEN_HOOKED(mark_hook))
	for (i = 0; i < mark_sd->mark_ctr; i++)
	  run_mark_hook(mark_sd->chans[i], mark_sd->marks[i]->id, MARK_RELEASE);
      if (mark_control_clicked)
	{
	  for (i = mark_sd->mark_ctr - 1; i >= 0; i--)
	    {
	      /* do the edits in reverse order on the assumption that marks sharing a channel were ordered to begin with,
	       *   so they'll happen in reverse order here, so the lower sample edits in rippling won't affect the higher
	       */
	      mark *sdm;
	      sdm = mark_sd->marks[i];
	      edit_dragged_mark(mark_sd->chans[i], sdm, mark_sd->initial_samples[i]);
	    }
	  finalize_md_context(mark_sd->mark_ctr);
	}
      for (i = 0; i < mark_sd->mark_ctr; i++)
	if (mark_sd->chans[i])
	  {
	    int j;
	    sort_marks(mark_sd->chans[i]); /* resort marks in case movement scrambled them */
	    for (j = i + 1; j < mark_sd->mark_ctr; j++)    /* only sort each channel once */
	      if (mark_sd->chans[j] == mark_sd->chans[i])
		mark_sd->chans[j] = NULL;
	  }
    }
  else 
    {
      run_mark_hook(cp, m->id, MARK_RELEASE);
      if (mark_control_clicked) 
	{
	  edit_dragged_mark(cp, m, mark_initial_sample);
	  finalize_md_context(1);
	}
      sort_marks(cp);
    }
  if (mark_sd) mark_sd = free_syncdata(mark_sd);
}

void play_syncd_mark(chan_info *cp, mark *m)
{
  syncdata *sd;
  sd = gather_syncd_marks(m->sync);
  if ((sd) && (sd->mark_ctr > 0))
    play_channels(sd->chans, sd->mark_ctr, sd->initial_samples, NULL, IN_BACKGROUND, 
		  C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), false, "play sync'd mark", 0);
  if (sd) free_syncdata(sd);
}

static void make_mark_graph(chan_info *cp, off_t initial_sample, off_t current_sample, int which)
{
  snd_info *sp;
  int j = 0;
  off_t i, k, samps;
  axis_info *ap;
  double samples_per_pixel, samp, x, incr;  
  int pixels;
  snd_fd *sf = NULL;
  int x_start, x_end;
  double start_time = 0.0, cur_srate = 1.0;
  sp = cp->sound;
  ap = cp->axis;
  cur_srate = (double)SND_SRATE(sp);
  ap->losamp = (off_t)(ap->x0 * cur_srate);
  if (ap->losamp < 0) ap->losamp = 0;
  if (ap->x0 != ((double)(ap->losamp) / cur_srate)) ap->losamp++;
  start_time = (double)(ap->losamp) / cur_srate;
  ap->hisamp = (off_t)(ap->x1 * cur_srate);
  if ((ap->losamp == 0) && (ap->hisamp == 0)) return;
  x_start = ap->x_axis_x0;
  x_end = ap->x_axis_x1;
  samps = ap->hisamp - ap->losamp + 1;
  if ((x_start == x_end) && (samps > 10)) return; /* must be too-tiny graph */
  pixels = x_end - x_start;
  if (pixels >= POINT_BUFFER_SIZE) pixels = POINT_BUFFER_SIZE - 1;
  if ((x_start == x_end) || (samps <= 1))
    samples_per_pixel = 0.01; /* any non-zero value < 1.0 should be ok here */
  else samples_per_pixel = (Float)((double)(samps - 1) / (Float)pixels);

  /* this is assuming one dragged mark per channel */
  if ((samples_per_pixel < 5.0) && (samps < POINT_BUFFER_SIZE))
    {
      sf = init_sample_read(ap->losamp, cp, READ_FORWARD);
      if (sf == NULL) return;
      incr = (double)1.0 / cur_srate;
      if (current_sample < initial_sample)
	{
	  for (j = 0, i = ap->losamp, x = start_time; i <= ap->hisamp; i++, j++, x += incr)
	    {
	      if (i == current_sample) 
		for (k = current_sample; k < initial_sample; k++) 
		  read_sample(sf);
	      set_grf_point(grf_x(x, ap), j, grf_y(read_sample_to_float(sf), ap));
	    }
	}
      else
	{
	  for (j = 0, i = ap->losamp, x = start_time; i <= ap->hisamp; i++, j++, x += incr)
	    {
	      if ((i < initial_sample) || (i >= current_sample)) 
		samp = read_sample_to_float(sf);
	      else samp = 0.0;
	      set_grf_point(grf_x(x, ap), j, grf_y(samp, ap));
	    }
	}
      erase_and_draw_grf_points(mark_movers[which], cp, j);
    }
  else
    {
      mus_sample_t ymin, ymax, msamp;
      Locus xi;
      double xf;
      if (amp_env_usable(cp, samples_per_pixel, ap->hisamp, false, cp->edit_ctr, (samps > AMP_ENV_CUTOFF)))
	{
	  /* needs two sets of pointers and a frame within the amp env:
	   *   sample given mark edit: i and xk
	   *   sample within (original, unedited) amp env: ii and xki (xf)
	   *   frame bounds within amp env if relevant: k and kk
	   * this is confusing code!
	   */
	  double step, xk, xki;
	  off_t ii, kk;
	  env_info *ep;
	  ep = cp->amp_envs[cp->edit_ctr];
	  step = samples_per_pixel / (Float)(ep->samps_per_bin);
	  xf = (double)(ap->losamp) / (double)(ep->samps_per_bin);
	  j = 0;
	  x = ap->x0;
	  xi = grf_x(x, ap);
	  i = ap->losamp;
	  ii = ap->losamp;
	  xk = i;
	  xki = (double)(ap->losamp);
	  while (i <= ap->hisamp)
	    {
	      k = (off_t)xf;
	      kk = (off_t)(xf + step);
	      if (((current_sample >= initial_sample) && 
		   (i >= initial_sample) && 
		   (i < current_sample)) ||
		  (kk >= ep->amp_env_size))
		{
		  ymin = MUS_SAMPLE_0;
		  ymax = MUS_SAMPLE_0;
		}
	      else
		{
		  ymin = ep->fmax;
		  ymax = ep->fmin;
		  for (; k <= kk; k++)
		    {
		      if (ep->data_min[k] < ymin) ymin = ep->data_min[k];
		      if (ep->data_max[k] > ymax) ymax = ep->data_max[k];
		    }
		}
	      set_grf_points(xi++, j++,
			     grf_y(MUS_SAMPLE_TO_FLOAT(ymin), ap),
			     grf_y(MUS_SAMPLE_TO_FLOAT(ymax), ap));
	      xk += samples_per_pixel;
	      i = (off_t)xk;
	      if ((current_sample < initial_sample) && 
		  (ii >= current_sample) && 
		  (ii < initial_sample))
		{
		  xf = (Float)((double)initial_sample / (Float)ep->samps_per_bin);
		  ii = initial_sample;
		  xki = (double)initial_sample;
		}
	      else 
		{
		  if ((current_sample < initial_sample) ||
		      (i >= current_sample) ||
		      (i < initial_sample))
		    {
		      xf += step;
		      xki += samples_per_pixel;
		      ii = (off_t)xki;
		    }
		}
	    }
	  erase_and_draw_both_grf_points(mark_movers[which], cp, j);
	}
      else
	{
	  sf = init_sample_read(ap->losamp, cp, READ_FORWARD);
	  if (sf == NULL) return;
	  j = 0;      /* graph point counter */
	  x = ap->x0;
	  xi = grf_x(x, ap);
	  xf = 0.0;     /* samples per pixel counter */
	  ymin = MUS_SAMPLE_MAX;
	  ymax = MUS_SAMPLE_MIN;
	  if (current_sample < initial_sample) 
	    {
	      for (i = ap->losamp, xf = 0.0; i <= ap->hisamp; i++)
		{
		  if (i == current_sample) 
		    for (k = current_sample; k < initial_sample; k++) 
		      read_sample(sf);
		  msamp = read_sample(sf);
		  if (msamp > ymax) ymax = msamp;
		  if (msamp < ymin) ymin = msamp;
		  xf += 1.0;
		  if (xf > samples_per_pixel)
		    {
		      set_grf_points(xi, j, 
				     grf_y(MUS_SAMPLE_TO_FLOAT(ymin), ap), 
				     grf_y(MUS_SAMPLE_TO_FLOAT(ymax), ap));
		      xi++;
		      j++;
		      xf -= samples_per_pixel;
		      ymin = MUS_SAMPLE_MAX;
		      ymax = MUS_SAMPLE_MIN;
		    }
		}
	    }
	  else
	    {
	      for (i = ap->losamp, xf = 0.0; i <= ap->hisamp; i++)
		{
		  if ((i < initial_sample) || (i >= current_sample))
		    msamp = read_sample(sf);
		  else msamp = MUS_SAMPLE_0;
		  if (msamp > ymax) ymax = msamp;
		  if (msamp < ymin) ymin = msamp;
		  xf += 1.0;
		  if (xf > samples_per_pixel)
		    {
		      set_grf_points(xi, j, 
				     grf_y(MUS_SAMPLE_TO_FLOAT(ymin), ap), 
				     grf_y(MUS_SAMPLE_TO_FLOAT(ymax), ap));
		      xi++;
		      j++;
		      xf -= samples_per_pixel;
		      ymin = MUS_SAMPLE_MAX;
		      ymax = MUS_SAMPLE_MIN;
		    }
		}
	    }
	  erase_and_draw_both_grf_points(mark_movers[which], cp, j);
	}
    }
  free_snd_fd(sf);
}

static XEN snd_no_such_mark_error(const char *caller, XEN id)
{
  XEN_ERROR(XEN_ERROR_TYPE("no-such-mark"),
	    XEN_LIST_2(C_TO_XEN_STRING(caller),
		       id));
  return(XEN_FALSE);
}

#if MUS_DEBUGGING && HAVE_SCHEME
/* too hard to do this via mouse events in snd-test, so do it by hand here */
static XEN g_test_control_drag_mark(XEN snd, XEN chn, XEN mid)
{
  int x, y;
  off_t cx;
  chan_info *cp;
  mark *m = NULL, *m1 = NULL;
  cp = get_cp(snd, chn, "test-C-mark");
  if (!cp) return(XEN_FALSE);
  m = find_mark_from_id(XEN_TO_C_INT(mid), NULL, AT_CURRENT_EDIT_POSITION);
  if (m == NULL) 
    return(snd_no_such_mark_error("test-C-mark", mid));
  y = cp->axis->y_axis_y1;
  if (m->name) y += 10;
  x = grf_x((double)(m->samp) / (double)SND_SRATE(cp->sound), cp->axis);
  m1 = hit_mark(cp, x, y + 1, snd_ControlMask);
  if (m != m1)
    {
      fprintf(stderr, "ah rats! ");
      abort();
    }
  move_mark(cp, m, x - 50);
  finish_moving_mark(cp, m);
  x = grf_x((double)(m->samp) / (double)SND_SRATE(cp->sound), cp->axis);
  y = cp->axis->y_axis_y0 + 2;
  hit_triangle(cp, x, y);
  cx = m->samp + 50;
  move_play_mark(cp, &cx, x + 50);
  finish_moving_play_mark(cp);
  return(mid);
}
#endif


typedef enum {MARK_SAMPLE, MARK_NAME, MARK_SYNC, MARK_HOME} mark_field_t;

static XEN mark_get(XEN n, mark_field_t fld, XEN pos_n, const char *caller)
{
  int pos;
  chan_info *ncp[1];
  mark *m = NULL;
  pos = XEN_TO_C_INT_OR_ELSE(pos_n, AT_CURRENT_EDIT_POSITION);
  m = find_mark_from_id(XEN_TO_C_INT_OR_ELSE(n, 0), ncp, pos);
  if (m == NULL) 
    return(snd_no_such_mark_error(caller, n));
  switch (fld)
    {
    case MARK_SAMPLE: 
      return(C_TO_XEN_OFF_T(m->samp)); 
      break;
    case MARK_SYNC:   
      return(C_TO_XEN_INT(m->sync)); 
      break;
    case MARK_NAME:   
      if (m->name) 
	return(C_TO_XEN_STRING(m->name)); 
      else return(C_TO_XEN_STRING("")); 
      break;
    case MARK_HOME:   
      return(XEN_LIST_2(C_TO_XEN_INT((ncp[0]->sound)->index),
			C_TO_XEN_INT(ncp[0]->chan))); 
      break;
    }
  return(XEN_FALSE);
}

static XEN mark_set(XEN mark_n, XEN val, mark_field_t fld, const char *caller)
{
  chan_info *cp[1];
  mark *m;
  m = find_mark_from_id(XEN_TO_C_INT(mark_n), cp, AT_CURRENT_EDIT_POSITION);
  if (m == NULL) 
    return(snd_no_such_mark_error(caller, mark_n));
  switch (fld)
    {
    case MARK_SAMPLE: 
      m->samp = mus_oclamp(0, 
			   XEN_TO_C_OFF_T_OR_ELSE(val, 0),
			   CURRENT_SAMPLES(cp[0]));
      sort_marks(cp[0]); /* update and re-sort current mark list */
      run_mark_hook(cp[0], m->id, MARK_MOVE);
      update_graph(cp[0]);
      break;
    case MARK_SYNC: 
      if (XEN_INTEGER_P(val))
	set_mark_sync(m, XEN_TO_C_INT(val));
      else set_mark_sync(m, (int)XEN_TO_C_BOOLEAN(val));
      break;
    case MARK_NAME:
      if (m->name) FREE(m->name);
      if (XEN_FALSE_P(val))
	m->name = NULL;
      else m->name = copy_string(XEN_TO_C_STRING(val));
      update_graph(cp[0]);
      break;
    default:
      break;
    }
  return(val);
}

static XEN g_mark_p(XEN id_n)
{
  #define H_mark_p "(" S_mark_p " id): " PROC_TRUE " if mark is active"
  if (XEN_INTEGER_P(id_n))
    return(C_TO_XEN_BOOLEAN(find_mark_from_id(XEN_TO_C_INT(id_n), NULL, AT_CURRENT_EDIT_POSITION)));
  return(XEN_FALSE);
}

#if WITH_RUN
bool r_mark_p(int n);
bool r_mark_p(int n)
{
  return((bool)(find_mark_from_id(n, NULL, AT_CURRENT_EDIT_POSITION)));
}

off_t r_mark_sample(int n);
off_t r_mark_sample(int n)
{
  mark *m;
  m = find_mark_from_id(n, NULL, AT_CURRENT_EDIT_POSITION);
  if (m) return(m->samp);
  return(-1);
}

off_t r_mark_sync(int n);
off_t r_mark_sync(int n)
{
  mark *m;
  m = find_mark_from_id(n, NULL, AT_CURRENT_EDIT_POSITION);
  if (m) return(m->sync);
  return(-1);
}

char *r_mark_name(int n);
char *r_mark_name(int n)
{
  mark *m;
  m = find_mark_from_id(n, NULL, AT_CURRENT_EDIT_POSITION);
  if (m) return(m->name);
  return(NULL);
}
#endif

static XEN g_mark_sample(XEN mark_n, XEN pos_n) 
{
  #define H_mark_sample "(" S_mark_sample " :optional id pos): mark's location (sample number) at edit history pos"
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(mark_n), mark_n, XEN_ARG_1, S_mark_sample, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(pos_n), pos_n, XEN_ARG_2, S_mark_sample, "an integer");
  return(mark_get(mark_n, MARK_SAMPLE, pos_n, S_mark_sample));
}

static XEN g_set_mark_sample(XEN mark_n, XEN samp_n) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(mark_n), mark_n, XEN_ARG_1, S_setB S_mark_sample, "an integer");
  XEN_ASSERT_TYPE(XEN_OFF_T_P(samp_n) || XEN_NOT_BOUND_P(samp_n), samp_n, XEN_ARG_2, S_setB S_mark_sample, "an integer");
  return(mark_set(mark_n, samp_n, MARK_SAMPLE, S_setB S_mark_sample));
}

static XEN g_mark_sync(XEN mark_n) 
{
  #define H_mark_sync "(" S_mark_sync " :optional id): mark's sync value (default: 0)"
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(mark_n), mark_n, XEN_ONLY_ARG, S_mark_sync, "an integer");
  return(mark_get(mark_n, MARK_SYNC, XEN_UNDEFINED, S_mark_sync));
}

static XEN g_set_mark_sync(XEN mark_n, XEN sync_n) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(mark_n), mark_n, XEN_ARG_1, S_setB S_mark_sync, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_P(sync_n), sync_n, XEN_ARG_2, S_setB S_mark_sync, "an integer");
  return(mark_set(mark_n, sync_n, MARK_SYNC, S_setB S_mark_sync));
}

static XEN g_mark_name(XEN mark_n) 
{
  #define H_mark_name "(" S_mark_name " :optional id snd chn): mark's name"
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(mark_n), mark_n, XEN_ONLY_ARG, S_mark_name, "an integer");
  return(mark_get(mark_n, MARK_NAME, XEN_UNDEFINED, S_mark_name));
}

static XEN g_set_mark_name(XEN mark_n, XEN name) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_P(mark_n), mark_n, XEN_ARG_1, S_setB S_mark_name, "an integer");
  XEN_ASSERT_TYPE(XEN_STRING_P(name) || XEN_FALSE_P(name), name, XEN_ARG_2, S_setB S_mark_name, "a string");
  return(mark_set(mark_n, name, MARK_NAME, S_setB S_mark_name));
}

static XEN g_mark_sync_max(void) 
{
  #define H_mark_sync_max "(" S_mark_sync_max "): max mark sync value seen so far"
  return(C_TO_XEN_INT(mark_sync_max()));
}

static XEN g_mark_home(XEN mark_n)
{
  #define H_mark_home "(" S_mark_home " :optional id): the sound (index) and channel that hold mark id"
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(mark_n), mark_n, XEN_ONLY_ARG, S_mark_home, "an integer");
  return(mark_get(mark_n, MARK_HOME, XEN_UNDEFINED, S_mark_home));
}

static XEN g_find_mark(XEN samp_n, XEN snd_n, XEN chn_n, XEN edpos) 
{
  #define H_find_mark "(" S_find_mark " samp-or-name :optional snd chn edpos): \
find the mark in snd's channel chn at samp (if a number) or with the given name (if a string); return the mark id or " PROC_FALSE " if no mark found."

  mark **mps;
  int pos;
  chan_info *cp = NULL;
  XEN_ASSERT_TYPE((XEN_NUMBER_P(samp_n) || XEN_STRING_P(samp_n) || (XEN_FALSE_P(samp_n))), samp_n, XEN_ARG_1, S_find_mark, "a number or string or " PROC_FALSE);
  ASSERT_CHANNEL(S_find_mark, snd_n, chn_n, 2); 
  cp = get_cp(snd_n, chn_n, S_find_mark);
  if (!cp) return(XEN_FALSE);
  if (cp->marks == NULL) 
    return(XEN_FALSE);
  pos = to_c_edit_position(cp, edpos, S_find_mark, 4);
  mps = cp->marks[pos];
  if (mps)
    {
      int i;
      off_t samp = 0;
      char *name = NULL;
      if (XEN_STRING_P(samp_n))
	name = XEN_TO_C_STRING(samp_n);
      else samp = XEN_TO_C_OFF_T_OR_ELSE(samp_n, 0);
      if (name)
	{
	  for (i = 0; i <= cp->mark_ctr[pos]; i++) 
	    if ((mps[i]) && 
		(mps[i]->name) && 
		(strcmp(name, mps[i]->name) == 0))
	      return(C_TO_XEN_INT(mps[i]->id));
	}
      else
	{
	  for (i = 0; i <= cp->mark_ctr[pos]; i++)
	    if ((mps[i]) && 
		(mps[i]->samp == samp)) 
	      return(C_TO_XEN_INT(mps[i]->id));
	}
    }
  return(XEN_FALSE);
}

static XEN g_add_mark(XEN samp_n, XEN snd_n, XEN chn_n, XEN name, XEN sync) 
{
  #define H_add_mark "(" S_add_mark " samp :optional snd chn name (sync 0)): add a mark at sample samp returning the mark id."
  mark *m = NULL;
  chan_info *cp;
  off_t loc;
  int msync = 0;
  char *mname = NULL;

  XEN_ASSERT_TYPE(XEN_OFF_T_P(samp_n) || XEN_NOT_BOUND_P(samp_n), samp_n, XEN_ARG_1, S_add_mark, "an integer");
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(name) || XEN_FALSE_P(name), name, XEN_ARG_4, S_add_mark, "a string");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(sync), sync, XEN_ARG_5, S_add_mark, "an integer");
  ASSERT_CHANNEL(S_add_mark, snd_n, chn_n, 2);

  cp = get_cp(snd_n, chn_n, S_add_mark);
  if (!cp) return(XEN_FALSE);

  loc = XEN_TO_C_OFF_T_OR_ELSE(samp_n, 0);
  if ((loc < 0) || (loc >= CURRENT_SAMPLES(cp)))
    XEN_ERROR(NO_SUCH_SAMPLE,
	      XEN_LIST_2(C_TO_XEN_STRING(S_add_mark),
			 samp_n));

  if (XEN_STRING_P(name)) mname = XEN_TO_C_STRING(name);
  if (XEN_INTEGER_P(sync)) msync = XEN_TO_C_INT(sync);

  m = add_mark(loc, mname, cp);
  if (m)
    {
      if (msync != 0) set_mark_sync(m, msync);
      update_graph(cp);
      return(C_TO_XEN_INT(m->id));
    }
  return(XEN_FALSE);
}

static XEN g_delete_mark(XEN id_n) 
{
  #define H_delete_mark "(" S_delete_mark " id): delete mark id"
  chan_info *cp[1];
  mark *m;
  int id;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(id_n), id_n, XEN_ONLY_ARG, S_delete_mark, "an integer");
  id = XEN_TO_C_INT(id_n);
  m = find_mark_from_id(id, cp, AT_CURRENT_EDIT_POSITION);
  if (m == NULL) 
    return(snd_no_such_mark_error(S_delete_mark, id_n));
  if (delete_mark_id(id, cp[0]))
    update_graph(cp[0]);
  else return(snd_no_such_mark_error(S_delete_mark, id_n));
  return(id_n);
}

static XEN g_delete_marks(XEN snd_n, XEN chn_n) 
{
  #define H_delete_marks "(" S_delete_marks " :optional snd chn): delete all marks in snd's channel chn"
  chan_info *cp;
  ASSERT_CHANNEL(S_delete_marks, snd_n, chn_n, 1);
  cp = get_cp(snd_n, chn_n, S_delete_marks);
  if (!cp) return(XEN_FALSE);
  delete_marks(cp);
  return(XEN_FALSE);
}

static XEN int_array_to_list(int *arr, int i, int len)
{
  if (i < len)
    return(XEN_CONS(C_TO_XEN_INT(arr[i]), 
		    int_array_to_list(arr, i + 1, len)));
  else return(XEN_CONS(C_TO_XEN_INT(arr[i]), 
		       XEN_EMPTY_LIST));
}

static int *syncd_marks(int sync)
{
  syncdata *sd;
  int *ids;
  int i;
  sd = make_syncdata(sync);
  for_each_normal_chan_with_void(gather_chan_syncd_marks, (void *)sd);
  ids = (int *)CALLOC(1 + sd->mark_ctr, sizeof(int));
  ids[0] = sd->mark_ctr;
  for (i = 0; i < sd->mark_ctr; i++) ids[i + 1] = sd->marks[i]->id;
  free_syncdata(sd);
  return(ids);
}

static XEN g_syncd_marks(XEN sync)
{
  #define H_syncd_marks "(" S_syncd_marks " sync): list of mark ids that share a given sync value (" S_mark_sync ")"
  int *ids;
  XEN res;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(sync), sync, XEN_ONLY_ARG, S_syncd_marks, "an integer");
  ids = syncd_marks(XEN_TO_C_INT(sync));
  if (ids == NULL) return(XEN_EMPTY_LIST);
  if (ids[0] == 0) {FREE(ids); return(XEN_EMPTY_LIST);}
  res = int_array_to_list(ids, 1, ids[0]);
  FREE(ids);
  return(res);
}

static XEN g_mark_tag_width(void) {return(C_TO_XEN_INT(mark_tag_width(ss)));}
static XEN g_set_mark_tag_width(XEN val) 
{
  #define H_mark_tag_width "(" S_mark_tag_width "): width (pixels) of mark tags (10)"
  int width;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_mark_tag_width, "an integer"); 
  width = mus_iclamp(0, XEN_TO_C_INT(val), LOTSA_PIXELS);
  set_mark_tag_width(width);
  for_each_normal_chan(update_graph);
  return(C_TO_XEN_INT(mark_tag_width(ss)));
}

static XEN g_mark_tag_height(void) {return(C_TO_XEN_INT(mark_tag_height(ss)));}
static XEN g_set_mark_tag_height(XEN val) 
{
  #define H_mark_tag_height "(" S_mark_tag_height "): height (pixels) of mark tags (4)"
  int height;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_mark_tag_height, "an integer"); 
  height = mus_iclamp(0, XEN_TO_C_INT(val), LOTSA_PIXELS);
  set_mark_tag_height(height);
  for_each_normal_chan(update_graph);
  return(C_TO_XEN_INT(mark_tag_height(ss)));
}

static int *channel_marks(chan_info *cp, int pos)
{
  int *ids = NULL;
  if (cp->marks)
    {
      mark **mps;
      int marks;
      mps = cp->marks[pos];
      marks = cp->mark_ctr[pos];
      if (mps)
	{
	  int i;
	  ids = (int *)CALLOC(marks + 2, sizeof(int)); /* 1 for size, 1 because mark_ctr is current count */
	  ids[0] = marks + 1;
	  for (i = 0; i <= marks; i++) 
	    ids[i + 1] = mps[i]->id;
	}
    }
  return(ids);
}

static XEN g_marks(XEN snd_n, XEN chn_n, XEN pos_n) 
{
  #define H_marks "(" S_marks " :optional snd chn edpos): list of marks (ids) in snd/chn at edit history position pos. \
mark list is: channel given: (id id ...), snd given: ((id id) (id id ...)), neither given: (((id ...) ...) ...)."
  chan_info *cp;
  snd_info *sp;
  XEN res1 = XEN_EMPTY_LIST;
  if (XEN_INTEGER_P(snd_n))
      {
	int i, pos;
	int *ids;
	XEN res;
	if (XEN_INTEGER_P(chn_n))
	  {
	    cp = get_cp(snd_n, chn_n, S_marks);
	    if (!cp) return(XEN_FALSE);
	    if (XEN_INTEGER_P(pos_n)) 
	      {
		pos = XEN_TO_C_INT(pos_n);
		if (pos == AT_CURRENT_EDIT_POSITION)
		  pos = cp->edit_ctr;
		if ((pos < 0) || (pos >= cp->edit_size) || (cp->edits[pos] == NULL))
		  XEN_ERROR(NO_SUCH_EDIT,
			    XEN_LIST_2(C_TO_XEN_STRING(S_marks),
				       pos_n));
	      }
	    else pos = cp->edit_ctr;
	    ids = channel_marks(cp, pos);
	    if (ids == NULL) return(XEN_EMPTY_LIST);
	    if (ids[0] == 0) 
	      {
		FREE(ids); 
		return(XEN_EMPTY_LIST);
	      }
	    res = int_array_to_list(ids, 1, ids[0]);
	    FREE(ids);
	    return(res);
	  }
	else
	  {
	    sp = get_sp(snd_n, NO_PLAYERS);
	    if (sp == NULL) 
	      return(snd_no_such_sound_error(S_marks, snd_n));
	    for (i = sp->nchans - 1; i >= 0; i--)
	      {
		cp = sp->chans[i];
		ids = channel_marks(cp, cp->edit_ctr);
		if ((ids == NULL) || (ids[0] == 0))
		  res1 = XEN_CONS(XEN_EMPTY_LIST, res1);
		else res1 = XEN_CONS(int_array_to_list(ids, 1, ids[0]), 
				     res1);
		if (ids) FREE(ids);
	      }
	  }
      }
  else
    {
      int j;
      /* all marks */
      for (j = ss->max_sounds - 1; j >= 0; j--)
	{
	  sp = ss->sounds[j];
	  if ((sp) && (sp->inuse == SOUND_NORMAL))
	    res1 = XEN_CONS(g_marks(C_TO_XEN_INT(j), XEN_UNDEFINED, XEN_UNDEFINED), 
			    res1);
	}
    }
  return(res1);
}


/* ---------------- saved marks ----------------
 *
 * upon restoration, pre-existing marks can collide both in mark-id and mark-sync
 *   with the saved marks, so these need to be fixed-up as they are encountered.
 *   Also mark-sync-max needs to reflect the newly chosen sync values.
 *
 * We used to try to save the entire mark history, but I can't see how that can work
 *   in general -- the save-marks output can be loaded at any time, so we can't
 *   rely on anything in regard to the edit history.
 */

static bool find_any_marks(chan_info *cp)
{
  return((cp->marks) && (cp->mark_ctr[cp->edit_ctr] >= 0)); /* initialized to -1 -- 0 is first mark */
}

typedef struct {
  FILE *fd;
  int size;
  int *syncs;
} save_mark_info;

#define SYNC_BASE "_sync_"
#define SYNC_NAME_SIZE 32

static char *mark_sync_name(int cur_sync)
{
  char *result;
  result = (char *)CALLOC(SYNC_NAME_SIZE, sizeof(char));
  mus_snprintf(result, SYNC_NAME_SIZE, "%s%d", SYNC_BASE, cur_sync);
  return(result);
}

static char *map_mark_sync(chan_info *cp, mark *m, save_mark_info *sv)
{
  /* if sync 0 just return "0",  else if already in syncs array, use _sync_n_ as name,
   *   else open a let, declare _sync_n_ with new safe value,
   *   add current int value to syncs array (can't assume unshared here will not collide later)
   */

  int i, cur_sync;
  cur_sync = m->sync;
  if (cur_sync == 0)
    return(copy_string("0"));

  if (sv->size > 0)
    for (i = 0; i < sv->size; i++)
      if (cur_sync == sv->syncs[i])
	return(mark_sync_name(cur_sync));

  /* add sync to current set (protect against later collisions, take current shared-syncs into account) */
  if (sv->size == 0)
    sv->syncs = (int *)CALLOC(1, sizeof(int));
  else sv->syncs = (int *)REALLOC(sv->syncs, (sv->size + 1) * sizeof(int));
  sv->syncs[sv->size++] = cur_sync;

#if (!HAVE_FORTH)
  fprintf(sv->fd, "      ");
  for (i = 0; i < sv->size - 1; i++) fprintf(sv->fd, "  "); /* previous lets */
#endif

#if HAVE_SCHEME
  fprintf(sv->fd, "(let ((%s%d (1+ (mark-sync-max))))\n", SYNC_BASE, cur_sync);
#endif

#if HAVE_RUBY
  fprintf(sv->fd, "begin\n        %s%d = mark_sync_max + 1\n", SYNC_BASE, cur_sync);
#endif

#if HAVE_FORTH
  fprintf(sv->fd, "mark-sync-max 1 + value %s%d\n", SYNC_BASE, cur_sync);
#endif

  return(mark_sync_name(cur_sync));
}

static mark *save_mark(chan_info *cp, mark *m, void *info)
{
  /* we're called within "sound_block" where "sfile" = current sound index */

  save_mark_info *sv = (save_mark_info *)info;
  char *mapped_sync;
  int i;

  mapped_sync = map_mark_sync(cp, m, sv);

  fprintf(sv->fd, "      ");
  for (i = 0; i < sv->size; i++) fprintf(sv->fd, "  "); /* lets */

#if HAVE_SCHEME
  if (m->name)
    fprintf(sv->fd, "(add-mark " OFF_TD " sfile %d \"%s\" %s)\n", m->samp, cp->chan, m->name, mapped_sync);
  else fprintf(sv->fd, "(add-mark " OFF_TD " sfile %d #f %s)\n", m->samp, cp->chan, mapped_sync);
#endif

#if HAVE_RUBY
  if (m->name)
    fprintf(sv->fd, "add_mark(" OFF_TD ", sfile, %d, \"%s\", %s)\n", m->samp, cp->chan, m->name, mapped_sync);
  else fprintf(sv->fd, "add_mark(" OFF_TD ", sfile, %d, false, %s)\n", m->samp, cp->chan, mapped_sync);
#endif

#if HAVE_FORTH
  if (m->name)
    fprintf(sv->fd, OFF_TD " sfile %d \"%s\" %s add-mark drop\n", m->samp, cp->chan, m->name, mapped_sync);
  else fprintf(sv->fd, OFF_TD " sfile %d #f %s add-mark drop\n", m->samp, cp->chan, mapped_sync);
#endif

  FREE(mapped_sync);
  return(NULL); /* returning a mark here breaks out of the map mark loop */
}

void save_mark_list(FILE *fd, chan_info *cp, bool all_chans)
{
  /* used in save-marks (below) and the edit history stuff in snd-edits.c
   *   changed 23-Sep-06 -- restore-marks is now a no-op, and no attempt is made to save the entire mark history.
   */

  save_mark_info *sv;
  if ((!all_chans) && (!(find_any_marks(cp)))) return; /* in the sound (all_chans) case, this has been checked already */
  sv = (save_mark_info *)CALLOC(1, sizeof(save_mark_info));
  sv->fd = fd;
  sv->size = 0;
  sv->syncs = NULL;
  if (all_chans)
    {
      int i;
      snd_info *sp;
      sp = cp->sound;
      for (i = 0; i < sp->nchans; i++)
	map_over_marks(sp->chans[i], save_mark, (void *)sv, READ_FORWARD);
    }
  else map_over_marks(cp, save_mark, (void *)sv, READ_FORWARD);
  if (sv->size > 0)
    {
#if HAVE_SCHEME || HAVE_RUBY
      int i;
#endif
      fprintf(fd, "      ");

#if HAVE_SCHEME
      for (i = 0; i < sv->size; i++) fprintf(fd, ")");
#endif

#if HAVE_RUBY
      for (i = 0; i < sv->size; i++) fprintf(fd, "end\n");
#endif

      fprintf(fd, "\n");
    }
  if (sv->syncs) FREE(sv->syncs);
  FREE(sv);
}  

static XEN g_save_marks(XEN snd_n, XEN filename)
{
  #define H_save_marks "(" S_save_marks " :optional snd (filename \"<snd-file-name>.marks\")): save snd's marks in filename. \
The saved file is " XEN_LANGUAGE_NAME " code, so to restore the marks, load that file."
  snd_info *sp;
  XEN res = XEN_FALSE;
  ASSERT_SOUND(S_save_marks, snd_n, 1);
  XEN_ASSERT_TYPE(XEN_STRING_IF_BOUND_P(filename), filename, XEN_ARG_2, S_save_marks, "a string");  
  sp = get_sp(snd_n, NO_PLAYERS);
  if (sp == NULL) 
    return(snd_no_such_sound_error(S_save_marks, snd_n));
  if (map_over_sound_chans(sp, find_any_marks)) /* are there any marks? */
    {
      char *newname = NULL;
      int i, len;
      FILE *fd;
      if (XEN_STRING_P(filename))
	newname = copy_string(XEN_TO_C_STRING(filename));
      else
	{
	  len = strlen(sp->filename);
	  newname = (char *)CALLOC(len + 7, sizeof(char));
#if MUS_DEBUGGING
	  set_printable(PRINT_CHAR);
#endif
	  strcpy(newname, sp->filename);
	  for (i = len - 1; i > 0; i--) 
	    if (newname[i] == '.') 
	      break;
	  if (i > 0) len = i;
	  newname[len] = '\0';
	  strcat(newname, ".marks");
	}
      fd = FOPEN(newname, "w");
      if (fd == NULL)
	{
	  XEN lname;
	  lname = C_TO_XEN_STRING(newname);
	  FREE(newname);
	  XEN_ERROR(CANNOT_SAVE,
		    XEN_LIST_3(C_TO_XEN_STRING(S_save_marks),
			       C_TO_XEN_STRING("open ~A: ~A"),
			       XEN_LIST_2(lname,
					  C_TO_XEN_STRING(snd_open_strerror()))));
	}
      else
	{
#if HAVE_FORTH
	  fprintf(fd, "#f value sfile\n");
#endif
	  open_save_sound_block(sp, fd, false);   /* find-sound or open it with enclosing let */
	  save_mark_list(fd, sp->chans[0], true); /* true -> save all chans, matching cross-channel syncs */
	  close_save_sound_block(fd, false);      /* close the let form */
	  snd_fclose(fd, newname);
	  res = C_TO_XEN_STRING(newname);
	}
      FREE(newname);
    }
  return(res);
}

static XEN g_restore_marks(XEN size, XEN snd, XEN chn, XEN marklist)
{
  /* this exists so that old saved-state files will still load.  It is obviously now a no-op */
  return(XEN_FALSE);
}


#ifdef XEN_ARGIFY_1
XEN_ARGIFY_2(g_mark_sample_w, g_mark_sample)
XEN_NARGIFY_2(g_set_mark_sample_w, g_set_mark_sample)
XEN_ARGIFY_1(g_mark_sync_w, g_mark_sync)
XEN_NARGIFY_2(g_set_mark_sync_w, g_set_mark_sync)
XEN_ARGIFY_1(g_mark_name_w, g_mark_name)
XEN_NARGIFY_2(g_set_mark_name_w, g_set_mark_name)
XEN_NARGIFY_4(g_restore_marks_w, g_restore_marks)
XEN_NARGIFY_0(g_mark_sync_max_w, g_mark_sync_max)
XEN_ARGIFY_1(g_mark_home_w, g_mark_home)
XEN_ARGIFY_3(g_marks_w, g_marks)
XEN_ARGIFY_5(g_add_mark_w, g_add_mark)
XEN_NARGIFY_1(g_delete_mark_w, g_delete_mark)
XEN_ARGIFY_2(g_delete_marks_w, g_delete_marks)
XEN_NARGIFY_1(g_syncd_marks_w, g_syncd_marks)
XEN_NARGIFY_0(g_mark_tag_width_w, g_mark_tag_width)
XEN_NARGIFY_1(g_set_mark_tag_width_w, g_set_mark_tag_width)
XEN_NARGIFY_0(g_mark_tag_height_w, g_mark_tag_height)
XEN_NARGIFY_1(g_set_mark_tag_height_w, g_set_mark_tag_height)
XEN_ARGIFY_4(g_find_mark_w, g_find_mark)
XEN_ARGIFY_2(g_save_marks_w, g_save_marks)
XEN_NARGIFY_1(g_mark_p_w, g_mark_p)
#if MUS_DEBUGGING && HAVE_SCHEME
  XEN_NARGIFY_3(g_test_control_drag_mark_w, g_test_control_drag_mark)
#endif
#else
#define g_mark_sample_w g_mark_sample
#define g_set_mark_sample_w g_set_mark_sample
#define g_mark_sync_w g_mark_sync
#define g_set_mark_sync_w g_set_mark_sync
#define g_mark_name_w g_mark_name
#define g_set_mark_name_w g_set_mark_name
#define g_restore_marks_w g_restore_marks
#define g_mark_sync_max_w g_mark_sync_max
#define g_mark_home_w g_mark_home
#define g_marks_w g_marks
#define g_add_mark_w g_add_mark
#define g_delete_mark_w g_delete_mark
#define g_delete_marks_w g_delete_marks
#define g_syncd_marks_w g_syncd_marks
#define g_mark_tag_width_w g_mark_tag_width
#define g_set_mark_tag_width_w g_set_mark_tag_width
#define g_mark_tag_height_w g_mark_tag_height
#define g_set_mark_tag_height_w g_set_mark_tag_height
#define g_find_mark_w g_find_mark
#define g_save_marks_w g_save_marks
#define g_mark_p_w g_mark_p
#if MUS_DEBUGGING && HAVE_SCHEME
  #define g_test_control_drag_mark_w g_test_control_drag_mark
#endif
#endif

void g_init_marks(void)
{
  #define H_mark_drag_hook S_mark_drag_hook " (id): called when a mark is dragged"
  #define H_mark_hook S_mark_hook " (id snd chn reason): called when a mark added, deleted, or moved. \
'Reason' can be 0: add, 1: delete, 2: move, 3: delete all marks"

  mark_drag_hook = XEN_DEFINE_HOOK(S_mark_drag_hook, 1, H_mark_drag_hook); /* arg = id */
  mark_hook = XEN_DEFINE_HOOK(S_mark_hook, 4, H_mark_hook);                /* args = id snd chn reason */

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mark_sample, g_mark_sample_w, H_mark_sample,
				   S_setB S_mark_sample, g_set_mark_sample_w, 0, 2, 2, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mark_sync, g_mark_sync_w, H_mark_sync,
				   S_setB S_mark_sync, g_set_mark_sync_w, 0, 1, 2, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mark_name, g_mark_name_w, H_mark_name,
				   S_setB S_mark_name, g_set_mark_name_w, 0, 1, 2, 0);

  XEN_DEFINE_PROCEDURE(S_mark_sync_max, g_mark_sync_max_w, 0, 0, 0, H_mark_sync_max);
  XEN_DEFINE_PROCEDURE(S_mark_home,     g_mark_home_w,     0, 1, 0, H_mark_home);
  XEN_DEFINE_PROCEDURE(S_marks,         g_marks_w,         0, 3, 0, H_marks);
  XEN_DEFINE_PROCEDURE(S_add_mark,      g_add_mark_w,      0, 5, 0, H_add_mark);
  XEN_DEFINE_PROCEDURE(S_delete_mark,   g_delete_mark_w,   1, 0, 0, H_delete_mark);
  XEN_DEFINE_PROCEDURE(S_delete_marks,  g_delete_marks_w,  0, 2, 0, H_delete_marks);
  XEN_DEFINE_PROCEDURE(S_syncd_marks,   g_syncd_marks_w,   1, 0, 0, H_syncd_marks);
  XEN_DEFINE_PROCEDURE(S_find_mark,     g_find_mark_w,     1, 3, 0, H_find_mark);
  XEN_DEFINE_PROCEDURE(S_save_marks,    g_save_marks_w,    0, 2, 0, H_save_marks);
  XEN_DEFINE_PROCEDURE(S_mark_p,        g_mark_p_w,        1, 0, 0, H_mark_p);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mark_tag_width, g_mark_tag_width_w, H_mark_tag_width,
				   S_setB S_mark_tag_width, g_set_mark_tag_width_w, 0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mark_tag_height, g_mark_tag_height_w, H_mark_tag_height,
				   S_setB S_mark_tag_height, g_set_mark_tag_height_w, 0, 0, 1, 0);

  #define H_draw_mark_hook S_draw_mark_hook " (mark-id): called before a mark is drawn (in XOR mode). \
If the hook returns " PROC_TRUE ", the mark is not drawn."

  draw_mark_hook = XEN_DEFINE_HOOK(S_draw_mark_hook, 1, H_draw_mark_hook);  /* arg = mark-id */

#if MUS_DEBUGGING && HAVE_SCHEME
  XEN_DEFINE_PROCEDURE("internal-test-control-drag-mark", g_test_control_drag_mark_w, 3, 0, 0, "internal testing func");
#endif

  /* obsolete */
  XEN_DEFINE_PROCEDURE("restore-marks", g_restore_marks_w, 4, 0, 0, "no-op for backwards compatibility");
}


