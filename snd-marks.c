/* TODO  drag via mark could still use amp-env opts
 */

/* if mark is handled as an smob, rather than a bare integer, we get better
 *   type-checking, but (for example) searching for a given mark becomes messy
 */

#include "snd.h"

/* to handle undo/redo cleanly, we keep the mark list as an array (indexed to edit_ctr)
 * of arrays of pointers to marks.  Any mark-related operation follows cp->edit_ctr's
 * array within the outer array.  This way, marks come and go in an intuitively clean
 * manner without endless list operations in this file.  The downside is that the
 * mark associated with a given id is actually a list of marks, and which one is
 * "current" can change at any time.
 */

/* added sync field and related operations 16-June-00 */

typedef mark *mark_map_func(chan_info *cp, mark *mp, void *m);

static XEN mark_drag_hook;
static XEN mark_hook; /* add, delete, move */

#define MARK_ID_MASK   0x0fffffff
#define MARK_VISIBLE   0x10000000

static mark *map_over_marks(chan_info *cp, mark_map_func *func, void *m, int direction)
{
  int i, marks, pos;
  mark *mp;
  mark **mps;
  if (cp->marks)
    {
      pos = cp->edit_ctr;
      mps = cp->marks[pos];
      marks = cp->mark_ctr[pos];
      if (mps)
	{
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

static mark *make_mark_1(int samp, char *name, int id, unsigned int sc)
{
  mark *mp;
  mp = (mark *)CALLOC(1, sizeof(mark));
  if (name) mp->name = copy_string(name); else mp->name = NULL;
  mp->samp = samp;
  mp->id = id;
  mp->sync = sc;
  return(mp);
}

static int mark_id_counter = 0;

static mark *make_mark(int samp, char *name) {return(make_mark_1(samp, name, mark_id_counter++, 0));}

static mark *copy_mark(mark *m) {return(make_mark_1(m->samp, m->name, mark_id(m), m->sync));}

int mark_id(mark *m) {return(m->id & MARK_ID_MASK);}

static int mark_sync(mark *m) {return(m->sync);}

static int sync_max = 0;

int mark_sync_max(void) {return(sync_max);}

int set_mark_sync(mark *m, int val) 
{
  m->sync = (unsigned int)val; 
  if (val > sync_max) 
    sync_max = val; 
  return(val);
}

static mark *free_mark (mark *mp)
{
  if (mp)
    {
      if (mp->name) FREE(mp->name);
      FREE(mp);
    }
  return(NULL);
}

static mark *find_mark_id_1(chan_info *cp, mark *mp, void *uid)
{
  if (mark_id(mp) == (*((int *)uid))) return(mp); else return(NULL);
}

static mark *find_mark_from_id(snd_state *ss, int id, chan_info **cps, int pos)
{
  int i, j, old_pos;
  snd_info *sp;
  chan_info *cp;
  mark *mp;
  for (i = 0; i < ss->max_sounds; i++)
    if ((sp = ((snd_info *)(ss->sounds[i]))) &&
	(sp->inuse))
      for (j = 0; j<(sp->nchans); j++)
	if ((cp = ((chan_info *)(sp->chans[j]))))
	  {
	    old_pos = cp->edit_ctr;
	    if (pos >= 0) cp->edit_ctr = pos;
	    /* memoization would have to be done here where we know cp->edit_ctr */
	    mp = map_over_marks(cp, find_mark_id_1, (void *)(&id), READ_FORWARD);
	    cp->edit_ctr = old_pos;
	    if (mp) 
	      {
		cps[0] = cp; 
		return(mp);
	      }
	  }
  return(NULL);
}

static mark *find_mark_id(chan_info **cps, int id, int pos) 
{
  return(find_mark_from_id(get_global_state(), id, cps, pos));
}


static mark *find_named_mark_1(chan_info *cp, mark *mp, void *uname)
{
  char *name = (char *)uname;
  if ((mp->name) && (name) && (strcmp(mp->name, name) == 0)) return(mp);
  else return(NULL);
}

static mark *find_named_mark(chan_info *cp, char *name)
{
  return(map_over_marks(cp, find_named_mark_1, (void *)name, READ_FORWARD));
}

static mark *find_previous_mark_1(chan_info *cp, mark *mp, void *m)
{
  if (mp->samp < (*((int *)m))) return(mp); else return(NULL);
}

static mark *find_previous_mark (int current_sample, chan_info *cp)
{
  return(map_over_marks(cp, find_previous_mark_1, (void *)(&current_sample), READ_BACKWARD));
}

static mark *find_next_mark_1(chan_info *cp, mark *mp, void *m)
{
  if (mp->samp > (*((int *)m))) return(mp); else return(NULL);
}

static mark *find_next_mark (int current_sample, chan_info *cp)
{
  return(map_over_marks(cp, find_next_mark_1, (void *)(&current_sample), READ_FORWARD));
}

static mark* marks_off_1(chan_info *cp, mark *mp, void *m)
{
  mp->id &= (~MARK_VISIBLE);
  return(NULL);
}

void marks_off(chan_info *cp)
{
  map_over_marks(cp, marks_off_1, NULL, READ_FORWARD);
}


#define PLAY_ARROW_SIZE 10
#define MARK_TAB_WIDTH 10
#define MARK_TAB_HEIGHT 4

static XEN draw_mark_hook;

static void draw_mark_1(chan_info *cp, axis_info *ap, mark *mp, int show)
{
  /* fields are samp and name */
  int len, top, cx, y0, y1;
  axis_context *ax;
  XEN res = XEN_FALSE;
  ax = mark_context(cp);
  if (XEN_HOOKED(draw_mark_hook))
    {
      res = g_c_run_progn_hook(draw_mark_hook,
			       XEN_LIST_1(C_TO_SMALL_XEN_INT(mark_id(mp))),
			       S_draw_mark_hook);
      if (XEN_TRUE_P(res))
	{
	  if (show) mp->id |= MARK_VISIBLE; else mp->id &= (~MARK_VISIBLE);
	  return;
	}
    }
  top = ap->y_axis_y1;
  y1 = top;
  y0 = ap->y_axis_y0;
  if (mp->name) top += 10;
  cx = grf_x((double)(mp->samp) / (double)SND_SRATE(cp->sound), ap);
  if (mp->name)
    {
      activate_button_font(ax, cp->state);
      len = mark_name_width(cp->state, mp->name);
      draw_string(ax, (int)(cx - 0.5 * len), y1 + 6, mp->name, strlen(mp->name));
    }
  fill_rectangle(ax,
		 cx - MARK_TAB_WIDTH, top,
		 2 * MARK_TAB_WIDTH, MARK_TAB_HEIGHT);
  draw_line(ax, cx, top + 4, cx, y0);
  fill_polygon(ax, 4,
	       cx,                   y0,
	       cx + PLAY_ARROW_SIZE, y0 +     PLAY_ARROW_SIZE,
	       cx,                   y0 + 2 * PLAY_ARROW_SIZE,
	       cx,                   y0);

  if (show) mp->id |= MARK_VISIBLE; else mp->id &= (~MARK_VISIBLE);
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

void draw_mark(chan_info *cp, axis_info *ap, mark *mp)
{
  if (!(mp->id & MARK_VISIBLE)) draw_mark_1(cp, ap, mp, 1);
}

static void erase_mark(chan_info *cp, axis_info *ap, mark *mp)
{
  if (mp->id & MARK_VISIBLE) draw_mark_1(cp, ap, mp, 0);
}


typedef struct {
  int x, y;
} mdata;

static mark *hit_mark_1(chan_info *cp, mark *mp, void *m)
{
  /* we're going left to right, so after passing x, give up */
  int mx;
  mdata *md = (mdata *)m;
  axis_info *ap;
  mx = grf_x((double)(mp->samp) / (double)SND_SRATE(cp->sound), cp->axis);
  if (mx > (md->x + MARK_TAB_WIDTH)) return(NULL); /* past it */
  if (mx < (md->x - MARK_TAB_WIDTH)) return(NULL); /* before it */
  if (mp->name == NULL)                          /* check y if unnamed */
    {
      ap = cp->axis;
      if ((md->y >= ap->y_axis_y1) && 
	  (md->y <= (ap->y_axis_y1 + MARK_TAB_HEIGHT))) 
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
  mx = grf_x((double)(mp->samp) / (double)SND_SRATE(cp->sound), cp->axis);
  if (mx > md->x) return(NULL);
  if ((mx + PLAY_ARROW_SIZE) < md->x) return(NULL);
  ap = cp->axis;
  y = md->y - ap->y_axis_y0 - PLAY_ARROW_SIZE;
  if (y < 0) y = -y;
  if ((mx + PLAY_ARROW_SIZE - y) >= md->x) return(mp);
  /* the last is assuming the triangle shape for hit detection */
  return(NULL);
}

mark *hit_triangle(chan_info *cp, int x, int y)
{
  mark *mp;
  mdata *md;
  axis_info *ap;
  ap = cp->axis;
  if (cp->marks)
    {
      /* first check that we're in the bottom portion of the graph where the mark triangles are */
      if ((y >= ap->y_axis_y0) && 
	  (y <= (ap->y_axis_y0 + 2 * PLAY_ARROW_SIZE)))
	{
	  md = (mdata *)CALLOC(2, sizeof(mdata));
	  md->x = x;
	  md->y = y;
	  mp = map_over_marks(cp, hit_triangle_1, (void *)md, READ_FORWARD);
	  FREE(md);
	  return(mp);
	}
    }
  return(NULL);
}


static int watching_mouse = 0; /* this is tracking axis moves */
static int last_mouse_x = 0;
static mark *moving_mark = NULL; /* used only while "off-screen" during axis moves */

static void move_axis_to_track_mark(chan_info *cp);
static BACKGROUND_FUNCTION_TYPE watch_mouse_button = 0;
static BACKGROUND_TYPE WatchMouse(GUI_POINTER cp)
{
  if (watch_mouse_button)
    {
      move_axis_to_track_mark((chan_info *)cp);
      return(BACKGROUND_CONTINUE);
    }
  else return(BACKGROUND_QUIT);
}

static void start_mark_watching(chan_info *cp, mark *mp)
{
  snd_state *ss;
  moving_mark = mp;
  ss = cp->state;
  watch_mouse_button = BACKGROUND_ADD(ss, WatchMouse, cp);
  watching_mouse = 1;
}

static void cancel_mark_watch(chan_info *cp)
{
  if (watch_mouse_button) BACKGROUND_REMOVE(watch_mouse_button);
  watch_mouse_button = 0;
  watching_mouse = 0;
  moving_mark = NULL;
}

static int move_mark_1(chan_info *cp, mark *mp, int x)
{
  axis_info *ap;
  int nx, samps;
  int redraw;
  ap = cp->axis;
  redraw = (!watching_mouse);
  if ((x > ap->x_axis_x1) || (x < ap->x_axis_x0)) 
    {
      if (watching_mouse)
	{
	  if (((x < ap->x_axis_x0) && (ap->x0 == ap->xmin)) ||
	      ((x > ap->x_axis_x1) && (ap->x1 == ap->xmax)))
	    return(0);
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
	  redraw = 0;
	}
    }
  mp->samp = (int)(ungrf_x(ap, nx) * SND_SRATE(cp->sound));
  if (mp->samp < 0) mp->samp = 0;
  samps = current_ed_samples(cp);
  if (mp->samp > samps) mp->samp = samps;
  if (XEN_HOOKED(mark_drag_hook))
    g_c_run_progn_hook(mark_drag_hook,
		       XEN_LIST_1(C_TO_SMALL_XEN_INT(mark_id(mp))),
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

int move_play_mark(chan_info *cp, int *mc, Locus cx)
{
  /* mc = mouse loc sampwise return samps updating mc */
  int cur_mc;
  axis_info *ap;
  ap = cp->axis;
  if (prev_cx > 0) draw_play_triangle(cp, prev_cx);
  prev_cx = cx;
  draw_play_triangle(cp, cx);
  cur_mc = (*mc);
  (*mc) = (int)(ungrf_x(ap, cx) * SND_SRATE(cp->sound));
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

enum {MARK_ADD, MARK_DELETE, MARK_MOVE, MARKS_DELETE, MARK_RELEASE};
static void run_mark_hook(chan_info *cp, int id, int reason)
{
  /* called after the mark list has been made consistent */
  if (XEN_HOOKED(mark_hook))
    g_c_run_progn_hook(mark_hook,
		       XEN_LIST_4(C_TO_SMALL_XEN_INT(id),
				  C_TO_SMALL_XEN_INT(cp->sound->index),
				  C_TO_SMALL_XEN_INT(cp->chan),
				  C_TO_SMALL_XEN_INT(reason)),
		       S_mark_hook);
}

static int ignore_redundant_marks = 0;

static void allocate_marks(chan_info *cp, int edit_ctr)
{
  int i;
  cp->marks_size = edit_ctr + 16;
  cp->marks = (mark ***)CALLOC(cp->marks_size, sizeof(mark **));
  cp->mark_size = (int *)CALLOC(cp->marks_size, sizeof(int));
  cp->mark_ctr = (int *)CALLOC(cp->marks_size, sizeof(int));
  for (i = 0; i < cp->marks_size; i++) cp->mark_ctr[i] = -1;
}

mark *add_mark(int samp, char *name, chan_info *cp)
{
  int i, j, ed, med;
  mark **mps;
  mark *mp;
  if (!(cp->marks)) allocate_marks(cp, cp->edit_ctr);
  /* our current mark list is cp->edit_ctr (it starts at 0 -- see snd-chn.c and snd-edits.c) */
  ed = cp->edit_ctr;
  cp->mark_ctr[ed]++;
  if (cp->mark_ctr[ed] >= cp->mark_size[ed])
    {
      cp->mark_size[ed] += 16;
      if (!cp->marks[ed]) 
	cp->marks[ed] = (mark **)CALLOC(cp->mark_size[ed], sizeof(mark *));
      else 
	{
	  cp->marks[ed] = (mark **)REALLOC(cp->marks[ed], cp->mark_size[ed] * sizeof(mark *));
	  for (i = cp->mark_size[ed] - 16; i < cp->mark_size[ed]; i++) cp->marks[ed][i] = NULL;
	}
    }
  mps = cp->marks[ed];
  med = cp->mark_ctr[ed];
  if (med == 0)
    {
      if (mps[0]) free_mark(mps[0]);
      mps[0] = make_mark(samp, name);
      run_mark_hook(cp, mark_id(mps[0]), MARK_ADD);
      return(mps[0]);
    }
  else
    {
      for (i = 0; i < med; i++) /* not <= because we pre-incremented above */
	{
	  mp = mps[i];
	  if ((ignore_redundant_marks) && (samp == mp->samp))
	    {
	      cp->mark_ctr[ed]--;
	      return(NULL);
	    }
	  if (samp < mp->samp)
	    {
	      if (mps[med]) free_mark(mps[med]);
	      for (j = med; j > i; j--)
		mps[j] = mps[j - 1];
	      mps[i] = make_mark(samp, name);
	      run_mark_hook(cp, mark_id(mps[i]), MARK_ADD);
	      return(mps[i]);
	    }
	}
      /* insert at end */
      if (mps[med]) free_mark(mps[med]);
      mps[med] = make_mark(samp, name);
      run_mark_hook(cp, mark_id(mps[med]), MARK_ADD);
      return(mps[med]);
    }
}

void delete_mark_samp(int samp, chan_info *cp)
{
  int i, j, ed, edm, id = -1;
  mark *mp;
  mark **mps;
  axis_info *ap;
  if ((cp) && (cp->marks))
    {
      ed = cp->edit_ctr;
      mps = cp->marks[ed];
      if (mps)
	{
	  edm = cp->mark_ctr[ed];
	  for (i = 0; i <= edm; i++)
	    {
	      mp = mps[i];
	      if (mp->samp == samp)
		{
		  ap = cp->axis;
		  if ((mp->samp >= ap->losamp) && (mp->samp <= ap->hisamp)) erase_mark(cp, ap, mp); 
		  id = mark_id(mp);
		  free_mark(mp);
		  mps[i] = NULL;
		  if (i < edm)
		    {
		      for (j = i; j < edm; j++) mps[j] = mps[j + 1];
		      mps[edm] = NULL;
		    }
		  cp->mark_ctr[ed]--;
		  run_mark_hook(cp, id, MARK_DELETE);
		  return;
		}
	    }
	}
    }
  report_in_minibuffer(cp->sound, "no mark at sample %d", samp);
}

static void delete_mark_id(int id, chan_info *cp)
{
  int i, j, ed, edm;
  mark *mp;
  mark **mps;
  axis_info *ap;
  if ((cp) && (cp->marks))
    {
      ed = cp->edit_ctr;
      mps = cp->marks[ed];
      if (mps)
	{
	  edm = cp->mark_ctr[ed];
	  for (i = 0; i <= edm; i++)
	    {
	      mp = mps[i];
	      if (mark_id(mp) == id)
		{
		  ap = cp->axis;
		  if ((mp->samp >= ap->losamp) && (mp->samp <= ap->hisamp)) erase_mark(cp, ap, mp); 
		  free_mark(mp);
		  mps[i] = NULL;
		  if (i < edm)
		    {
		      for (j = i; j < edm; j++) mps[j] = mps[j + 1];
		      mps[edm] = NULL;
		    }
		  cp->mark_ctr[ed]--;
		  run_mark_hook(cp, id, MARK_DELETE);
		  return;
		}
	    }
	}
    }
  report_in_minibuffer(cp->sound, "no mark with id: %d", id);
}

static void delete_marks(chan_info *cp)
{
  int i, ed;
  mark *mp;
  mark **mps;
  axis_info *ap;
  if ((cp) && (cp->marks))
    {
      ed = cp->edit_ctr;
      mps = cp->marks[ed];
      if (mps)
	{
	  for (i = 0; i < cp->mark_size[ed]; i++)
	    {
	      mp = mps[i];
	      if (mp) 
		{
		  ap = cp->axis;
		  if ((mp->samp >= ap->losamp) && (mp->samp <= ap->hisamp)) erase_mark(cp, ap, mp); 
		  free_mark(mp);
		  mps[i] = NULL;
		}
	    }
	  cp->mark_ctr[ed] = -1;
	  run_mark_hook(cp, -1, MARKS_DELETE);
	  /* if (!(ss->graph_hook_active)) update_graph(cp, NULL); */
	}
    }
}

void free_mark_list(chan_info *cp, int ignore)
{
  int i, j;
  mark **mps;
  mark *mp;
  if (cp->marks)
    {
      for (i = 0; i < cp->marks_size; i++) 
	{
	  if (i != ignore)
	    {
	      mps = cp->marks[i];
	      if (mps)
		{
		  for (j = 0; j < cp->mark_size[i]; j++)
		    {
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
      FREE(cp->mark_ctr);
      cp->mark_ctr = NULL;
      FREE(cp->mark_size);
      cp->mark_size = NULL;
    }
}

void collapse_marks (snd_info *sp)
{
  /* in all channels, move current edit_ctr mark list to 0, freeing all the rest */
  int i, ed, len, size;
  chan_info *cp;
  mark **mps;
  for (i = 0; i < sp->nchans; i++)
    {
      cp = sp->chans[i];
      if ((cp) && (cp->marks))
	{
	  ed = cp->edit_ctr;
	  mps = cp->marks[ed];
	  /* this is the one to save */
	  if (mps)
	    {
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

static mark *find_nth_mark(chan_info *cp, int count)
{
  int i, c, samp;
  mark *mp = NULL;
  if ((!cp) || (!cp->marks)) return(NULL);
  if (count > 0) c = count; else c = -count;
  samp = cp->cursor;
  for (i = 0; i < c; i++)
    {
      if (count > 0) mp = find_next_mark(samp, cp);
      else mp = find_previous_mark(samp, cp);
      if (!mp) break;
      samp = mp->samp;
    }
  return(mp);
}

int goto_mark(chan_info *cp, int count)
{
  mark *mp;
  if ((!cp) || (!cp->marks))
    {
      if (cp) report_in_minibuffer(cp->sound, "no marks");
      return(CURSOR_IN_VIEW);
    }
  mp = find_nth_mark(cp, count);
  if (!mp) 
    {
      report_in_minibuffer(cp->sound, "no such mark");
      return(CURSOR_IN_VIEW);
    }
  return(cursor_moveto(cp, mp->samp));
}

int goto_named_mark(chan_info *cp, char *name)
{
  mark *mp;
  mp = find_named_mark(cp, name);
  if (mp) return(cursor_moveto(cp, mp->samp));
  return(CURSOR_IN_VIEW);
}

static mark *active_mark_1(chan_info *cp, mark *mp, void *m)
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

int mark_beg(chan_info *cp)
{
  mark *mp;
  mp = active_mark(cp);
  if (mp) return(mp->samp);
  return(-1);
}

static mark *display_channel_marks_1(chan_info *cp,  mark *mp, void *m)
{
  axis_info *ap;
  int *last_samp = (int *)m;
  ap = cp->axis;
  if (mp->samp > ap->hisamp) return(mp); /* terminates loop */
  if (mp->samp == last_samp[0])
    return(NULL); 
  else last_samp[0] = mp->samp;          /* avoid drawing twice at same point == erase */
  if ((mp->samp >= ap->losamp) && 
      (mp->samp <= ap->hisamp) && 
      (mp != moving_mark)) 
    draw_mark(cp, ap, mp);
  return(NULL);
}

void display_channel_marks(chan_info *cp)
{
  int last_samp[1];
  last_samp[0] = -1;
  map_over_marks(cp, display_channel_marks_1, (void *)last_samp, READ_FORWARD);
}

void release_pending_marks(chan_info *cp, int edit_ctr)
{
  /* free the mark list at edit_ctr */
  mark **mps;
  mark *mp;
  int j;
  if ((cp) && (cp->marks))
    {
      mps = cp->marks[edit_ctr];
      if (mps)
	{
	  for (j = 0; j < cp->mark_size[edit_ctr]; j++) /* was <= cp->mark_ctr[edit_ctr] */
	    {
	      mp = mps[j];
	      if (mp) free_mark(mp);
	      mps[j] = NULL;
	    }
	  cp->mark_ctr[edit_ctr] = -1;
	}
    }
}

void ripple_marks(chan_info *cp, int beg, int change)
{
  /* if change = 0, just set ptr, else copy and fixup with deletions */
  /* this is called after the tree has been pushed forward, so edit_ctr is ahead of us */
  /* but we don't do anything if no marks */
  int old_m, new_m, end, i, old_size; 
  mark **mps, **mpo;
  mark *mp;
  if ((cp) && (cp->marks))
    {
      if (cp->edit_ctr == 0) return;
      old_m = cp->edit_ctr - 1;
      new_m = cp->edit_ctr;
      if (new_m >= cp->marks_size) /* groan -- we have to realloc the base array of array of pointers! */
	{
	  old_size = cp->marks_size;
	  cp->marks_size += 16;
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
      cp->mark_ctr[new_m] = cp->mark_ctr[old_m];
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
      cp->mark_size[new_m] = cp->mark_size[old_m];
      if (cp->mark_size[new_m] > 0)
	{
	  cp->marks[new_m] = (mark **)CALLOC(cp->mark_size[new_m], sizeof(mark *));
	  if (cp->mark_ctr[new_m] >= 0)
	    {
	      mps = cp->marks[new_m];
	      mpo = cp->marks[old_m];
	      for (i = 0; i <= cp->mark_ctr[new_m]; i++)
		mps[i] = copy_mark(mpo[i]);
	      if (change < 0)
		{
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

void mark_define_region(chan_info *cp, int count)
{
  int beg, end, i;
  mark *mp;
  sync_info *si;
  int ends[1];
  if (cp)
    {
      if (cp->marks)
	{
	  beg = cp->cursor;
	  mp = find_nth_mark(cp, count);
	  if (mp)
	    {
	      end = mp->samp;
	      if (end != beg)
		{
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
		      update_graph(si->cps[i], NULL);
		    }
		  si = free_sync_info(si);
		}
	    }
	  else report_in_minibuffer(cp->sound, "no such mark");
	}
      else report_in_minibuffer(cp->sound, "no marks");
    }
}

void save_mark_list(FILE *fd, chan_info *cp)
{
  /* assumes we're calling from the edit history list maker in snd-edits.c */
  /* as with edit-tree, graft these lists onto the end of the current mark list */
  /*   if none, pad out to current cp->edit_ctr? */
  /*   since the edits ripple the marks, we'll have to assume we're called later and hope */
  int i, j, marks;
  mark **mps;
  mark *m;
  if (cp->marks)
    {
      fprintf(fd, "      (%s %d sfile %d '(", S_restore_marks, cp->marks_size, cp->chan);
      for (i = 0; i < cp->marks_size; i++)
	{
	  fprintf(fd, "\n        (%d %d (", cp->mark_size[i], cp->mark_ctr[i]);
	  mps = cp->marks[i];
	  if (mps)
	    {
	      marks = cp->mark_ctr[i];
	      for (j = 0; j <= marks; j++)
		{
		  m = mps[j];
		  if (m)
		    {
		      if (m->name)
			fprintf(fd, "(\"%s\" %d %d %d) ", m->name, m->samp, mark_id(m), mark_sync(m));
		      else fprintf(fd, "(#f %d %d %d) ", m->samp, mark_id(m), mark_sync(m));
		    }
		  else fprintf(fd, "(#f #f #f #f) ");
		}
	    }
	  fprintf(fd, "))");
	}
      fprintf(fd, "))\n");
    }
}

static mark *reverse_mark_1(chan_info *cp, mark *mp, void *um)
{
  mark *m = (mark *)um;
  mp->samp = m->samp - mp->samp;
  return(NULL);
}

void reverse_marks(chan_info *cp, int beg, int dur) /* beg -1 for full sound */
{
  mark *m;
  mark **mps;
  int ed, end, marks, i;
  ed = cp->edit_ctr;
  mps = cp->marks[ed];
  if (beg == -1)
    {
      m = make_mark_1(current_ed_samples(cp) - 1, NULL, 0, 0);
      map_over_marks(cp, reverse_mark_1, (void *)m, READ_FORWARD);
      free_mark(m);
    }
  else
    {
      end = beg + dur - 1;
      marks = cp->mark_ctr[ed];
      for (i = 0; i <= marks; i++) 
	{
	  m = mps[i];
	  if ((m->samp >= beg) && (m->samp <= end))
	    m->samp = end - (m->samp - beg);
	}
    }
  if ((mps) && (cp->mark_ctr[ed] >= 0))
    qsort((void *)mps, cp->mark_ctr[ed] + 1, sizeof(mark *), compare_mark_samps);
}

void src_marks(chan_info *cp, Float ratio, int old_samps, int new_samps, int beg, int over_selection)
{
  int i, marks, pos, end;
  mark *m;
  mark **mps;
  if (cp->marks)
    {		
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
		    m->samp = (int)(m->samp / ratio);
		  else m->samp = (int)((old_samps - 1 - m->samp) / (-ratio)); /* ratio < 0 here */
		}
	    }
	  else
	    {
	      end = beg + old_samps - 1;
	      for (i = 0; i <= marks; i++) 
		{
		  m = mps[i];
		  if ((m->samp >= beg) && (m->samp <= end))
		    {
		      if (ratio > 0.0)
			m->samp = beg + (int)((m->samp - beg) / ratio);
		      else m->samp = beg + (int)((old_samps - 1 - (m->samp - beg)) / (-ratio));
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

void reset_marks(chan_info *cp, int num, int *samps, int end, int extension, int over_selection)
{
  int i, marks, pos;
  mark *m;
  mark **mps;
  if (cp->marks)
    {		
      pos = cp->edit_ctr;
      mps = cp->marks[pos];
      marks = cp->mark_ctr[pos];
      if ((mps) && (marks >= 0))
	{
	  if (over_selection)
	    for (i = 0; i <= marks; i++) 
	      {
		m = mps[i];
		if (m->samp > end) m->samp += extension;
	      }
	  for (i = 0; (i <= marks) && (i < num); i++) 
	    {
	      m = mps[i];
	      if (samps[i] >= 0) m->samp = samps[i];
	    }
	  qsort((void *)mps, marks + 1, sizeof(mark *), compare_mark_samps);
	}
    }
}

void ripple_trailing_marks(chan_info *cp, int beg, int old_len, int new_len)
{
  int i, marks, pos;
  mark *m;
  mark **mps;
  if (cp->marks)
    {		
      ripple_marks(cp, 0, 0);
      pos = cp->edit_ctr;
      mps = cp->marks[pos];
      marks = cp->mark_ctr[pos];
      if ((mps) && (marks >= 0))
	{
	  for (i = 0; i <= marks; i++) 
	    {
	      m = mps[i];
	      if (m->samp > (beg + old_len)) m->samp += (new_len - old_len);
	    }
	}
    }
}

void swap_marks(chan_info *cp0, chan_info *cp1)
{
  mark **mps0 = NULL, **mps1 = NULL;
  int ctr0 = 0, ctr1 = 0;
  int size0 = 0, size1 = 0;
  int pos0, pos1;
  if ((cp0->marks) || (cp1->marks))
    {
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
  int *initial_samples;
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
  sd->initial_samples = (int *)CALLOC(sd->marks_size, sizeof(int));
  return(sd);
}

static void add_syncd_mark(syncdata *sd, mark *mp, chan_info *cp)
{
  int i;
  sd->marks[sd->mark_ctr] = mp;
  sd->initial_samples[sd->mark_ctr] = mp->samp;
  sd->chans[sd->mark_ctr++] = cp;
  if (sd->mark_ctr == sd->marks_size)
    {
      sd->marks = (mark **)REALLOC(sd->marks, sd->marks_size * 2 * sizeof(mark *));
      sd->chans = (chan_info **)REALLOC(sd->chans, sd->marks_size * 2 * sizeof(chan_info *));
      for (i = sd->marks_size; i < sd->marks_size * 2; i++) {sd->marks[i] = NULL; sd->chans[i] = NULL;}
      sd->marks_size *= 2;
    }
}

static mark *gather_local_syncd_marks(chan_info *cp, mark *mp, void *usd)
{
  syncdata *sd = (syncdata *)usd;
  if ((unsigned int)(sd->sync) == mp->sync)
    add_syncd_mark(sd, mp, cp);
  return(NULL);
}

static int gather_chan_syncd_marks(chan_info *cp, void *sd)
{
  map_over_marks(cp, gather_local_syncd_marks, sd, READ_FORWARD);
  return(0);
}

static syncdata *gather_syncd_marks(snd_state *ss, int sync)
{
  syncdata *sd;
  sd = make_syncdata(sync);
  map_over_chans(ss, gather_chan_syncd_marks, (void *)sd);
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

static int mark_control_clicked = 0; /* C-click of mark -> drag data as mark is dragged */
static int mark_initial_sample = 0;
static syncdata *mark_sd = NULL;
static mix_context **mark_movers = NULL;

static void initialize_md_context(int size, chan_info **cps)
{
  int i;
  mix_context *ms;
  mark_movers = (mix_context **)CALLOC(size, sizeof(mix_context *));
  for (i = 0; i < size; i++)
    {
      mark_movers[i] = make_mix_context(cps[i]);
      ms = mark_movers[i];
      ms->lastpj = make_graph(cps[i], cps[i]->sound, cps[i]->state); 
      mix_save_graph(cps[i]->state, ms, ms->lastpj);
    }
}

static void finalize_md_context(int size)
{
  int i;
  if (mark_movers)
    {
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
  mdata *md;
  axis_info *ap;
  ap = cp->axis;
  if (cp->marks)
    {
      /* first check that we're in the top portion of the graph where the mark tabs are */
      if ((y >= ap->y_axis_y1) && 
	  (y <= (ap->y_axis_y1 + MARK_TAB_HEIGHT + 10)))               /*  + 10 for named marks -- checked again later */
	{
	  md = (mdata *)CALLOC(1, sizeof(mdata));
	  md->x = x;
	  md->y = y;
	  mp = map_over_marks(cp, hit_mark_1, (void *)md, READ_FORWARD);
	  FREE(md);
	  if (mp)
	    {
	      mark_control_clicked = (key_state & snd_ControlMask);
	      if (mp->sync != 0) mark_sd = gather_syncd_marks(cp->state, mp->sync);
	      if (mark_control_clicked)
		{
		  mark_initial_sample = mp->samp;
		  if (mark_sd) 
		    initialize_md_context(mark_sd->mark_ctr, mark_sd->chans);
		  else initialize_md_context(1, &cp);
		}
	    }
	}
    }
  return(mp);
}

static void make_mark_graph(chan_info *cp, snd_info *sp, int initial_sample, int current_sample, int which);

static int move_syncd_mark(chan_info *cp, mark *m, int x)
{
  int old_samp, diff, i, samps, redraw;
  mark *mp;
  axis_info *ap;
  chan_info *ncp;
  old_samp = m->samp;
  redraw = move_mark_1(cp, m, x);
  diff = m->samp - old_samp;
  if (diff != 0)
    {
      if ((mark_sd) && (mark_sd->mark_ctr > 1))
	{
	  for (i = 0; i < mark_sd->mark_ctr; i++)
	    {
	      mp = mark_sd->marks[i];
	      if (mp != m)
		{
		  ncp = mark_sd->chans[i];
		  ap = ncp->axis;
		  if ((mp->samp >= ap->losamp) && 
		      (mp->samp <= ap->hisamp)) 
		    erase_mark(ncp, ap, mp);
		  mp->samp += diff;
		  if (mp->samp < 0) mp->samp = 0;
		  samps = current_ed_samples(ncp);
		  if (mp->samp > samps) mp->samp = samps;
		  if (mark_control_clicked)
		    make_mark_graph(ncp, ncp->sound, mark_sd->initial_samples[i], mp->samp, i);
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
  int redraw;
  if (moving_mark)
    {
      if (moving_mark->sync)
	redraw = move_syncd_mark(cp, moving_mark, last_mouse_x);
      else redraw = move_mark_1(cp, moving_mark, last_mouse_x);
      if (redraw) draw_mark(cp, cp->axis, moving_mark);
    }
}

void move_mark(chan_info *cp, mark *mp, int x) /* from mouse drag callback in snd-chn.c, called whenever mark is visible */
{
  int redraw;
  last_mouse_x = x;
  if (mp->sync)
    redraw = move_syncd_mark(cp, mp, x);
  else redraw = move_mark_1(cp, mp, x);
  if (mark_control_clicked)
    make_mark_graph(cp, cp->sound, mark_initial_sample, mp->samp, 0);
  if (redraw) draw_mark(cp, cp->axis, mp);
}

static void edit_dragged_mark(chan_info *cp, mark *m, int initial_sample)
{
  /* edit -- initial_sample is where we were when the drag started, ended at m->samp */
  int num, mark_final_sample, id;
  mark *new_m;
  mark_final_sample = m->samp;
  num = mark_final_sample - initial_sample;
  m->samp = initial_sample;
  id = mark_id(m);
  if (num > 0)
    extend_with_zeros(cp, initial_sample, num, "mark dragged", cp->edit_ctr);
      /* at this point, old mark pointer is irrelevant (it lives in the previous edit history list) */
      /*   but since the ripple didn't touch it, we need to move it forward to reflect the insertion */
  else 
    if (num < 0)
      {
	new_m = map_over_marks(cp, find_mark_id_1, (void *)(&id), READ_FORWARD);
	new_m->samp = initial_sample;
	delete_samples(mark_final_sample, -num, cp, "mark dragged", cp->edit_ctr);
      }
  if (num != 0) 
    {
      new_m = map_over_marks(cp, find_mark_id_1, (void *)(&id), READ_FORWARD);
      new_m->samp = mark_final_sample;
      update_graph(cp, NULL);
    }
}

void finish_moving_mark(chan_info *cp, mark *m) /* button release called from snd-chn.c */
{
  int i, j;
  mark *sdm;
  if (watching_mouse) cancel_mark_watch(cp);
  if ((m->sync != 0) && (mark_sd))
    {
      if (XEN_HOOKED(mark_hook))
	for (i = 0; i < mark_sd->mark_ctr; i++)
	  run_mark_hook(mark_sd->chans[i], mark_id(mark_sd->marks[i]), MARK_RELEASE);
      if (mark_control_clicked)
	{
	  for (i = mark_sd->mark_ctr - 1; i >= 0; i--)
	    {
	      /* do the edits in reverse order on the assumption that marks sharing a channel were ordered to begin with,
	       *   so they'll happen in reverse order here, so the lower sample edits in rippling won't affect the higher
	       */
	      sdm = mark_sd->marks[i];
	      edit_dragged_mark(mark_sd->chans[i], sdm, mark_sd->initial_samples[i]);
	    }
	  finalize_md_context(mark_sd->mark_ctr);
	}
      for (i = 0; i < mark_sd->mark_ctr; i++)
	if (mark_sd->chans[i])
	  {
	    sort_marks(mark_sd->chans[i]); /* resort marks in case movement scrambled them */
	    for (j = i + 1; j < mark_sd->mark_ctr; j++)    /* only sort each channel once */
	      if (mark_sd->chans[j] == mark_sd->chans[i])
		mark_sd->chans[j] = NULL;
	  }
    }
  else 
    {
      run_mark_hook(cp, mark_id(m), MARK_RELEASE);
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
  sd = gather_syncd_marks(cp->state, m->sync);
  if ((sd) && (sd->mark_ctr > 0))
    play_channels(sd->chans, sd->mark_ctr, sd->initial_samples, NULL, IN_BACKGROUND, 
		  C_TO_XEN_INT(AT_CURRENT_EDIT_POSITION), 
		  "drag and play sync'd marks", 0, FALSE);
  if (sd) free_syncdata(sd);
}

static void move_to_next_sample(snd_fd *sf)
{
  if (sf->view_buffered_data > sf->last)
    next_sound(sf);
  else sf->view_buffered_data++;
}

static void make_mark_graph(chan_info *cp, snd_info *sp, int initial_sample, int current_sample, int which)
{
  int i, j = 0, samps, k;
  Locus xi;
  axis_info *ap;
  Float samples_per_pixel, xf, samp;
  double x, incr;  
  Float ymin, ymax;
  int pixels;
  snd_fd *sf = NULL;
  int x_start, x_end;
  double start_time = 0.0, cur_srate = 1.0;
  ap = cp->axis;
  cur_srate = (double)SND_SRATE(sp);
  ap->losamp = (int)(ap->x0 * cur_srate);
  if (ap->losamp < 0) ap->losamp = 0;
  if (ap->x0 != (ap->losamp / cur_srate)) ap->losamp++;
  start_time = (double)(ap->losamp) / cur_srate;
  ap->hisamp = (int)(ap->x1 * cur_srate);
  if ((ap->losamp == 0) && (ap->hisamp == 0)) return;
  x_start = ap->x_axis_x0;
  x_end = ap->x_axis_x1;
  samps = ap->hisamp - ap->losamp + 1;
  if ((x_start == x_end) && (samps > 10)) return; /* must be too-tiny graph */
  pixels = x_end - x_start;
  if (pixels >= POINT_BUFFER_SIZE) pixels = POINT_BUFFER_SIZE - 1;
  if ((x_start == x_end) || (samps <= 1))
    samples_per_pixel = 0.01; /* any non-zero value < 1.0 should be ok here */
  else samples_per_pixel = (Float)(samps - 1) / (Float)pixels;

 /* this is assuming one such mark per channel */
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
		  move_to_next_sample(sf);
	      set_grf_point(grf_x(x, ap), j, grf_y(next_sample_to_float(sf), ap));
	    }
	}
      else
	{
	  for (j = 0, i = ap->losamp, x = start_time; i <= ap->hisamp; i++, j++, x += incr)
	    {
	      if ((i < initial_sample) || (i >= current_sample)) 
		samp = next_sample_to_float(sf);
	      else samp = 0.0;
	      set_grf_point(grf_x(x, ap), j, grf_y(samp, ap));
	    }
	}
      erase_and_draw_grf_points(mark_movers[which], cp, j);
    }
  else
    {
      sf = init_sample_read(ap->losamp, cp, READ_FORWARD);
      if (sf == NULL) return;
      j = 0;      /* graph point counter */
      x = ap->x0;
      xi = grf_x(x, ap);
      xf = 0.0;     /* samples per pixel counter */
      ymin = 100.0;
      ymax = -100.0;
      if (current_sample < initial_sample) 
	{
	  for (i = ap->losamp, xf = 0.0; i <= ap->hisamp; i++)
	    {
	      if (i == current_sample) 
		for (k = current_sample; k < initial_sample; k++) 
		  move_to_next_sample(sf);
	      samp = next_sample_to_float(sf);
	      if (samp > ymax) ymax = samp;
	      if (samp < ymin) ymin = samp;
	      xf += 1.0;
	      if (xf > samples_per_pixel)
		{
		  set_grf_points(xi, j, grf_y(ymin, ap), grf_y(ymax, ap));
		  xi++;
		  j++;
		  xf -= samples_per_pixel;
		  ymin = 100.0;
		  ymax = -100.0;
		}
	    }
	}
      else
	{
	  for (i = ap->losamp, xf = 0.0; i <= ap->hisamp; i++)
	    {
	      if ((i < initial_sample) || (i >= current_sample))
		samp = next_sample_to_float(sf);
	      else samp = 0.0;
	      if (samp > ymax) ymax = samp;
	      if (samp < ymin) ymin = samp;
	      xf += 1.0;
	      if (xf > samples_per_pixel)
		{
		  set_grf_points(xi, j, grf_y(ymin, ap), grf_y(ymax, ap));
		  xi++;
		  j++;
		  xf -= samples_per_pixel;
		  ymin = 100.0;
		  ymax = -100.0;
		}
	    }
	}
      erase_and_draw_both_grf_points(mark_movers[which], cp, j);
    }
  if (sf) {free_snd_fd(sf); sf = NULL;}
}


static XEN snd_no_such_mark_error(const char *caller, XEN id)
{
  XEN_ERROR(NO_SUCH_MARK,
	    XEN_LIST_2(C_TO_XEN_STRING(caller),
		       id));
  return(XEN_FALSE);
}

static XEN g_restore_marks(XEN size, XEN snd, XEN chn, XEN marklist)
{
  XEN lst; XEN el; XEN nm; XEN sm; XEN mlst; XEN olst; XEN molst;
  chan_info *cp;
  snd_info *sp;
  char *str;
  int i, j, list_size, in_size, id, sync;
  ASSERT_CHANNEL(S_restore_marks, snd, chn, 2);
  sp = get_sp(snd);
  if (sp == NULL) 
    return(snd_no_such_sound_error(S_restore_marks, snd));
  cp = get_cp(snd, chn, S_restore_marks);
  if ((cp) && (!(cp->marks)))
    {
      cp->marks_size = XEN_TO_C_INT(size);
      cp->marks = (mark ***)CALLOC(cp->marks_size, sizeof(mark **));
      cp->mark_size = (int *)CALLOC(cp->marks_size, sizeof(int));
      cp->mark_ctr = (int *)CALLOC(cp->marks_size, sizeof(int));
      for (i = 0; i < cp->marks_size; i++) cp->mark_ctr[i] = -1;
      list_size = XEN_LIST_LENGTH(marklist);
      for (i = 0, olst = marklist; i < list_size; i++, olst = XEN_CDR(olst))
	{
	  lst = XEN_CAR(olst);
	  cp->mark_size[i] = XEN_TO_C_INT(XEN_CAR(lst));
	  cp->mark_ctr[i] = XEN_TO_C_INT(XEN_CADR(lst));
          if (cp->mark_size[i] > 0)
	    {
	      mlst = XEN_CADDR(lst);
	      cp->marks[i] = (mark **)CALLOC(cp->mark_size[i], sizeof(mark *));
	      in_size = XEN_LIST_LENGTH(mlst);
	      for (j = 0, molst = mlst; j < in_size; j++, molst = XEN_CDR(molst))
		{
		  el = XEN_CAR(molst);
		  if (!(XEN_LIST_P(el))) 
		    snd_error("%s[%d] %s: saved mark data is not a list?? ",
			      __FILE__, __LINE__, __FUNCTION__);
		  else
		    {
		      sm = XEN_CADR(el);
		      if (XEN_NOT_FALSE_P(sm))
			{
			  nm = XEN_CAR(el);
			  if (XEN_NOT_FALSE_P(nm))
			    str = XEN_TO_C_STRING(nm);
			  else str = NULL;
			  id = XEN_TO_C_INT(XEN_CADDR(el));
			  if (XEN_LIST_LENGTH(el) > 3)
			    sync = XEN_TO_C_INT(XEN_CADDDR(el));
			  else sync = 0;
			  cp->marks[i][j] = make_mark_1(XEN_TO_C_INT(sm), str, id, sync);
			  if (id > mark_id_counter) mark_id_counter = id;
			}
		    }
		}
	    }
	}
      return(XEN_TRUE);
    }
  else 
    if (cp->marks) 
      snd_error("%s[%d] %s: there are marks here already!",
		__FILE__, __LINE__, __FUNCTION__);
  return(XEN_FALSE);
}

enum {MARK_SAMPLE, MARK_NAME, MARK_SYNC, MARK_HOME};

static XEN iread_mark(XEN n, int fld, XEN pos_n, char *caller)
{
  int pos;
  chan_info *ncp[1];
  mark *m = NULL;
  pos = XEN_TO_C_INT_OR_ELSE_WITH_CALLER(pos_n, -1, caller);
  m = find_mark_id(ncp, XEN_TO_C_INT_OR_ELSE_WITH_CALLER(n, 0, caller), pos);
  if (m == NULL) 
    return(snd_no_such_mark_error(caller, n));
  switch (fld)
    {
    case MARK_SAMPLE: 
      return(C_TO_XEN_INT(m->samp)); 
      break;
    case MARK_SYNC:   
      return(C_TO_XEN_INT(mark_sync(m))); 
      break;
    case MARK_NAME:   
      if (m->name) 
	return(C_TO_XEN_STRING(m->name)); 
      else return(C_TO_XEN_STRING("")); 
      break;
    case MARK_HOME:   
      return(XEN_LIST_2(C_TO_SMALL_XEN_INT((ncp[0]->sound)->index),
			C_TO_SMALL_XEN_INT(ncp[0]->chan))); 
      break;
    }
  return(XEN_FALSE);
}

static XEN iwrite_mark(XEN mark_n, XEN val, int fld, char *caller)
{
  chan_info *cp[1];
  mark *m;
  m = find_mark_id(cp, XEN_TO_C_INT_OR_ELSE_WITH_CALLER(mark_n, 0, caller), -1);
  if (m == NULL) 
    return(snd_no_such_mark_error(caller, mark_n));
  switch (fld)
    {
    case MARK_SAMPLE: 
      m->samp = mus_iclamp(0, 
			   XEN_TO_C_INT_OR_ELSE_WITH_CALLER(val, 0, caller),
			   current_ed_samples(cp[0]));
      sort_marks(cp[0]); /* update and re-sort current mark list */
      run_mark_hook(cp[0], mark_id(m), MARK_MOVE);
      update_graph(cp[0], NULL);
      break;
    case MARK_SYNC: 
      set_mark_sync(m, XEN_TO_C_INT_OR_ELSE_WITH_CALLER(val, 0, caller));
      break;
    case MARK_NAME:
      if (m->name) FREE(m->name);
      m->name = copy_string(XEN_TO_C_STRING(val));
      update_graph(cp[0], NULL);
      break;
    }
  return(val);
}

static XEN g_mark_p(XEN id_n)
{
  #define H_mark_p "(" S_mark_p " id) -> #t if mark is active"
  chan_info *ncp[1];
  if (XEN_INTEGER_P(id_n))
    return(C_TO_XEN_BOOLEAN(find_mark_id(ncp, XEN_TO_C_INT(id_n), -1)));
  return(XEN_FALSE);
}

static XEN g_mark_sample(XEN mark_n, XEN pos_n) 
{
  #define H_mark_sample "(" S_mark_sample " &optional id pos) returns the mark's location (sample number) at edit history pos"
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(mark_n), mark_n, XEN_ARG_1, S_mark_sample, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(pos_n), pos_n, XEN_ARG_2, S_mark_sample, "an integer");
  return(iread_mark(mark_n, MARK_SAMPLE, pos_n, S_mark_sample));
}

static XEN g_set_mark_sample(XEN mark_n, XEN samp_n) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(mark_n), mark_n, XEN_ARG_1, "set-" S_mark_sample, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(samp_n), samp_n, XEN_ARG_2, "set-" S_mark_sample, "an integer");
  return(iwrite_mark(mark_n, samp_n, MARK_SAMPLE, "set-" S_mark_sample));
}

static XEN g_mark_sync(XEN mark_n) 
{
  #define H_mark_sync "(" S_mark_sync " id) returns the mark's sync value"
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(mark_n), mark_n, XEN_ONLY_ARG, S_mark_sync, "an integer");
  return(iread_mark(mark_n, MARK_SYNC, XEN_UNDEFINED, S_mark_sync));
}

static XEN g_set_mark_sync(XEN mark_n, XEN sync_n) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(mark_n), mark_n, XEN_ARG_1, "set-" S_mark_sync, "an integer");
  XEN_ASSERT_TYPE(XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(sync_n), sync_n, XEN_ARG_2, "set-" S_mark_sync, "an integer");
  return(iwrite_mark(mark_n, sync_n, MARK_SYNC, "set-" S_mark_sync));
}

static XEN g_mark_name(XEN mark_n) 
{
  #define H_mark_name "(" S_mark_name " id &optional snd chn) returns the mark's name"
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(mark_n), mark_n, XEN_ONLY_ARG, S_mark_name, "an integer");
  return(iread_mark(mark_n, MARK_NAME, XEN_UNDEFINED, S_mark_name));
}

static XEN g_set_mark_name(XEN mark_n, XEN name) 
{
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(mark_n), mark_n, XEN_ARG_1, "set-" S_mark_name, "an integer");
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_2, "set-" S_mark_name, "a string");
  return(iwrite_mark(mark_n, name, MARK_NAME, "set-" S_mark_name));
}

static XEN g_mark_sync_max(void) 
{
  #define H_mark_sync_max "(" S_mark_sync_max ") -> max mark sync value seen so far"
  return(C_TO_XEN_INT(mark_sync_max()));
}

static XEN g_mark_home(XEN mark_n)
{
  #define H_mark_home "(" S_mark_home " id) returns the sound (index) and channel that hold mark id"
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(mark_n), mark_n, XEN_ONLY_ARG, S_mark_home, "an integer");
  return(iread_mark(mark_n, MARK_HOME, XEN_UNDEFINED, S_mark_home));
}

static XEN g_find_mark(XEN samp_n, XEN snd_n, XEN chn_n) 
{
  #define H_find_mark "(" S_find_mark " samp-or-name &optional snd chn)\n\
finds the mark in snd's channel chn at samp (if a number) or with the given name (if a string), returning the mark id; returns #f if no mark found."

  mark **mps;
  int i, samp = 0;
  char *name = NULL;
  chan_info *cp = NULL;
  XEN_ASSERT_TYPE((XEN_NUMBER_P(samp_n) || XEN_STRING_P(samp_n) || (XEN_NOT_BOUND_P(samp_n)) || (XEN_FALSE_P(samp_n))), 
		  samp_n, XEN_ARG_1, S_find_mark, "a number or string or #f");
  ASSERT_CHANNEL(S_find_mark, snd_n, chn_n, 2); 
  cp = get_cp(snd_n, chn_n, S_find_mark);
  if (cp->marks == NULL) 
    return(XEN_FALSE);
  mps = cp->marks[cp->edit_ctr];
  if (mps)
    {
      if (XEN_STRING_P(samp_n))
	name = XEN_TO_C_STRING(samp_n);
      else samp = XEN_TO_C_INT_OR_ELSE(samp_n, 0);
      if (name)
	{
	  for (i = 0; i <= cp->mark_ctr[cp->edit_ctr]; i++) 
	    if ((mps[i]) && 
		(mps[i]->name) && 
		(strcmp(name, mps[i]->name) == 0))
	      return(C_TO_XEN_INT(mark_id(mps[i])));
	}
      else
	{
	  for (i = 0; i <= cp->mark_ctr[cp->edit_ctr]; i++)
	    if ((mps[i]) && 
		(mps[i]->samp == samp)) 
	      return(C_TO_XEN_INT(mark_id(mps[i])));
	}
    }
  return(XEN_FALSE);
}

static XEN g_add_mark(XEN samp_n, XEN snd_n, XEN chn_n) 
{
  #define H_add_mark "(" S_add_mark ") samp &optional snd chn) adds a mark at sample samp returning the mark id."
  mark *m;
  chan_info *cp;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(samp_n), samp_n, XEN_ARG_1, S_add_mark, "an integer");
  ASSERT_CHANNEL(S_add_mark, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_add_mark);
  m = add_mark(XEN_TO_C_INT_OR_ELSE(samp_n, 0), NULL, cp);
  if (m)
    {
      /* if it's a redundant mark, and we're ignoring them, return -1 */
      update_graph(cp, NULL);
      return(C_TO_XEN_INT(mark_id(m)));
    }
  else return(C_TO_XEN_INT(-1)); /* ?? XEN_FALSE? or 'redundant-mark? */
}

static XEN g_delete_mark(XEN id_n) 
{
  #define H_delete_mark "(" S_delete_mark " id) deletes mark id"
  chan_info *cp[1];
  mark *m;
  int id;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(id_n), id_n, XEN_ONLY_ARG, S_delete_mark, "an integer");
  m = find_mark_id(cp, id = XEN_TO_C_INT_OR_ELSE(id_n, 0), -1);
  if (m == NULL) 
    return(snd_no_such_mark_error(S_delete_mark, id_n));
  delete_mark_id(id, cp[0]);
  update_graph(cp[0], NULL);
  return(id_n);
}

static XEN g_delete_marks(XEN snd_n, XEN chn_n) 
{
  #define H_delete_marks "(" S_delete_marks " &optional snd chn) deletes all marks in snd's channel chn"
  chan_info *cp;
  ASSERT_CHANNEL(S_delete_marks, snd_n, chn_n, 1);
  cp = get_cp(snd_n, chn_n, S_delete_marks);
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

static int *syncd_marks(snd_state *ss, int sync)
{
  syncdata *sd;
  int *ids;
  int i;
  sd = make_syncdata(sync);
  map_over_chans(ss, gather_chan_syncd_marks, (void *)sd);
  ids = (int *)CALLOC(1 + sd->mark_ctr, sizeof(int));
  ids[0] = sd->mark_ctr;
  for (i = 0; i < sd->mark_ctr; i++) ids[i + 1] = mark_id(sd->marks[i]);
  free_syncdata(sd);
  return(ids);
}

static XEN g_syncd_marks(XEN sync)
{
  #define H_syncd_marks "(" S_syncd_marks " sync) -> list of mark ids that share a given sync value (mark-sync)"
  int *ids;
  XEN res;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(sync), sync, XEN_ONLY_ARG, S_syncd_marks, "an integer");
  ids = syncd_marks(get_global_state(), XEN_TO_C_INT(sync));
  if ((ids == NULL) || (ids[0] == 0)) return(XEN_EMPTY_LIST);
  res = int_array_to_list(ids, 1, ids[0]);
  FREE(ids);
  return(res);
}

static int *channel_marks(chan_info *cp, int pos)
{
  int *ids = NULL;
  int i, marks;
  mark **mps;
  if (cp->marks)
    {
      mps = cp->marks[pos];
      marks = cp->mark_ctr[pos];
      if (mps)
	{
	  ids = (int *)CALLOC(marks + 2, sizeof(int)); /* 1 for size, 1 because mark_ctr is current count */
	  ids[0] = marks + 1;
	  for (i = 0; i <= marks; i++) 
	    ids[i + 1] = mark_id(mps[i]);
	}
    }
  return(ids);
}

static XEN g_marks(XEN snd_n, XEN chn_n, XEN pos_n) 
{
  #define H_marks "(" S_marks " &optional snd chn pos) -> list of marks (ids) in snd/chn at edit history position pos. \
mark list is: channel given: (id id ...), snd given: ((id id) (id id ...)), neither given: (((id ...) ...) ...)."
  chan_info *cp;
  snd_info *sp;
  snd_state *ss;
  XEN res; XEN res1 = XEN_EMPTY_LIST;
  int *ids;
  int i, pos, j;
  if (XEN_INTEGER_P(snd_n))
      {
	if (XEN_INTEGER_P(chn_n))
	  {
	    cp = get_cp(snd_n, chn_n, S_marks);
	    if (XEN_INTEGER_P(pos_n)) 
	      pos = XEN_TO_C_INT(pos_n); 
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
	    sp = get_sp(snd_n);
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
      /* all marks */
      ss = get_global_state();
      for (j = ss->max_sounds - 1; j >= 0; j--)
	{
	  sp = ss->sounds[j];
	  if ((sp) && (sp->inuse))
	    res1 = XEN_CONS(g_marks(C_TO_SMALL_XEN_INT(j), XEN_UNDEFINED, XEN_UNDEFINED), 
			    res1);
	}
    }
  return(res1);
}

static XEN g_forward_mark(XEN count, XEN snd, XEN chn) 
{
  #define H_forward_mark "(" S_forward_mark " &optional (count 1) snd chn) moves the cursor forward by count marks"
  int val; 
  chan_info *cp;
  mark *mp = NULL;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(count), count, XEN_ARG_1, S_forward_mark, "an integer");
  ASSERT_CHANNEL(S_forward_mark, snd, chn, 2);
  cp = get_cp(snd, chn, S_forward_mark);
  val = XEN_TO_C_INT_OR_ELSE(count, 1); 
  if (cp->marks) mp = find_nth_mark(cp, val);
  if (mp)
    {
      cursor_moveto(cp, mp->samp);
      return(C_TO_XEN_INT(mark_id(mp)));
    }
  return(XEN_FALSE);
}

static XEN g_backward_mark(XEN count, XEN snd, XEN chn) 
{
  #define H_backward_mark "(" S_backward_mark " &optional (count 1) snd chn) moves the cursor back by count marks"
  int val; 
  chan_info *cp;
  mark *mp = NULL;
  XEN_ASSERT_TYPE(XEN_INTEGER_IF_BOUND_P(count), count, XEN_ARG_1, S_backward_mark, "an integer");
  ASSERT_CHANNEL(S_backward_mark, snd, chn, 2);
  cp = get_cp(snd, chn, S_backward_mark);
  val = -(XEN_TO_C_INT_OR_ELSE(count, 1)); 
  if (cp->marks) mp = find_nth_mark(cp, val);
  if (mp)
    {
      cursor_moveto(cp, mp->samp);
      return(C_TO_XEN_INT(mark_id(mp)));
    }
  return(XEN_FALSE);
}

static char *mark_file_name(snd_info *sp)
{
  char *newname;
  int len, i;
  len = strlen(sp->filename);
  newname = (char *)CALLOC(len + 7, sizeof(char));
  strcpy(newname, sp->filename);
  for (i = len - 1; i > 0; i--) 
    if (newname[i] == '.') 
      break;
  if (i > 0) len = i;
  newname[len] = '\0';
  strcat(newname, ".marks");
  return(newname);
}

static int find_any_marks (chan_info *cp, void *ptr)
{
  if (cp->marks) 
    return(cp->mark_ctr[cp->edit_ctr] + 1); /* initialized to -1 -- 0 is first mark */
  return(0);
}

static char *save_marks(snd_info *sp)
{
  char *newname = NULL;
  int i;
  FILE *fd;
  if ((sp) && (map_over_sound_chans(sp, find_any_marks, NULL)))
    {
      newname = mark_file_name(sp);
      fd = fopen(newname, "w");
      if (fd)
	{
	  fprintf(fd, "(let ((sfile (find-sound \"%s\")))\n", sp->short_filename);
	  for (i = 0; i < sp->nchans; i++)
	    save_mark_list(fd, sp->chans[i]);
	  fprintf(fd, ")");
	  fclose(fd);
	}
      else 
	report_in_minibuffer_and_save(sp, "%s %s ", newname, strerror(errno));
    }
  return(newname);
}

static XEN g_save_marks(XEN snd_n)
{
  #define H_save_marks "(" S_save_marks " &optional snd) saves snd's marks in <snd's file-name>.marks"
  snd_info *sp;
  char *str;
  XEN res = XEN_FALSE;
  ASSERT_SOUND(S_save_marks, snd_n, 1);
  sp = get_sp(snd_n);
  if (sp == NULL) 
    return(snd_no_such_sound_error(S_save_marks, snd_n));
  str = save_marks(sp);
  if (str)
    {
      res = C_TO_XEN_STRING(str);
      FREE(str);
    }
  return(res);
}

#ifdef XEN_ARGIFY_1
XEN_ARGIFY_2(g_mark_sample_w, g_mark_sample)
XEN_ARGIFY_2(g_set_mark_sample_w, g_set_mark_sample)
XEN_ARGIFY_1(g_mark_sync_w, g_mark_sync)
XEN_ARGIFY_2(g_set_mark_sync_w, g_set_mark_sync)
XEN_ARGIFY_1(g_mark_name_w, g_mark_name)
XEN_ARGIFY_2(g_set_mark_name_w, g_set_mark_name)
XEN_NARGIFY_4(g_restore_marks_w, g_restore_marks)
XEN_NARGIFY_0(g_mark_sync_max_w, g_mark_sync_max)
XEN_ARGIFY_1(g_mark_home_w, g_mark_home)
XEN_ARGIFY_3(g_marks_w, g_marks)
XEN_ARGIFY_3(g_add_mark_w, g_add_mark)
XEN_ARGIFY_1(g_delete_mark_w, g_delete_mark)
XEN_ARGIFY_2(g_delete_marks_w, g_delete_marks)
XEN_NARGIFY_1(g_syncd_marks_w, g_syncd_marks)
XEN_ARGIFY_3(g_find_mark_w, g_find_mark)
XEN_ARGIFY_3(g_forward_mark_w, g_forward_mark)
XEN_ARGIFY_3(g_backward_mark_w, g_backward_mark)
XEN_ARGIFY_1(g_save_marks_w, g_save_marks)
XEN_NARGIFY_1(g_mark_p_w, g_mark_p)
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
#define g_find_mark_w g_find_mark
#define g_forward_mark_w g_forward_mark
#define g_backward_mark_w g_backward_mark
#define g_save_marks_w g_save_marks
#define g_mark_p_w g_mark_p
#endif

void g_init_marks(void)
{
  #define H_mark_drag_hook S_mark_drag_hook " (id) is called when a mark is dragged"
  #define H_mark_hook S_mark_hook " (id snd chn reason) is called when a mark added, deleted, or moved. \
'Reason' can be 0: add, 1: delete, 2: move, 3: delete all marks"

  XEN_DEFINE_HOOK(mark_drag_hook, S_mark_drag_hook, 1, H_mark_drag_hook); /* arg = id */
  XEN_DEFINE_HOOK(mark_hook, S_mark_hook, 4, H_mark_hook);                /* args = id snd chn reason */

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mark_sample, g_mark_sample_w, H_mark_sample,
				   "set-" S_mark_sample, g_set_mark_sample_w, 0, 2, 1, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mark_sync, g_mark_sync_w, H_mark_sync,
				   "set-" S_mark_sync, g_set_mark_sync_w, 0, 1, 1, 1);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_mark_name, g_mark_name_w, H_mark_name,
				   "set-" S_mark_name, g_set_mark_name_w, 0, 1, 1, 1);

  XEN_DEFINE_PROCEDURE(S_restore_marks, g_restore_marks_w, 4, 0, 0, "internal func");
  XEN_DEFINE_PROCEDURE(S_mark_sync_max, g_mark_sync_max_w, 0, 0, 0, H_mark_sync_max);
  XEN_DEFINE_PROCEDURE(S_mark_home,     g_mark_home_w, 0, 1, 0,     H_mark_home);
  XEN_DEFINE_PROCEDURE(S_marks,         g_marks_w, 0, 3, 0,         H_marks);
  XEN_DEFINE_PROCEDURE(S_add_mark,      g_add_mark_w, 0, 3, 0,      H_add_mark);
  XEN_DEFINE_PROCEDURE(S_delete_mark,   g_delete_mark_w, 0, 1, 0,   H_delete_mark);
  XEN_DEFINE_PROCEDURE(S_delete_marks,  g_delete_marks_w, 0, 2, 0,  H_delete_marks);
  XEN_DEFINE_PROCEDURE(S_syncd_marks,   g_syncd_marks_w, 1, 0, 0,   H_syncd_marks);
  XEN_DEFINE_PROCEDURE(S_find_mark,     g_find_mark_w, 1, 2, 0,     H_find_mark);
  XEN_DEFINE_PROCEDURE(S_forward_mark,  g_forward_mark_w, 0, 3, 0,  H_forward_mark);
  XEN_DEFINE_PROCEDURE(S_backward_mark, g_backward_mark_w, 0, 3, 0, H_backward_mark);
  XEN_DEFINE_PROCEDURE(S_save_marks,    g_save_marks_w, 0, 1, 0,    H_save_marks);
  XEN_DEFINE_PROCEDURE(S_mark_p,        g_mark_p_w, 1, 0, 0,        H_mark_p);

  #define H_draw_mark_hook S_draw_mark_hook " (mark-id) is called before a mark is drawn (in XOR mode). \
If the hook returns #t, the mark is not drawn."

  XEN_DEFINE_HOOK(draw_mark_hook, S_draw_mark_hook, 1, H_draw_mark_hook);  /* arg = mark-id */
}


