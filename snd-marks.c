/* TODO  drag via mark could still use amp-env opts
 * TODO  sync colors? could use 6-7 bits of id+table (or local mark context)
 * TODO  mark fixups -- test negative src env mark fixups (probably broken)
 * TODO  control panel apply from mark?
 * TODO  syncd play when syncd triangle dragged?
 * TODO  reverse (or any fixup) if syncd mark, but unsyncd chans -- should other marks move or break sync chain?
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

static SCM mark_drag_hook;

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
		{
		  mp = (*func)(cp, mps[i], m);
		  if (mp) return(mp);
		}
	    }
	  else
	    {
	      for (i = marks; i >= 0; i--) 
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

static void draw_mark_1(chan_info *cp, axis_info *ap, mark *mp, int show)
{
  /* fields are samp and name */
  int len, top, cx, y0, y1;
  axis_context *ax;
  ax = mark_context(cp); /* was cursor_context */
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
  if (HOOKED(mark_drag_hook))
    g_c_run_progn_hook(mark_drag_hook,
		       SCM_LIST1(TO_SMALL_SCM_INT(mark_id(mp))),
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
  sp->srate = 1.0;
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
	      return(mps[i]);
	    }
	}
      /* insert at end */
      if (mps[med]) free_mark(mps[med]);
      mps[med] = make_mark(samp, name);
      return(mps[med]);
    }
}

void delete_mark_samp(int samp, chan_info *cp)
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
	      if (mp->samp == samp)
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
		  return;
		}
	    }
	}
    }
  report_in_minibuffer(cp->sound, "no mark with id: %d", id);
}

static void delete_marks (chan_info *cp)
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
		    {
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

void reverse_marks(chan_info *cp, int over_selection)
{
  mark *m;
  mark **mps;
  int ed, beg, end, marks, i;
  ed = cp->edit_ctr;
  mps = cp->marks[ed];
  if (!over_selection)
    {
      m = make_mark_1(current_ed_samples(cp) - 1, NULL, 0, 0);
      map_over_marks(cp, reverse_mark_1, (void *)m, READ_FORWARD);
      free_mark(m);
    }
  else
    {
      beg = selection_beg(cp);
      end = beg + selection_len() - 1;
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
			{
			  m->samp += (new_samps - old_samps);
			}
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
	    {
	      for (i = 0; i <= marks; i++) 
		{
		  m = mps[i];
		  if (m->samp > end) m->samp += extension;
		}
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
	      if (m->samp > (beg+old_len)) m->samp += (new_len - old_len);
	    }
	}
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
    extend_with_zeros(cp, initial_sample, num, "mark dragged");
      /* at this point, old mark pointer is irrelevant (it lives in the previous edit history list) */
      /*   but since the ripple didn't touch it, we need to move it forward to reflect the insertion */
  else 
    if (num < 0)
      {
	new_m = map_over_marks(cp, find_mark_id_1, (void *)(&id), READ_FORWARD);
	new_m->samp = initial_sample;
	delete_samples(mark_final_sample, -num, cp, "mark dragged");
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
		  TO_SCM_INT(AT_CURRENT_EDIT_POSITION), "drag and play sync'd marks");
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


static SCM snd_no_such_mark_error(const char *caller, SCM id)
{
  ERROR(NO_SUCH_MARK,
	SCM_LIST2(TO_SCM_STRING(caller),
		  id));
  return(SCM_BOOL_F);
}

static SCM g_restore_marks(SCM size, SCM snd, SCM chn, SCM marklist)
{
  SCM lst, el, nm, sm, mlst, olst, molst;
  chan_info *cp;
  snd_info *sp;
  char *str;
  snd_state *ss;
  int i, j, list_size, in_size, id, sync;
  SND_ASSERT_CHAN(S_restore_marks, snd, chn, 2);
  ss = get_global_state();
  sp = get_sp(snd);
  cp = get_cp(snd, chn, S_restore_marks);
  if ((cp) && (!(cp->marks)))
    {
      cp->marks_size = TO_C_INT(size);
      cp->marks = (mark ***)CALLOC(cp->marks_size, sizeof(mark **));
      cp->mark_size = (int *)CALLOC(cp->marks_size, sizeof(int));
      cp->mark_ctr = (int *)CALLOC(cp->marks_size, sizeof(int));
      for (i = 0; i < cp->marks_size; i++) cp->mark_ctr[i] = -1;
      list_size = LIST_LENGTH(marklist);
      for (i = 0, olst = marklist; i < list_size; i++, olst = SCM_CDR(olst))
	{
	  lst = SCM_CAR(olst);
	  cp->mark_size[i] = TO_C_INT(SCM_CAR(lst));
	  cp->mark_ctr[i] = TO_C_INT(SCM_CADR(lst));
          if (cp->mark_size[i] > 0)
	    {
	      mlst = SCM_CADDR(lst);
	      cp->marks[i] = (mark **)CALLOC(cp->mark_size[i], sizeof(mark *));
	      in_size = LIST_LENGTH(mlst);
	      for (j = 0, molst = mlst; j < in_size; j++, molst = SCM_CDR(molst))
		{
		  el = SCM_CAR(molst);
		  if (!(LIST_P(el))) 
		    snd_error("%s[%d] %s: saved mark data is not a list?? ",
			      __FILE__, __LINE__, __FUNCTION__);
		  sm = SCM_CADR(el);
		  if (NOT_FALSE_P(sm))
		    {
		      nm = SCM_CAR(el);
		      if (NOT_FALSE_P(nm))
			str = TO_C_STRING(nm);
		      else str = NULL;
		      id = TO_C_INT(SCM_CADDR(el));
		      if (LIST_LENGTH(el) > 3)
			sync = TO_C_INT(SCM_CADDDR(el));
		      else sync = 0;
		      cp->marks[i][j] = make_mark_1(TO_C_INT(sm), str, id, sync);
		      if (id > mark_id_counter) mark_id_counter = id;
		    }
		}
	    }
	}
      return(SCM_BOOL_T);
    }
  else 
    if (cp->marks) 
      snd_error("%s[%d] %s: there are marks here already!",
		__FILE__, __LINE__, __FUNCTION__);
  return(SCM_BOOL_F);
}

enum {MARK_SAMPLE, MARK_NAME, MARK_SYNC, MARK_HOME};

static SCM iread_mark(SCM n, int fld, SCM pos_n, char *caller)
{
  int pos;
  chan_info *ncp[1];
  mark *m = NULL;
  pos = TO_C_INT_OR_ELSE_WITH_ORIGIN(pos_n, -1, caller);
  m = find_mark_id(ncp, TO_C_INT_OR_ELSE_WITH_ORIGIN(n, 0, caller), pos);
  if (m == NULL) 
    return(snd_no_such_mark_error(caller, n));
  switch (fld)
    {
    case MARK_SAMPLE: 
      return(TO_SCM_INT(m->samp)); 
      break;
    case MARK_SYNC:   
      return(TO_SCM_INT(mark_sync(m))); 
      break;
    case MARK_NAME:   
      if (m->name) 
	return(TO_SCM_STRING(m->name)); 
      else return(TO_SCM_STRING("")); 
      break;
    case MARK_HOME:   
      return(SCM_LIST2(TO_SMALL_SCM_INT((ncp[0]->sound)->index),
		       TO_SMALL_SCM_INT(ncp[0]->chan))); 
      break;
    }
  return(SCM_BOOL_F);
}

static SCM iwrite_mark(SCM mark_n, SCM val, int fld, char *caller)
{
  chan_info *cp[1];
  mark *m;
  m = find_mark_id(cp, TO_C_INT_OR_ELSE_WITH_ORIGIN(mark_n, 0, caller), -1);
  if (m == NULL) 
    return(snd_no_such_mark_error(caller, mark_n));
  switch (fld)
    {
    case MARK_SAMPLE: 
      m->samp = mus_iclamp(0, 
			   TO_C_INT_OR_ELSE_WITH_ORIGIN(val, 0, caller),
			   current_ed_samples(cp[0]));
      sort_marks(cp[0]); /* update and re-sort current mark list */
      update_graph(cp[0], NULL);
      break;
    case MARK_SYNC: 
      set_mark_sync(m, TO_C_INT_OR_ELSE_WITH_ORIGIN(val, 0, caller));
      break;
    case MARK_NAME:
      if (m->name) FREE(m->name);
      m->name = copy_string(TO_C_STRING(val));
      update_graph(cp[0], NULL);
      break;
    }
  return(val);
}

static SCM g_markQ(SCM id_n)
{
  #define H_markQ "(" S_markQ " id) -> #t if mark is active"
  chan_info *ncp[1];
  ASSERT_TYPE(INTEGER_P(id_n), id_n, SCM_ARGn, S_markQ, "an integer");
  return(TO_SCM_BOOLEAN(find_mark_id(ncp, TO_C_INT(id_n), -1)));
}

static SCM g_mark_sample(SCM mark_n, SCM pos_n) 
{
  #define H_mark_sample "(" S_mark_sample " &optional id pos) returns the mark's location (sample number) at edit history pos"
  ASSERT_TYPE(INTEGER_IF_BOUND_P(mark_n), mark_n, SCM_ARG1, S_mark_sample, "an integer");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(pos_n), pos_n, SCM_ARG2, S_mark_sample, "an integer");
  return(iread_mark(mark_n, MARK_SAMPLE, pos_n, S_mark_sample));
}

static SCM g_set_mark_sample(SCM mark_n, SCM samp_n) 
{
  ASSERT_TYPE(INTEGER_IF_BOUND_P(mark_n), mark_n, SCM_ARG1, "set-" S_mark_sample, "an integer");
  ASSERT_TYPE(INTEGER_IF_BOUND_P(samp_n), samp_n, SCM_ARG2, "set-" S_mark_sample, "an integer");
  return(iwrite_mark(mark_n, samp_n, MARK_SAMPLE, "set-" S_mark_sample));
}

static SCM g_mark_sync(SCM mark_n) 
{
  #define H_mark_sync "(" S_mark_sync " id) returns the mark's sync value"
  ASSERT_TYPE(INTEGER_IF_BOUND_P(mark_n), mark_n, SCM_ARGn, S_mark_sync, "an integer");
  return(iread_mark(mark_n, MARK_SYNC, SCM_UNDEFINED, S_mark_sync));
}

static SCM g_set_mark_sync(SCM mark_n, SCM sync_n) 
{
  ASSERT_TYPE(INTEGER_IF_BOUND_P(mark_n), mark_n, SCM_ARG1, "set-" S_mark_sync, "an integer");
  ASSERT_TYPE(INTEGER_OR_BOOLEAN_IF_BOUND_P(sync_n), sync_n, SCM_ARG2, "set-" S_mark_sync, "an integer");
  return(iwrite_mark(mark_n, sync_n, MARK_SYNC, "set-" S_mark_sync));
}

static SCM g_mark_name(SCM mark_n) 
{
  #define H_mark_name "(" S_mark_name " id &optional snd chn) returns the mark's name"
  ASSERT_TYPE(INTEGER_IF_BOUND_P(mark_n), mark_n, SCM_ARGn, S_mark_name, "an integer");
  return(iread_mark(mark_n, MARK_NAME, SCM_UNDEFINED, S_mark_name));
}

static SCM g_set_mark_name(SCM mark_n, SCM name) 
{
  ASSERT_TYPE(INTEGER_IF_BOUND_P(mark_n), mark_n, SCM_ARG1, "set-" S_mark_name, "an integer");
  ASSERT_TYPE(STRING_P(name), name, SCM_ARG2, "set-" S_mark_name, "a string");
  return(iwrite_mark(mark_n, name, MARK_NAME, "set-" S_mark_name));
}

static SCM g_mark_sync_max(void) 
{
  #define H_mark_sync_max "(" S_mark_sync_max ") -> max mark sync value seen so far"
  return(TO_SCM_INT(mark_sync_max()));
}

static SCM g_mark_to_sound(SCM mark_n)
{
  #define H_mark_to_sound "(" S_mark_to_sound " id) returns the sound (index) and channel that hold mark id"
  ASSERT_TYPE(INTEGER_IF_BOUND_P(mark_n), mark_n, SCM_ARGn, S_mark_to_sound, "an integer");
  return(iread_mark(mark_n, MARK_HOME, SCM_UNDEFINED, S_mark_to_sound));
}

static SCM g_find_mark(SCM samp_n, SCM snd_n, SCM chn_n) 
{
  #define H_find_mark "(" S_find_mark " samp-or-name &optional snd chn)\n\
finds the mark in snd's channel chn at samp (if a number) or with the given name (if a string), returning the mark id"

  mark **mps;
  int i, samp = 0;
  char *name = NULL;
  chan_info *cp = NULL;
  ASSERT_TYPE((NUMBER_P(samp_n) || STRING_P(samp_n) || (NOT_BOUND_P(samp_n)) || (FALSE_P(samp_n))), samp_n, SCM_ARG1, S_find_mark, "a number or string or #f");
  SND_ASSERT_CHAN(S_find_mark, snd_n, chn_n, 2); 
  cp = get_cp(snd_n, chn_n, S_find_mark);
  if (cp->marks == NULL) 
    return(snd_no_such_mark_error(S_find_mark,	samp_n));
  mps = cp->marks[cp->edit_ctr];
  if (mps)
    {
      if (STRING_P(samp_n))
	name = TO_C_STRING(samp_n);
      else samp = TO_C_INT_OR_ELSE(samp_n, 0);
      if (name)
	{
	  for (i = 0; i <= cp->mark_ctr[cp->edit_ctr]; i++) 
	    if ((mps[i]) && 
		(mps[i]->name) && 
		(strcmp(name, mps[i]->name) == 0))
	      return(TO_SCM_INT(mark_id(mps[i])));
	}
      else
	{
	  for (i = 0; i <= cp->mark_ctr[cp->edit_ctr]; i++)
	    if ((mps[i]) && 
		(mps[i]->samp == samp)) 
	      return(TO_SCM_INT(mark_id(mps[i])));
	}
    }
  snd_no_such_mark_error(S_find_mark, samp_n);
  return(samp_n);
}

static SCM g_add_mark(SCM samp_n, SCM snd_n, SCM chn_n) 
{
  #define H_add_mark "(" S_add_mark ") samp &optional snd chn) adds a mark at sample samp returning the mark id"
  mark *m;
  chan_info *cp;
  ASSERT_TYPE(INTEGER_IF_BOUND_P(samp_n), samp_n, SCM_ARG1, S_add_mark, "an integer");
  SND_ASSERT_CHAN(S_add_mark, snd_n, chn_n, 2);
  cp = get_cp(snd_n, chn_n, S_add_mark);
  m = add_mark(TO_C_INT_OR_ELSE(samp_n, 0), NULL, cp);
  if (m)
    {
      /* if it's a redundant mark, and we're ignoring them, return -1 */
      update_graph(cp, NULL);
      return(TO_SCM_INT(mark_id(m)));
    }
  else return(TO_SCM_INT(-1)); /* ?? SCM_BOOL_F? or 'redundant-mark? */
}

static SCM g_delete_mark(SCM id_n) 
{
  #define H_delete_mark "(" S_delete_mark " id) delete mark id (- C-m)"
  chan_info *cp[1];
  mark *m;
  int id;
  ASSERT_TYPE(INTEGER_IF_BOUND_P(id_n), id_n, SCM_ARGn, S_delete_mark, "an integer");
  m = find_mark_id(cp, id = TO_C_INT_OR_ELSE(id_n, 0), -1);
  if (m == NULL) 
    return(snd_no_such_mark_error(S_delete_mark, id_n));
  delete_mark_id(id, cp[0]);
  update_graph(cp[0], NULL);
  return(id_n);
}

static SCM g_delete_marks(SCM snd_n, SCM chn_n) 
{
  #define H_delete_marks "(" S_delete_marks " &optional snd chn) delete all marks in snd's channel chn"
  chan_info *cp;
  SND_ASSERT_CHAN(S_delete_marks, snd_n, chn_n, 1);
  cp = get_cp(snd_n, chn_n, S_delete_marks);
  delete_marks(cp);
  return(SCM_BOOL_F);
}

static SCM int_array_to_list(int *arr, int i, int len)
{
  if (i < len)
    return(CONS(TO_SCM_INT(arr[i]), 
		int_array_to_list(arr, i + 1, len)));
  else return(CONS(TO_SCM_INT(arr[i]), 
		   SCM_EOL));
}

static int *syncd_marks(snd_state *ss, int sync)
{
  syncdata *sd;
  int *ids;
  int i;
  sd = make_syncdata(sync);
  map_over_chans(ss, gather_chan_syncd_marks, (void *)sd);
  ids = (int *)CALLOC(1+sd->mark_ctr, sizeof(int));
  ids[0] = sd->mark_ctr;
  for (i = 0; i < sd->mark_ctr; i++) ids[i + 1] = mark_id(sd->marks[i]);
  free_syncdata(sd);
  return(ids);
}

static SCM g_syncd_marks(SCM sync)
{
  #define H_syncd_marks "(" S_syncd_marks " sync) -> list of mark ids that share sync"
  int *ids;
  SCM res;
  ASSERT_TYPE(INTEGER_P(sync), sync, SCM_ARGn, S_syncd_marks, "an integer");
  ids = syncd_marks(get_global_state(), TO_C_INT(sync));
  if ((ids == NULL) || (ids[0] == 0)) return(SCM_EOL);
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

static SCM g_marks(SCM snd_n, SCM chn_n, SCM pos_n) 
{
  #define H_marks "(" S_marks " &optional snd chn pos) -> list of marks (ids) in snd/chn at edit history position pos"
  /* mark list is: channel: (id id id), snd: ((id id) (id id...)), neither: (((id...)...)...) */
  chan_info *cp;
  snd_info *sp;
  snd_state *ss;
  SCM res, res1 = SCM_EOL;
  int *ids;
  int i, pos, j;
  if (INTEGER_P(snd_n))
      {
	if (INTEGER_P(chn_n))
	  {
	    cp = get_cp(snd_n, chn_n, S_marks);
	    if (INTEGER_P(pos_n)) 
	      pos = TO_C_INT(pos_n); 
	    else pos = cp->edit_ctr;
	    ids = channel_marks(cp, pos);
	    if (ids == NULL) return(SCM_EOL);
	    if (ids[0] == 0) 
	      {
		FREE(ids); 
		return(SCM_EOL);
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
		  res1 = CONS(SCM_EOL, res1);
		else res1 = CONS(int_array_to_list(ids, 1, ids[0]), 
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
	    res1 = CONS(g_marks(TO_SMALL_SCM_INT(j), SCM_UNDEFINED, SCM_UNDEFINED), 
			   res1);
	}
    }
  return(res1);
}

static SCM g_forward_mark(SCM count, SCM snd, SCM chn) 
{
  #define H_forward_mark "(" S_forward_mark " &optional (count 1) snd chn) moves the cursor forward by count marks"
  int val; 
  chan_info *cp;
  ASSERT_TYPE(INTEGER_IF_BOUND_P(count), count, SCM_ARG1, S_forward_mark, "an integer");
  SND_ASSERT_CHAN(S_forward_mark, snd, chn, 2);
  cp = get_cp(snd, chn, S_forward_mark);
  val = TO_C_INT_OR_ELSE(count, 1); 
  handle_cursor(cp, goto_mark(cp, val));
  return(TO_SCM_INT(val));
}

static SCM g_backward_mark(SCM count, SCM snd, SCM chn) 
{
  #define H_backward_mark "(" S_backward_mark " &optional (count 1) snd chn) moves the cursor back by count marks"
  int val; 
  chan_info *cp;
  ASSERT_TYPE(INTEGER_IF_BOUND_P(count), count, SCM_ARG1, S_backward_mark, "an integer");
  SND_ASSERT_CHAN(S_backward_mark, snd, chn, 2);
  cp = get_cp(snd, chn, S_backward_mark);
  val = -(TO_C_INT_OR_ELSE(count, 1)); 
  handle_cursor(cp, goto_mark(cp, val));
  return(TO_SCM_INT(val));
}

static char *mark_file_name(snd_info *sp)
{
  char *newname;
  int len, i;
  len = strlen(sp->fullname);
  newname = (char *)CALLOC(len + 7, sizeof(char));
  strcpy(newname, sp->fullname);
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
  if (cp->marks) return(cp->mark_ctr[cp->edit_ctr] + 1); /* initialized to -1 -- 0 is first mark */
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
	  fprintf(fd, "(let ((sfile (find-sound \"%s\")))\n", sp->shortname);
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

static SCM g_save_marks(SCM snd_n)
{
  #define H_save_marks "(" S_save_marks " &optional snd) saves snd's marks in <snd's file-name>.marks"
  snd_info *sp;
  char *str;
  SCM res = SCM_BOOL_F;
  SND_ASSERT_SND(S_save_marks, snd_n, 1);
  sp = get_sp(snd_n);
  if (sp == NULL) 
    return(snd_no_such_sound_error(S_save_marks, snd_n));
  str = save_marks(sp);
  if (str)
    {
      res = TO_SCM_STRING(str);
      FREE(str);
    }
  return(res);
}

void g_init_marks(SCM local_doc)
{
  #define H_mark_drag_hook S_mark_drag_hook " (id) is called when a mark is dragged"

  mark_drag_hook = MAKE_HOOK(S_mark_drag_hook, 1, H_mark_drag_hook); /* arg = id */

  define_procedure_with_setter(S_mark_sample, SCM_FNC g_mark_sample, H_mark_sample,
			       "set-" S_mark_sample, SCM_FNC g_set_mark_sample,
			       local_doc, 0, 2, 1, 1);

  define_procedure_with_setter(S_mark_sync, SCM_FNC g_mark_sync, H_mark_sync,
			       "set-" S_mark_sync, SCM_FNC g_set_mark_sync,
			       local_doc, 0, 1, 1, 1);

  define_procedure_with_setter(S_mark_name, SCM_FNC g_mark_name, H_mark_name,
			       "set-" S_mark_name, SCM_FNC g_set_mark_name,
			       local_doc, 0, 1, 1, 1);

  DEFINE_PROC(S_restore_marks, g_restore_marks, 4, 0, 0, "internal func");
  DEFINE_PROC(S_mark_sync_max, g_mark_sync_max, 0, 0, 0, H_mark_sync_max);
  DEFINE_PROC(S_mark_to_sound, g_mark_to_sound, 0, 1, 0, H_mark_to_sound);
  DEFINE_PROC(S_marks,         g_marks, 0, 3, 0,         H_marks);
  DEFINE_PROC(S_add_mark,      g_add_mark, 0, 3, 0,      H_add_mark);
  DEFINE_PROC(S_delete_mark,   g_delete_mark, 0, 1, 0,   H_delete_mark);
  DEFINE_PROC(S_delete_marks,  g_delete_marks, 0, 2, 0,  H_delete_marks);
  DEFINE_PROC(S_syncd_marks,   g_syncd_marks, 1, 0, 0,   H_syncd_marks);
  DEFINE_PROC(S_find_mark,     g_find_mark, 1, 2, 0,     H_find_mark);
  DEFINE_PROC(S_forward_mark,  g_forward_mark, 0, 3, 0,  H_forward_mark);
  DEFINE_PROC(S_backward_mark, g_backward_mark, 0, 3, 0, H_backward_mark);
  DEFINE_PROC(S_save_marks,    g_save_marks, 0, 1, 0,    H_save_marks);
  DEFINE_PROC(S_markQ,         g_markQ, 1, 0, 0,         H_markQ);
}


