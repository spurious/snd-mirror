#include "snd.h"

/* to handle undo/redo cleanly, we keep the mark list as an array (indexed to edit_ctr)
 * of arrays of pointers to marks.  Any mark-related operation follows cp->edit_ctr's
 * array within the outer array.  This way, marks come and go in the intuitively clean
 * manner without endless list operations in this file.  Each mark is very small,
 * so the extra data should not be a problem.
 */

typedef mark *mark_map_func(chan_info *cp, mark *mp, mark *m);

#define ID_MASK      0x0fffffff
#define VISIBLE_MASK 0xf0000000
#define VISIBLE_ON   0x10000000

static mark *map_over_marks(chan_info *cp, mark_map_func *func, mark *m, int direction)
{
  int i,marks,pos;
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
	      for (i=0;i<=marks;i++) 
		{
		  mp = (*func)(cp,mps[i],m);
		  if (mp) return(mp);
		}
	    }
	  else
	    {
	      for (i=marks;i>=0;i--) 
		{
		  mp = (*func)(cp,mps[i],m);
		  if (mp) return(mp);
		}
	    }
	}
    }
  return(NULL);
}


static mark *make_mark_1(int samp, char *name, int id)
{
  mark *mp;
  mp = (mark *)CALLOC(1,sizeof(mark));
  if (name) mp->name = copy_string(name); else mp->name = NULL;
  mp->samp = samp;
  mp->id = id;
  return(mp);
}

static int mark_id_counter = 0;
static mark *make_mark(int samp, char *name) {return(make_mark_1(samp,name,mark_id_counter++));}
static mark *copy_mark(mark *m) {return(make_mark_1(m->samp,m->name,m->id));}
int mark_id(mark *m) {return((m->id) & ID_MASK);}

static mark *free_mark (mark *mp)
{
  if (mp)
    {
      if (mp->name) FREE(mp->name);
      FREE(mp);
    }
  return(NULL);
}

mark *find_mark_id(chan_info *cp, int id)
{
  mark **mps;
  int i;
  if (cp) 
    {
      if (cp->marks)
	{
	  mps = cp->marks[cp->edit_ctr];
	  if (mps)
	    {
	      for (i=0;i<=cp->mark_ctr[cp->edit_ctr];i++)
		if ((mps[i]) && ((mps[i]->id & ID_MASK) == (unsigned int)id))
		  return(mps[i]);
	    }
	}
    }
  return(NULL);
}

static mark *find_named_mark_1(chan_info *cp, mark *mp, mark *m)
{
  if ((mp->name) && (m->name) && (strcmp(mp->name,m->name) == 0)) return(mp);
  else return(NULL);
}

static mark *find_named_mark(chan_info *cp, char *name)
{
  mark *m,*mp;
  m=make_mark_1(0,name,0);
  mp = map_over_marks(cp,find_named_mark_1,m,READ_FORWARD);
  free_mark(m);
  return(mp);
}

static mark *find_previous_mark_1(chan_info *cp, mark *mp, mark *m)
{
  if (mp->samp < m->samp) return(mp);
  else return(NULL);
}

static mark *find_previous_mark (int current_sample, chan_info *cp)
{
  mark *m,*mp;
  m=make_mark_1(current_sample,NULL,0);
  mp = map_over_marks(cp,find_previous_mark_1,m,READ_BACKWARD);
  free_mark(m);
  return(mp);
}

static mark *find_next_mark_1(chan_info *cp, mark *mp, mark *m)
{
  if (mp->samp > m->samp) return(mp);
  else return(NULL);
}

static mark *find_next_mark (int current_sample, chan_info *cp)
{
  mark *m,*mp;
  m=make_mark_1(current_sample,NULL,0);
  mp = map_over_marks(cp,find_next_mark_1,m,READ_FORWARD);
  free_mark(m);
  return(mp);
}

static mark* marks_off_1(chan_info *cp, mark *mp, mark *m)
{
  mp->id &= ID_MASK;
  return(NULL);
}

void marks_off(chan_info *cp)
{
  map_over_marks(cp,marks_off_1,NULL,READ_FORWARD);
}


#define PLAY_ARROW_SIZE 10
#define MARK_TAB_WIDTH 10
#define MARK_TAB_HEIGHT 4

static void draw_mark_1(chan_info *cp, axis_info *ap, mark *mp, int show)
{
  /* fields are samp and name */
  int top,len,cx,y0,y1;
  axis_context *ax;
  ax = mark_context(cp); /* was cursor_context */
  top = ap->y_axis_y1;
  y1 = top;
  y0 = ap->y_axis_y0;
  if (mp->name) top += 10;
  cx = grf_x((double)(mp->samp)/(double)SND_SRATE(cp->sound),ap);
  if (mp->name)
    {
      activate_button_font(ax,cp->state);
      len = mark_name_width(cp->state,mp->name);
      draw_string(ax,(int)(cx-0.5*len),y1+6,mp->name,strlen(mp->name));
    }
  fill_rectangle(ax,
		 cx - MARK_TAB_WIDTH,top,
		 2*MARK_TAB_WIDTH,MARK_TAB_HEIGHT);
  draw_line(ax,cx,top+4,cx,y0);
  fill_polygon(ax,4,
	       cx,                y0,
	       cx+PLAY_ARROW_SIZE,y0 +   PLAY_ARROW_SIZE,
	       cx,                y0 + 2*PLAY_ARROW_SIZE,
	       cx,                y0);

  if (show) mp->id |= VISIBLE_ON; else mp->id &= ID_MASK;
}

static void draw_play_triangle(chan_info *cp, int x)
{
  int y0;
  y0 = ((axis_info *)(cp->axis))->y_axis_y0;
  draw_polygon(mark_context(cp), 4,
	       x,                y0,
	       x+PLAY_ARROW_SIZE,y0 +   PLAY_ARROW_SIZE,
	       x,                y0 + 2*PLAY_ARROW_SIZE,
	       x,                y0);
}

void draw_mark(chan_info *cp, axis_info *ap, mark *mp)
{
  if (!(mp->id & VISIBLE_MASK)) draw_mark_1(cp,ap,mp,1);
}

static void erase_mark(chan_info *cp, axis_info *ap, mark *mp)
{
  if (mp->id & VISIBLE_MASK) draw_mark_1(cp,ap,mp,0);
}

static mark *hit_mark_1(chan_info *cp, mark *mp, mark *m)
{
  /* m->samp = raw x mouse position */
  /* we're going left to right, so after passing x, give up */
  int mx,y;
  axis_info *ap;
  mx = grf_x((double)(mp->samp)/(double)SND_SRATE(cp->sound),cp->axis);
  if (mx > (m->samp+MARK_TAB_WIDTH)) return(m);    /* past it */
  if (mx < (m->samp-MARK_TAB_WIDTH)) return(NULL); /* before it */
  if (mp->name == NULL)                            /* check y if unnamed */
    {
      ap = cp->axis;
      y = m->id;
      if ((y >= ap->y_axis_y1) && (y <= (ap->y_axis_y1+MARK_TAB_HEIGHT))) 
	return(mp); 
      else return(NULL);
    }
  else return(mp);
}

mark *hit_mark(chan_info *cp, int x, int y)
{
  mark *m,*mp;
  axis_info *ap;
  ap = cp->axis;
  if (cp->marks)
    {
      /* first check that we're in the top portion of the graph where the mark tabs are */
      if ((y >= ap->y_axis_y1) && (y <= (ap->y_axis_y1+MARK_TAB_HEIGHT+10))) /* +10 for named marks -- checked again later */
	{
	  m = make_mark_1(x,NULL,y);
	  mp = map_over_marks(cp,hit_mark_1,m,READ_FORWARD);
	  if (mp == m) mp = NULL; /* premature exit flag */
	  free_mark(m);
	  return(mp);
	}
    }
  return(NULL);
}

static mark *hit_triangle_1(chan_info *cp, mark *mp, mark *m)
{
  /* m->samp = raw x mouse position */
  /* we're going left to right, so after passing x, give up */
  int mx,y;
  axis_info *ap;
  mx = grf_x((double)(mp->samp)/(double)SND_SRATE(cp->sound),cp->axis);
  if (mx > m->samp) return(m);
  if ((mx+PLAY_ARROW_SIZE) < m->samp) return(NULL);
  ap = cp->axis;
  y = m->id - ap->y_axis_y0 - PLAY_ARROW_SIZE;
  if (y < 0) y = -y;
  if ((mx+PLAY_ARROW_SIZE-y) >= m->samp) return(mp);
  /* the last is assuming the triangle shape for hit detection */
  return(NULL);
}

mark *hit_triangle(chan_info *cp, int x, int y)
{
  mark *m,*mp;
  axis_info *ap;
  ap = cp->axis;
  if (cp->marks)
    {
      /* first check that we're in the bottom portion of the graph where the mark triangles are */
      if ((y >= ap->y_axis_y0) && (y <= (ap->y_axis_y0+2*PLAY_ARROW_SIZE)))
	{
	  m = make_mark_1(x,NULL,y);
	  mp = map_over_marks(cp,hit_triangle_1,m,READ_FORWARD);
	  if (mp == m) mp = NULL; /* premature exit flag */
	  free_mark(m);
	  return(mp);
	}
    }
  return(NULL);
}


static int watching_mouse = 0;
static int last_mouse_x = 0;
static mark *moving_mark = NULL;

static void start_mark_watching(chan_info *cp, mark *mp)
{
  moving_mark = mp;
  StartMarkWatch(cp);
  watching_mouse = 1;
}

static void cancel_mark_watch(chan_info *cp)
{
  CancelMarkWatch();
  watching_mouse = 0;
  moving_mark = NULL;
}

static void move_mark_1(chan_info *cp, mark *mp, int x)
{
  axis_info *ap;
  int nx,samps;
  ap = cp->axis;
  erase_mark(cp,ap,mp);
  if ((x > ap->x_axis_x1) || (x < ap->x_axis_x0)) 
    {
      if (watching_mouse)
	{
	  if ((x < ap->x_axis_x0) && (ap->x0 == ap->xmin)) return;
	  if ((x > ap->x_axis_x1) && (ap->x1 == ap->xmax)) return;
	}
      nx = move_axis(cp,ap,x);
      if (!watching_mouse) start_mark_watching(cp,mp);
    }
  else 
    {
      nx = x;
      if (watching_mouse) cancel_mark_watch(cp);
    }
  mp->samp = (int)(ungrf_x(ap,nx) * SND_SRATE(cp->sound));
  if (mp->samp < 0) mp->samp = 0;
  samps = current_ed_samples(cp);
  if (mp->samp > samps) mp->samp = samps;
  if (!watching_mouse) draw_mark(cp,ap,mp);
}

void move_mark_2(chan_info *cp)
{
  move_mark_1(cp,moving_mark,last_mouse_x);
}

void move_mark(chan_info *cp, mark *mp, int x, int y)
{
  last_mouse_x = x;
  move_mark_1(cp,mp,x);
}


static int prev_cx = -1;

int move_play_mark(chan_info *cp, int *mc, int cx)
{
  /* mc = mouse loc sampwise return samps updating mc */
  int cur_mc;
  axis_info *ap;
  ap = cp->axis;
  if (prev_cx > 0) draw_play_triangle(cp,prev_cx);
  prev_cx = cx;
  draw_play_triangle(cp,cx);
  cur_mc = (*mc);
  (*mc) = (int)(ungrf_x(ap,cx) * SND_SRATE(cp->sound));
  return((*mc) - cur_mc);
}

void finish_moving_play_mark(chan_info *cp)
{
  snd_info *sp;
  sp = cp->sound;
  draw_play_triangle(cp,prev_cx);
  prev_cx = -1;
  sp->srate = 1.0;
}

static int compare_mark_samps(const void *mp1, const void *mp2)
{
  mark *m1,*m2;
  m1 = (mark *)(*((mark **)mp1));
  m2 = (mark *)(*((mark **)mp2));
  if (m1->samp < m2->samp) return(-1);
  if (m1->samp == m2->samp) return(0);
  return(1);
}

void finish_moving_mark(chan_info *cp, mark *mp)
{
  /* make sure marks are in order still */
  mark **mps;
  int ed;
  if (watching_mouse) cancel_mark_watch(cp);
  ed = cp->edit_ctr;
  mps = cp->marks[ed];
  qsort((void *)mps,cp->mark_ctr[ed]+1,sizeof(mark *),compare_mark_samps);
}


static int ignore_redundant_marks = 1;

static void allocate_marks(chan_info *cp, int edit_ctr)
{
  int i;
  cp->marks_size = edit_ctr+16;
  cp->marks = (mark ***)CALLOC(cp->marks_size,sizeof(mark **));
  cp->mark_size = (int *)CALLOC(cp->marks_size,sizeof(int));
  cp->mark_ctr = (int *)CALLOC(cp->marks_size,sizeof(int));
  for (i=0;i<cp->marks_size;i++) cp->mark_ctr[i] = -1;
}

mark *add_mark(int samp, char *name, chan_info *cp)
{
  int i,j,ed;
  mark **mps;
  mark *mp;
  if (!(cp->marks)) allocate_marks(cp,cp->edit_ctr);
  /* our current mark list is cp->edit_ctr (it starts at 0 -- see snd-chn.c and snd-edits.c) */
  ed = cp->edit_ctr;
  cp->mark_ctr[ed]++;
  if (cp->mark_ctr[ed] >= cp->mark_size[ed])
    {
      cp->mark_size[ed] += 16;
      if (!cp->marks[ed]) cp->marks[ed] = (mark **)CALLOC(cp->mark_size[ed],sizeof(mark *));
      else 
	{
	  cp->marks[ed] = (mark **)REALLOC(cp->marks[ed],cp->mark_size[ed] * sizeof(mark *));
	  for (i=cp->mark_size[ed]-16;i<cp->mark_size[ed];i++) cp->marks[ed][i] = NULL;
	}
    }
  mps = cp->marks[ed];
  if (cp->mark_ctr[ed] == 0)
    {
      if (mps[0]) free_mark(mps[0]);
      mps[0] = make_mark(samp,name);
      return(mps[0]);
    }
  else
    {
      for (i=0;i<cp->mark_ctr[ed];i++)
	{
	  mp = mps[i];
	  if ((ignore_redundant_marks) && (samp == mp->samp))
	    {
	      cp->mark_ctr[ed]--;
	      return(NULL);
	    }
	  if (samp < mp->samp)
	    {
	      for (j=cp->mark_ctr[ed];j>i;j--)
		{
		  mps[j] = mps[j-1];
		}
	      mps[i] = make_mark(samp,name);
	      return(mps[i]);
	    }
	}
      /* insert at end */
      mps[cp->mark_ctr[ed]] = make_mark(samp,name);
      return(mps[cp->mark_ctr[ed]]);
    }
}

void delete_mark_samp(int samp, chan_info *cp)
{
  int i,j,ed,edm;
  mark *mp;
  mark **mps;
  char *buf;
  axis_info *ap;
  if ((cp) && (cp->marks))
    {
      ed = cp->edit_ctr;
      mps = cp->marks[ed];
      if (mps)
	{
	  edm = cp->mark_ctr[ed];
	  for (i=0;i<=edm;i++)
	    {
	      mp = mps[i];
	      if (mp->samp == samp)
		{
		  ap = cp->axis;
		  if ((mp->samp >= ap->losamp) && (mp->samp <= ap->hisamp)) erase_mark(cp,cp->axis,mp); 
		  free_mark(mp);
		  mps[i] = NULL;
		  if (i < edm)
		    {
		      for (j=i;j<edm;j++) mps[j] = mps[j+1];
		      mps[edm] = NULL;
		    }
		  cp->mark_ctr[ed]--;
		  return;
		}
	    }
	}
    }
  buf = (char *)CALLOC(64,sizeof(char));
  sprintf(buf,"no mark at sample %d",samp);
  report_in_minibuffer(cp->sound,buf);
  FREE(buf);
}

void delete_mark_id(int id, chan_info *cp)
{
  int i,j,ed,edm;
  mark *mp;
  mark **mps;
  char *buf;
  axis_info *ap;
  if ((cp) && (cp->marks))
    {
      ed = cp->edit_ctr;
      mps = cp->marks[ed];
      if (mps)
	{
	  edm = cp->mark_ctr[ed];
	  for (i=0;i<=edm;i++)
	    {
	      mp = mps[i];
	      if ((mp->id & ID_MASK) == (unsigned int)id)
		{
		  ap = cp->axis;
		  if ((mp->samp >= ap->losamp) && (mp->samp <= ap->hisamp)) erase_mark(cp,cp->axis,mp); 
		  free_mark(mp);
		  mps[i] = NULL;
		  if (i < edm)
		    {
		      for (j=i;j<edm;j++) mps[j] = mps[j+1];
		      mps[edm] = NULL;
		    }
		  cp->mark_ctr[ed]--;
		  return;
		}
	    }
	}
    }
  buf = (char *)CALLOC(64,sizeof(char));
  sprintf(buf,"no mark with id: %d",id);
  report_in_minibuffer(cp->sound,buf);
  FREE(buf);
}

void delete_marks (chan_info *cp)
{
  int i,ed;
  mark *mp;
  mark **mps;
  if ((cp) && (cp->marks))
    {
      ed = cp->edit_ctr;
      mps = cp->marks[ed];
      if (mps)
	{
	  for (i=0;i<cp->mark_size[ed];i++)
	    {
	      mp = mps[i];
	      if (mp) free_mark(mp);
	      mps[i] = NULL;
	    }
	  cp->mark_ctr[ed] = -1;
	  /* update_graph(cp,NULL); */ /* or dependent on graph_hook? -- if (!(ss->graph_hook_active)) ... */
	}
    }
}

void free_mark_list(chan_info *cp, int ignore)
{
  int i,j;
  mark **mps;
  mark *mp;
  if (cp->marks)
    {
      for (i=0;i<cp->marks_size;i++) 
	{
	  if (i != ignore)
	    {
	      mps = cp->marks[i];
	      if (mps)
		{
		  for (j=0;j<cp->mark_size[i];j++)
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
  int i,ed,len,size;
  chan_info *cp;
  mark **mps;
  for (i=0;i<sp->nchans;i++)
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
	      free_mark_list(cp,ed);
	      allocate_marks(cp,0);
	      cp->marks[0] = mps;
	      cp->mark_ctr[0] = len;
	      cp->mark_size[0] = size;
	    }
	}
    }
}

static mark *find_nth_mark(chan_info *cp, int count)
{
  int i,c,samp;
  mark *mp = NULL;
  if ((!cp) || (!cp->marks)) return(NULL);
  if (count > 0) c=count; else c=-count;
  samp = cp->cursor;
  for (i=0;i<c;i++)
    {
      if (count > 0) mp = find_next_mark(samp,cp);
      else mp = find_previous_mark(samp,cp);
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
      if (cp) report_in_minibuffer(cp->sound,"no marks");
      return(CURSOR_IN_VIEW);
    }
  mp = find_nth_mark(cp,count);
  if (!mp) 
    {
      report_in_minibuffer(cp->sound,"no such mark");
      return(CURSOR_IN_VIEW);
    }
  return(cursor_moveto(cp,mp->samp));
}

int goto_named_mark(chan_info *cp, char *name)
{
  mark *mp;
  mp = find_named_mark(cp,name);
  if (mp) return(cursor_moveto(cp,mp->samp));
  return(CURSOR_IN_VIEW);
}

static mark *active_mark_1(chan_info *cp, mark *mp, mark *m)
{
  axis_info *ap;
  ap = cp->axis;
  if ((mp->samp >= ap->losamp) && (mp->samp <= ap->hisamp)) return(mp); 
  return(NULL);
}

mark *active_mark(chan_info *cp)
{
  return(map_over_marks(cp,active_mark_1,NULL,READ_FORWARD));
}

int mark_beg(chan_info *cp)
{
  mark *mp;
  mp = active_mark(cp);
  if (mp) return(mp->samp);
  return(-1);
}

static mark *display_channel_marks_1(chan_info *cp,  mark *mp, mark *m)
{
  axis_info *ap;
  ap = cp->axis;
  if (mp->samp > ap->hisamp) return(mp); /* terminates loop */
  if ((mp->samp >= ap->losamp) && (mp->samp <= ap->hisamp) && (mp != moving_mark)) draw_mark(cp,ap,mp);
  return(NULL);
}

void display_channel_marks(chan_info *cp)
{
  map_over_marks(cp,display_channel_marks_1,NULL,READ_FORWARD);
}

int add_named_mark(chan_info *cp) 
{
  snd_info *sp = cp->sound;
  clear_minibuffer(sp);
  make_minibuffer_label(sp,STR_mark_p);
  sp->minibuffer_on = 1;
  goto_minibuffer(sp);
  sp->marking = cp->cursor+1;
  return(CURSOR_IN_VIEW);
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
	  for (j=0;j<=cp->mark_ctr[edit_ctr];j++)
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
  int old,noo,end,i;
  mark **mps,**mpo;
  mark *mp;
  if ((cp) && (cp->marks))
    {
      if (cp->edit_ctr == 0) return;
      old = cp->edit_ctr-1;
      noo = cp->edit_ctr;
      if (noo>=cp->marks_size) /* groan -- we have to realloc the base array of array of pointers! */
	{
	  cp->marks_size += 16;
	  cp->marks = (mark ***)REALLOC(cp->marks,cp->marks_size * sizeof(mark **));
	  cp->mark_size = (int *)REALLOC(cp->mark_size,cp->marks_size * sizeof(int));
	  cp->mark_ctr = (int *)REALLOC(cp->mark_ctr,cp->marks_size * sizeof(int));
	  for (i=noo;i<cp->marks_size;i++) 
	    {
	      cp->mark_ctr[i] = -1;
	      cp->mark_size[i] = 0;
	      cp->marks[i] = NULL;
	    }
	}
      cp->mark_size[noo] = cp->mark_size[old];
      cp->mark_ctr[noo] = cp->mark_ctr[old];
      if (cp->marks[noo] == NULL)
	if (cp->mark_size[noo] > 0)
	  cp->marks[noo] = (mark **)CALLOC(cp->mark_size[noo],sizeof(mark *));
      if ((cp->mark_ctr[noo]>=0) && (cp->mark_size[noo]>0))
	{
	  mps = cp->marks[noo];
	  mpo = cp->marks[old];
	  for (i=0;i<=cp->mark_ctr[noo];i++)
	    {
	      if (mps[i]) free_mark(mps[i]);
	      mps[i] = copy_mark(mpo[i]);
	    }
	  if (change < 0)
	    {
	      /* if (change<0) and any marks are between beg and beg+change, they must be deleted */
	      end = beg-change-1;
	      i=0;
	      while (i<=cp->mark_ctr[noo])
		{
		  mp = mps[i];
		  if ((mp->samp >= beg) && (mp->samp <= end)) /* was mp->samp > beg, ditto end, beg can = end */
		    delete_mark_samp(mp->samp,cp); /* changes cp->mark_ctr, hence the while loop */
		  else 
		    {
		      if (mp->samp > beg)   /* don't change marks that precede the point of the change */
			mp->samp+=change;
		      i++;
		    }
		}
	    }
	  else
	    {
	      if (change > 0)
		{
		  for (i=0;i<=cp->mark_ctr[noo];i++)
		    {
		      mp = mps[i];
		      if (mp->samp > beg) mp->samp+=change;
		    }
		}
	    }
	}
    }
}

void mark_define_region(chan_info *cp,int count)
{
  int beg,end,temp;
  mark *mp;
  if (cp)
    {
      if (cp->marks)
	{
	  beg = cp->cursor;
	  mp = find_nth_mark(cp,count);
	  if (mp)
	    {
	      end = mp->samp;
	      if (end != beg)
		{
		  if (end < beg) {temp = end; end = beg; beg = temp;}
		  define_region(cp,beg,end,TRUE);
		}
	    }
	  else report_in_minibuffer(cp->sound,"no such mark");
	}
      else report_in_minibuffer(cp->sound,"no marks");
    }
}

static char *mark_file_name(snd_info *sp)
{
  char *newname;
  int len,i;
  len = strlen(sp->fullname);
  newname = (char *)CALLOC(len+7,sizeof(char));
  strcpy(newname,sp->fullname);
  for (i=len-1;i>0;i--) if (newname[i] == '.') break;
  if (i>0) len=i;
  newname[len]='.'; newname[len+1]='m'; newname[len+2]='a'; newname[len+3]='r'; newname[len+4]='k'; newname[len+5]='s'; newname[len+6]='\0';
  return(newname);
}

static int find_any_marks (chan_info *cp, void *ptr)
{
  if (cp->marks) return(cp->mark_ctr[cp->edit_ctr]+1); /* initialized to -1 -- 0 is first mark */
  return(0);
}

char *save_marks(snd_info *sp)
{
  char *newname = NULL,*str;
  int i;
  FILE *fd;
  if ((sp) && (map_over_sound_chans(sp,find_any_marks,NULL)))
    {
      newname = mark_file_name(sp);
      fd = fopen(newname,"w");
      if (fd)
	{
	  for (i=0;i<sp->nchans;i++)
	    save_mark_list(fd,sp->chans[i]);
	  fclose(fd);
	}
      else 
	{
	  str = (char *)CALLOC(128,sizeof(char));
	  sprintf(str,"%s %s ",newname,strerror(errno));
	  report_in_minibuffer(sp,str);
	  FREE(str);
	}
    }
  return(newname);
}

void save_mark_list(FILE *fd, chan_info *cp)
{
  /* assumes we're calling from the edit history list maker in snd-edits.c */
  /* as with edit-tree, graft these lists onto the end of the current mark list */
  /*   if none, pad out to current cp->edit_ctr? */
  /*   since the edits ripple the marks, we'll have to assume we're called later and hope */
  int i,j,marks;
  mark **mps;
  mark *m;
  if (cp->marks)
    {
      fprintf(fd,"      (%s %d sfile %d '(",S_restore_marks,cp->marks_size,cp->chan);
      for (i=0;i<cp->marks_size;i++)
	{
	  fprintf(fd,"\n        (%d %d (",cp->mark_size[i],cp->mark_ctr[i]);
	  mps = cp->marks[i];
	  if (mps)
	    {
	      marks = cp->mark_ctr[i];
	      for (j=0;j<=marks;j++)
		{
		  m = mps[j];
		  if (m)
		    {
		      if (m->name)
			fprintf(fd,"(%s %d %d) ",m->name,m->samp,(int)(m->id & ID_MASK));
		      else fprintf(fd,"(#f %d %d) ",m->samp,(int)(m->id & ID_MASK));
		    }
		  else fprintf(fd,"(#f #f #f) ");
		}
	    }
	  fprintf(fd,"))");
	}
      fprintf(fd,"))\n");
    }
}

static mark *reverse_mark_1(chan_info *cp, mark *mp, mark *m)
{
  mp->samp = m->samp - mp->samp;
  return(NULL);
}

void reverse_marks(chan_info *cp, int over_selection)
{
  mark *m;
  mark **mps;
  int ed,beg,end,marks,i;
  ed = cp->edit_ctr;
  mps = cp->marks[ed];
  if (!over_selection)
    {
      m=make_mark_1(current_ed_samples(cp)-1,NULL,0);
      map_over_marks(cp,reverse_mark_1,m,READ_FORWARD);
      FREE(m);
    }
  else
    {
      beg = selection_beg(cp);
      end = beg + region_len(0) -1;
      marks = cp->mark_ctr[ed];
      for (i=0;i<=marks;i++) 
	{
	  m = mps[i];
	  if ((m->samp >= beg) && (m->samp <= end))
	    m->samp = end - (m->samp - beg);
	}
    }
  if ((mps) && (cp->mark_ctr[ed] >= 0))
    qsort((void *)mps,cp->mark_ctr[ed]+1,sizeof(mark *),compare_mark_samps);
}

void src_marks(chan_info *cp,Float ratio,int old_samps,int new_samps, int beg, int over_selection)
{
  int i,marks,pos,end;
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
	      for (i=0;i<=marks;i++) 
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
	      for (i=0;i<=marks;i++) 
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
	  if (ratio < 0.0) qsort((void *)mps,marks+1,sizeof(mark *),compare_mark_samps);
	}
    }
}

void reset_marks(chan_info *cp, int num, int *samps, int end, int extension, int over_selection)
{
  int i,marks,pos;
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
	      for (i=0;i<=marks;i++) 
		{
		  m = mps[i];
		  if (m->samp > end) m->samp += extension;
		}
	    }
	  for (i=0;(i<=marks) && (i<num);i++) 
	    {
	      m = mps[i];
	      if (samps[i] >= 0) m->samp = samps[i];
	    }
	  qsort((void *)mps,marks+1,sizeof(mark *),compare_mark_samps);
	}
    }
}

void ripple_trailing_marks(chan_info *cp, int beg, int old_len, int new_len)
{
  int i,marks,pos;
  mark *m;
  mark **mps;
  if (cp->marks)
    {		
      ripple_marks(cp,0,0);
      pos = cp->edit_ctr;
      mps = cp->marks[pos];
      marks = cp->mark_ctr[pos];
      if ((mps) && (marks >= 0))
	{
	  for (i=0;i<=marks;i++) 
	    {
	      m = mps[i];
	      if (m->samp > (beg+old_len)) m->samp += (new_len - old_len);
	    }
	}
    }
}

#if HAVE_GUILE
static SCM g_restore_marks(SCM size, SCM snd, SCM chn, SCM marklist)
{
  SCM lst,el,nm,sm,mlst,s0,s1,s2;
  chan_info *cp;
  snd_info *sp;
  char *str;
  snd_state *ss;
  int i,j,list_size,in_size,id;
  ss = get_global_state();
  sp = ss->sounds[gh_scm2int(snd)];
  cp = sp->chans[gh_scm2int(chn)];
  if ((cp) && (!(cp->marks)))
    {
      cp->marks_size = gh_scm2int(size);
      cp->marks = (mark ***)CALLOC(cp->marks_size,sizeof(mark **));
      cp->mark_size = (int *)CALLOC(cp->marks_size,sizeof(int));
      cp->mark_ctr = (int *)CALLOC(cp->marks_size,sizeof(int));
      for (i=0;i<cp->marks_size;i++) cp->mark_ctr[i] = -1;
      list_size = gh_length(marklist);
      s0 = gh_int2scm(0);
      s1 = gh_int2scm(1);
      s2 = gh_int2scm(2);
      for (i=0;i<list_size;i++)
	{
	  lst = scm_list_ref(marklist,gh_int2scm(i));
	  cp->mark_size[i] = gh_scm2int(scm_list_ref(lst,s0));
	  cp->mark_ctr[i] = gh_scm2int(scm_list_ref(lst,s1));
          if (cp->mark_size[i] > 0)
	    {
	      mlst = scm_list_ref(lst,s2);
	      cp->marks[i] = (mark **)CALLOC(cp->mark_size[i],sizeof(mark *));
	      in_size = gh_length(mlst);
	      for (j=0;j<in_size;j++)
		{
		  el = scm_list_ref(mlst,gh_int2scm(j));
		  if (!(gh_list_p(el))) snd_error("%s[%d] %s: saved mark data is not a list?? ",__FILE__,__LINE__,__FUNCTION__);
		  sm = scm_list_ref(el,s1);
		  if (SCM_NFALSEP(sm))
		    {
		      nm = scm_list_ref(el,s0);
		      if (SCM_NFALSEP(nm))
			str = gh_scm2newstr(nm,NULL);
		      else str = NULL;
		      id = gh_scm2int(scm_list_ref(el,s2));
		      cp->marks[i][j] = make_mark_1(gh_scm2int(sm),str,id);
		      if (id > mark_id_counter) mark_id_counter = id;
		      if (str) {FREE(str); str=NULL;}
		    }
		}
	    }
	}
      return(SCM_BOOL_T);
    }
  else 
    if (cp->marks) snd_error("%s[%d] %s: there are marks here already!",__FILE__,__LINE__,__FUNCTION__);
  return(SCM_BOOL_F);
}

void gh_init_marks(void)
{
  gh_new_procedure4_0(S_restore_marks,g_restore_marks);
}

#endif
