#include "snd.h"

/* TODO  enved mix flt and src undo&apply cases need more testing (also clean undo of selection src)
 * TODO  edit of mix sound doesn't follow undo chains?
 */

Float un_dB(snd_state *ss, Float py)
{
  /* used only by envelope editor (snd-xenv etc) */
  return((py <= ss->min_dB) ? 0.0 : pow(10.0, py * .05));
}

static Float dB(snd_state *ss, Float py)
{
  return((py <= ss->lin_dB) ? ss->min_dB : (20.0 * (log10(py))));
}

env *free_env(env *e)
{
  if (e)
    {
      if (e->data) FREE(e->data);
      FREE(e);
    }
  return(NULL);
}

env *copy_env(env *e)
{
  env *ne;
  if (e)
    {
      ne = (env *)CALLOC(1, sizeof(env));
      ne->pts = e->pts;
      ne->data_size = e->pts * 2;
      ne->data = (Float *)MALLOC(e->pts * 2 * sizeof(Float));
      memcpy((void *)(ne->data), (void *)(e->data), e->pts * 2 * sizeof(Float));
      ne->base = e->base;
      return(ne);
    }
  return(NULL);
}

char *env_to_string(env *e)
{
  int i, j;
  char *expr_buf;
  char *news = NULL;
  if (e)
    {
      news = (char *)CALLOC(4 + (e->pts * 2 * 8), sizeof(char));
      news[0] = '\'';
      news[1] = '(';
      news[2] = '\0';
      expr_buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      for (i = 0, j = 0; i < e->pts; i++, j += 2)
	{
	  mus_snprintf(expr_buf, PRINT_BUFFER_SIZE, "%.3f %.3f ", e->data[j], e->data[j + 1]);
	  strcat(news, expr_buf);
	}
      FREE(expr_buf);
      strcat(news, ")");
    }
  else
    {
      news = copy_string("#f");
    }
  return(news);
}


env *make_envelope(Float *env_buffer, int len)
{
  env *e;
  int i, flen;
  if (len == 2) flen = 4; else flen = len;
  e = (env *)CALLOC(1, sizeof(env));
  e->data = (Float *)CALLOC(flen, sizeof(Float));
  e->data_size = flen;
  e->pts = flen / 2;
  for (i = 0; i < len; i++) e->data[i] = env_buffer[i];
  if ((flen == 4) && (len == 2))  /* fixup degenerate envelope */
    {
      e->data[2] = e->data[0] + 1.0; 
      e->data[3] = e->data[1];
    } 
  e->base = 1.0;
  return(e);
}

static void add_point (env *e, int pos, Float x, Float y)
{
  int i, j;
  if (e->pts * 2 == e->data_size)
    {
      e->data_size += 16;
      e->data = (Float *)REALLOC(e->data, (e->data_size) * sizeof(Float));
    }
  for (i = e->pts - 1, j = (e->pts - 1) * 2; i >= pos; i--, j -= 2)
    {
      e->data[j + 2] = e->data[j];
      e->data[j + 3] = e->data[j + 1];
#if DEBUGGING
      if ((j + 3) >= e->data_size) abort();
#endif

    }
  e->data[pos * 2] = x;
  e->data[pos * 2 + 1] = y;
#if DEBUGGING
      if ((pos * 2 + 1) >= e->data_size) abort();
#endif

  e->pts++;
}

void move_point (env *e, int pos, Float x, Float y)
{
  e->data[pos * 2] = x;
  e->data[pos * 2 + 1] = y;
#if DEBUGGING
      if ((pos * 2 + 1) >= e->data_size) abort();
#endif

}

void delete_point(env *e, int pos)
{
  int i, j;
  for (i = pos, j = pos * 2; i < e->pts - 1; i++, j += 2)
    {
      e->data[j] = e->data[j + 2];
      e->data[j + 1] = e->data[j + 3];
#if DEBUGGING
      if ((j + 1) >= e->data_size) abort();
#endif
    }
  e->pts--;
}

static int place_point(int *cxs, int points, int x)
{
  int i;
  for (i = 0; i < points; i++)
    if (x < cxs[i]) 
      return(i - 1);
  return(points);
}

static int hit_point(snd_state *ss, int *cxs, int *cys, int points, int x, int y)
{
  int i;
  for (i = 0; i < points; i++)
    if (((x > (cxs[i] - ss->enved_point_size)) && 
	 (x < (cxs[i] + ss->enved_point_size))) &&
	((y > (cys[i] - ss->enved_point_size)) && 
	 (y < (cys[i] + ss->enved_point_size))))
      return(i);
  return(-1);
}

env *default_env(Float x1, Float y)
{
  env *e;
  e = (env *)CALLOC(1, sizeof(env));
  e->data = (Float *)CALLOC(4, sizeof(Float));
  e->data_size = 4;
  e->pts = 2;
  e->data[0] = 0.0; 
  e->data[1] = y; 
  e->data[2] = x1;
  e->data[3] = y;
  e->base = 1.0;
  return(e);
}


/* -------- FILTER ENVELOPE -------- */

typedef struct {
  int *current_xs;
  int *current_ys;
  int current_size;
  chan_info *axis_cp;
  axis_info *gray_ap;
  TIME_TYPE down_time;
  int env_dragged;
  int env_pos;
  int click_to_delete;
  int edited;
} spflt;

void new_flt(snd_info *sp)
{
  spflt *spf;
  spf = (spflt *)CALLOC(1, sizeof(spflt));
  spf->current_xs = (int *)CALLOC(8, sizeof(int));
  spf->current_ys = (int *)CALLOC(8, sizeof(int));
  spf->current_size = 8;
  spf->env_dragged = 0;
  spf->env_pos = 0;
  spf->click_to_delete = 0;
  spf->edited = 0;
  (sp->sgx)->flt = spf;
}

void snd_filter_cleanup(snd_info *sp)
{
  spflt *spf;
  if (sp)
    {
      if (sp->sgx)
	{
	  spf = (spflt *)((sp->sgx)->flt);
	  if (spf)
	    {
	      spf->edited = 0;
	      spf->env_dragged = 0;
	      spf->env_pos = 0;
	      spf->click_to_delete = 0;
	    }
	  set_filter_text(sp, "");
	}
    }
}

static void sp_set_current_point(spflt *spf, int pos, int x, int y)
{
  if (pos == spf->current_size)
    {
      spf->current_size += 8;
      spf->current_xs = (int *)REALLOC(spf->current_xs, spf->current_size * sizeof(int));
      spf->current_ys = (int *)REALLOC(spf->current_ys, spf->current_size * sizeof(int));
    }
  spf->current_xs[pos] = x;
  spf->current_ys[pos] = y;
}

static void sp_make_axis_cp(snd_info *sp, char *name, int ex0, int ey0, int width, int height, Float xmin, Float xmax, Float ymin, Float ymax)
{
  /* conjure up minimal context for axis drawer in snd-axis.c */
  spflt *spf;
  snd_state *ss;
  ss = sp->state;
  spf = (spflt *)((sp->sgx)->flt);
  if (!(spf->axis_cp)) 
    {
      spf->axis_cp = new_env_axis(ss);
      fixup_axis_context((spf->axis_cp)->axis->ax, 
			 w_snd_filter_env(sp), 
			 (ss->sgx)->fltenv_basic_gc);
    }
  if (!(spf->gray_ap)) 
    {
      spf->gray_ap = new_wave_axis(ss);
      fixup_axis_context(spf->gray_ap->ax, 
			 w_snd_filter_env(sp), 
			 (ss->sgx)->fltenv_data_gc);
    }
  init_env_axes(spf->axis_cp, name, ex0, ey0, width, height, xmin, xmax, ymin, ymax);
}

#define EXP_SEGLEN 4

static short sp_grf_y_dB(snd_info *sp, Float val, axis_info *ap)
{
  if (sp->filter_control_in_dB)
    return(grf_y(dB(sp->state, val), ap));
  else return(grf_y(val, ap));
}

static double sp_ungrf_y_dB(snd_info *sp, axis_info *ap, int y)
{
  if (sp->filter_control_in_dB)
    return(un_dB(sp->state, ungrf_y(ap, y)));
  else return(ungrf_y(ap, y));
}

#define MIN_FILTER_GRAPH_HEIGHT 20

void display_filter_graph(snd_state *ss, snd_info *sp, axis_context *ax, int width, int height)
{
  axis_info *ap;
  spflt *spf;
  env *e;
  int i, j, k;
  Float ex0, ey0, ex1, ey1, val;
  int ix0, ix1, iy0, iy1, size, lx0, lx1, ly0, ly1;
  Float curx, xincr;
  int dur;
  e = sp->filter_control_env;
  if (e == NULL) return;
  spf = (spflt *)((sp->sgx)->flt);
  if (spf == NULL) return;
  ex0 = e->data[0];
  ey0 = e->data[1];
  ex1 = e->data[(e->pts * 2) - 2];
  ey1 = ey0;
  for (i = 3; i < e->pts * 2; i += 2)
    {
      val = e->data[i];
      if (ey0 > val) ey0 = val;
      if (ey1 < val) ey1 = val;
    }
  if (ey0 > 0.0) ey0 = 0.0;
  if ((ey0 == ey1) && (ey1 == 0.0)) ey1 = 1.0; /* fixup degenerate case */
  if (ey1 < 1.0) ey1 = 1.0;
  if (sp->filter_control_in_dB) 
    {
      ey0 = ss->min_dB; 
      ey1 = 0.0;
    }
  sp_make_axis_cp(sp, "frequency response", 0, 0, width, height, ex0, ex1, ey0, ey1); 
  ap = (spf->axis_cp)->axis;
  ix1 = grf_x(e->data[0], ap);
  iy1 = sp_grf_y_dB(sp, e->data[1], ap);
  if (e->pts < 100)
    size = ss->enved_point_size;
  else size = (int)(ss->enved_point_size * 0.4);
  sp_set_current_point(spf, 0, ix1, iy1);
  draw_arc(ax, ix1, iy1, size);
  if (sp->filter_control_in_dB)
    {
      for (j = 1, i = 2; i < e->pts * 2; i += 2, j++)
	{
	  ix0 = ix1;
	  iy0 = iy1;
	  ix1 = grf_x(e->data[i], ap);
	  iy1 = sp_grf_y_dB(sp, e->data[i + 1], ap);
	  sp_set_current_point(spf, j, ix1, iy1);
	  draw_arc(ax, ix1, iy1, size);
	  /* now try to fill in from the last point to this one */
	  if ((ix1 - ix0) < (2 * EXP_SEGLEN))
	    {
	      /* points are too close to be worth interpolating */
	      draw_line(ax, ix0, iy0, ix1, iy1);
	    }
	  else
	    {
	      /* interpolate so the display looks closer to dB (we should use the env base...) */
	      dur = (ix1 - ix0) / EXP_SEGLEN;
	      xincr = (e->data[i] - e->data[i - 2]) / (Float)dur;
	      curx = e->data[i - 2] + xincr;
	      lx1 = ix0;
	      ly1 = iy0;
	      for (k = 1; k < dur; k++, curx += xincr)
		{
		  lx0 = lx1;
		  ly0 = ly1;
		  lx1 = grf_x(curx, ap);
		  val = list_interp(curx, e->data, e->pts);
		  ly1 = grf_y(dB(ss, val), ap);
		  draw_line(ax, lx0, ly0, lx1, ly1);
		}
	      draw_line(ax, lx1, ly1, ix1, iy1);
	    }
	}
    }
  else
    {
      for (j = 1, i = 2; i < e->pts * 2; i += 2, j++)
	{
	  ix0 = ix1;
	  iy0 = iy1;
	  ix1 = grf_x(e->data[i], ap);
	  iy1 = grf_y(e->data[i + 1], ap);
	  sp_set_current_point(spf, j, ix1, iy1);
	  draw_arc(ax, ix1, iy1, size);
	  draw_line(ax, ix0, iy0, ix1, iy1);
	}
    }
  if (spf->edited)
    display_frequency_response(ss, e, ap, (spf->gray_ap)->ax, sp->filter_control_order, sp->filter_control_in_dB);
}

void handle_filter_point(snd_state *ss, snd_info *sp, int evx, int evy, TIME_TYPE motion_time)
{
  spflt *spf;
  axis_info *ap;
  Float x0, x1, x, y;
  env *e;
  spf = (spflt *)((sp->sgx)->flt);
  e = sp->filter_control_env;
  if ((motion_time - spf->down_time) < 100) return;
  spf->env_dragged = 1;
  spf->click_to_delete = 0;
  ap = (spf->axis_cp)->axis;
  x = ungrf_x(ap, evx);
  if (spf->env_pos > 0) 
    x0 = e->data[spf->env_pos * 2 - 2]; 
  else x0 = 0.0;
  if (spf->env_pos < e->pts) 
    x1 = e->data[spf->env_pos * 2 + 2]; 
  else x1 = sp->filter_control_env_xmax; /* x1 = 1.0; */
  if (x < x0) x = x0;
  if (x > x1) x = x1;
  if (spf->env_pos == 0) x = e->data[0];
  if (spf->env_pos == (e->pts - 1)) x = e->data[(e->pts - 1) * 2];
  y = ungrf_y(ap, evy);
  if (y < ap->y0) y = ap->y0;
  if (y > ap->y1) y = ap->y1;
  if (sp->filter_control_in_dB) y = un_dB(ss, y);
  move_point(e, spf->env_pos, x, y);
  spf->edited = 1;
  sp_display_env(sp);
  sp->filter_control_changed = 1;
}

void handle_filter_press(snd_info *sp, int evx, int evy, TIME_TYPE time)
{
  int pos;
  Float x, y;
  spflt *spf;
  axis_info *ap;
  env *e;
  spf = (spflt *)((sp->sgx)->flt);
  e = sp->filter_control_env;
  ap = (spf->axis_cp)->axis;
  spf->down_time = time;
  spf->env_dragged = 0;
  pos = hit_point(sp->state, spf->current_xs, spf->current_ys, e->pts, evx, evy);
  x = ungrf_x(ap, evx);
  y = sp_ungrf_y_dB(sp, ap, evy);
  if (pos == -1)
    {
      if (x < 0.0)
	{
	  pos = 0;
	  x = 0.0;
	}
      else 
	if (x > sp->filter_control_env_xmax) /* (x > 1.0) */
	  {
	    pos = e->pts - 1;
	    x = sp->filter_control_env_xmax; /* x = 1.0; */
	  }
    }
  spf->env_pos = pos;
  /* if not -1, then user clicked existing point -- wait for drag/release to decide what to do */
  if (pos == -1) 
    {
      pos = place_point(spf->current_xs, e->pts, evx);
      add_point(e, pos+1, x, y);
      spf->env_pos = pos + 1;
      spf->click_to_delete = 0;
      sp_display_env(sp);
    }
  else spf->click_to_delete = 1;
  spf->edited = 1;
}

void handle_filter_release(snd_info *sp)
{
  env *e;
  char *tmpstr = NULL;
  spflt *spf;
  spf = (spflt *)((sp->sgx)->flt);
  e = sp->filter_control_env;
  if ((spf->click_to_delete) && 
      (!(spf->env_dragged)) && 
      ((spf->env_pos > 0) && 
       (spf->env_pos < ((sp->filter_control_env)->pts - 1))))
    delete_point(e, spf->env_pos);
  spf->env_pos = 0;
  spf->env_dragged = 0;
  spf->click_to_delete = 0;
  sp_display_env(sp);
  set_filter_text(sp, tmpstr = env_to_string(e));
  if (tmpstr) FREE(tmpstr);
  sp->filter_control_changed = 1;
}

void report_filter_edit(snd_info *sp)
{
  spflt *spf;
  spf = (spflt *)((sp->sgx)->flt);
  spf->edited = 1;
}


/* -------- ENVELOPE EDITOR FUNCTIONS -------- */

static int env_list_size = 0;    /* current size of env edits list */
static int env_list_top = 0;     /* one past current active position in list */
static env **env_list = NULL;    /* env edits list (for local undo/redo/revert) */

static env **all_envs = NULL;    /* all envs, either loaded or created in editor */
static char **all_names = NULL;  /* parallel names */
static int all_envs_size = 0;    /* size of this array */
static int all_envs_top = 0;     /* one past pointer to last entry in this array */

static int *current_xs = NULL;
static int *current_ys = NULL;
static int current_size = 0;

static void set_current_point(int pos, int x, int y)
{
  if (pos == current_size)
    {
      if (current_size == 0)
	{
	  current_size = 32;
	  current_xs = (int *)CALLOC(current_size, sizeof(int));
	  current_ys = (int *)CALLOC(current_size, sizeof(int));
	}
      else
	{
	  current_size += 32;
	  current_xs = (int *)REALLOC(current_xs, current_size * sizeof(int));
	  current_ys = (int *)REALLOC(current_ys, current_size * sizeof(int));
	}
    }
  current_xs[pos] = x;
  current_ys[pos] = y;
}

chan_info *new_env_axis(snd_state *ss)
{
  chan_info *acp;
  axis_info *ap;
  axis_context *ax;
  acp = (chan_info *)CALLOC(1, sizeof(chan_info));
  acp->printing = 0;
  acp->state = ss;
  ap = (axis_info *)CALLOC(1, sizeof(axis_info));
  acp->axis = ap;
  ax = (axis_context *)CALLOC(1, sizeof(axis_context));
  ap->ax = ax;
  ap->ss = ss;
  ap->cp = acp;
  ax->ss = ss;
  return(acp);
}

axis_info *new_wave_axis(snd_state *ss)
{
  axis_info *gap;
  axis_context *gray_ax;
  gap = (axis_info *)CALLOC(1, sizeof(axis_info));
  gray_ax = (axis_context *)CALLOC(1, sizeof(axis_context));
  gap->ax = gray_ax;
  gap->ss = ss;
  gap->graph_active = 1;
  gray_ax->ss = ss;
  return(gap);
}

void init_env_axes(chan_info *acp, char *name, int x_offset, int ey0, int width, int height, Float xmin, Float xmax, Float ymin, Float ymax)
{
  axis_info *ap;
  ap = acp->axis;
  if (ap->xlabel) FREE(ap->xlabel);
  ap->xmin = xmin;
  ap->xmax = xmax;
  ap->ymin = ymin;
  ap->ymax = ymax;
  ap->y_ambit = ap->ymax - ap->ymin;
  ap->x_ambit = ap->xmax - ap->xmin;
  ap->xlabel = copy_string(name);
  ap->x0 = xmin;
  ap->x1 = xmax;
  ap->y0 = ymin;
  ap->y1 = ymax;
  ap->width = width;
  ap->window_width = width;
  ap->y_offset = ey0;
  ap->height = height;
  ap->graph_x0 = x_offset;
  make_axes_1(acp, ap, X_AXIS_IN_SECONDS, 1);
  /* if this is too small for an axis, it still sets up the fields needed for grf_x|y, so tiny envelope graphs will work */
}


static short grf_y_dB(snd_state *ss, Float val, axis_info *ap)
{
  if (enved_in_dB(ss))
    return(grf_y(dB(ss, val), ap));
  else return(grf_y(val, ap));
}

static double ungrf_y_dB(snd_state *ss, axis_info *ap, int y)
{
  if (enved_in_dB(ss))
    return(un_dB(ss, ungrf_y(ap, y)));
  else return(ungrf_y(ap, y));
}

void display_enved_env(snd_state *ss, env *e, axis_context *ax, chan_info *axis_cp, 
		       char *name, int x0, int y0, int width, int height, int dots)
{
  int i, j, k;
  Float ex0, ey0, ex1, ey1, val;
  int ix0, ix1, iy0, iy1, size = 0, lx0, lx1, ly0, ly1, index = 0;
  Float env_val, curx, xincr;
  mus_any *ce;
  int dur;

  if (e)
    {
      ex0 = e->data[0];
      ey0 = e->data[1];
      ex1 = e->data[(e->pts * 2) - 2];
      ey1 = ey0;
      for (i = 3; i < e->pts * 2; i += 2)
	{
	  val = e->data[i];
	  if (ey0 > val) ey0 = val;
	  if (ey1 < val) ey1 = val;
	}
      if (ey0 > 0.0) ey0 = 0.0;
      if ((ey0 == ey1) && (ey1 == 0.0)) ey1 = 1.0; /* fixup degenerate case */
      if ((dots) && (ey1 < 1.0)) ey1 = 1.0;
    }
  else
    {
      ex0 = 0.0;
      ex1 = 1.0;
      ey0 = 0.0;
      ey1 = 1.0;
    }

  if (enved_in_dB(ss)) 
    {
      ey0 = ss->min_dB; 
      ey1 = 0.0;
    }

  axis_cp = enved_make_axis_cp(ss, name, ax, x0, y0, width, height, ex0, ex1, ey0, ey1); /* ax used only for GC here */
  /* grf_x and grf_y (x|y, ap) can be used directly with XDrawLine */

  if (e)
    {
      ix1 = grf_x(e->data[0], axis_cp->axis);
      iy1 = grf_y_dB(ss, e->data[1], axis_cp->axis);
      if (dots)
	{
	  if (e->pts < 100)
	    size = ss->enved_point_size;
	  else size = (int)(ss->enved_point_size * 0.4);
	  set_current_point(0, ix1, iy1);
	  draw_arc(ax, ix1, iy1, size);
	}
      if (e->base == 1.0)
	{
	  if (enved_in_dB(ss))
	    {
	      for (j = 1, i = 2; i < e->pts * 2; i += 2, j++)
		{
		  ix0 = ix1;
		  iy0 = iy1;
		  ix1 = grf_x(e->data[i], axis_cp->axis);
		  iy1 = grf_y_dB(ss, e->data[i + 1], axis_cp->axis);
		  if (dots)
		    {
		      set_current_point(j, ix1, iy1);
		      draw_arc(ax, ix1, iy1, size);
		    }
		  /* now try to fill in from the last point to this one */
		  if ((ix1 - ix0) < (2 * EXP_SEGLEN))
		    {
		      /* points are too close to be worth interpolating */
		      draw_line(ax, ix0, iy0, ix1, iy1);
		    }
		  else
		    {
		      /* interpolate so the display looks closer to dB (we should use the env base...) */
		      dur = (ix1 - ix0) / EXP_SEGLEN;
		      xincr = (e->data[i] - e->data[i - 2]) / (Float)dur;
		      curx = e->data[i - 2] + xincr;
		      lx1 = ix0;
		      ly1 = iy0;
		      for (k = 1; k < dur; k++, curx += xincr)
			{
			  lx0 = lx1;
			  ly0 = ly1;
			  lx1 = grf_x(curx, axis_cp->axis);
			  val = list_interp(curx, e->data, e->pts);
			  ly1 = grf_y(dB(ss, val), axis_cp->axis);
			  draw_line(ax, lx0, ly0, lx1, ly1);
			}
		      draw_line(ax, lx1, ly1, ix1, iy1);
		    }
		}
	    }
	  else
	    {
	      for (j = 1, i = 2; i < e->pts * 2; i += 2, j++)
		{
		  ix0 = ix1;
		  iy0 = iy1;
		  ix1 = grf_x(e->data[i], axis_cp->axis);
		  iy1 = grf_y(e->data[i + 1], axis_cp->axis);
		  if (dots)
		    {
		      set_current_point(j, ix1, iy1);
		      draw_arc(ax, ix1, iy1, size);
		    }
		  draw_line(ax, ix0, iy0, ix1, iy1);
		  if (axis_cp->printing) ps_draw_line(axis_cp, ix0, iy0, ix1, iy1);
		}
	    }
	}
      else
	{
	  if (e->base <= 0.0)
	    {
	      for (j = 1, i = 2; i < e->pts * 2; i += 2, j++)
		{
		  ix0 = ix1;
		  iy0 = iy1;
		  ix1 = grf_x(e->data[i], axis_cp->axis);
		  iy1 = grf_y_dB(ss, e->data[i + 1], axis_cp->axis);
		  if (dots)
		    {
		      set_current_point(j, ix1, iy1);
		      draw_arc(ax, ix1, iy1, size);
		    }
		  draw_line(ax, ix0, iy0, ix1, iy0);
		  draw_line(ax, ix1, iy0, ix1, iy1);
		  if (axis_cp->printing) 
		    {
		      ps_draw_line(axis_cp, ix0, iy0, ix1, iy0);
		      ps_draw_line(axis_cp, ix1, iy0, ix1, iy1);
		    }
		}
	    }
	  else
	    {
	      ce = mus_make_env(e->data, e->pts, 1.0, 0.0, e->base, 0.0, 0, width / EXP_SEGLEN - 1, NULL);
	      /* exponential case */
	      dur = width / EXP_SEGLEN;
	      if (dur < e->pts) dur = e->pts;
	      env_val = mus_env(ce);
	      ix1 = grf_x(0.0, axis_cp->axis);
	      iy1 = grf_y_dB(ss, env_val, axis_cp->axis);
	      xincr = (ex1 - ex0) / (Float)dur;
	      j = 1;
	      for (i = 1, curx = ex0; i < dur; i++, curx += xincr)
		{
		  iy0 = iy1;
		  ix0 = ix1;
		  env_val = mus_env(ce);
		  ix1 = grf_x(curx, axis_cp->axis);
		  iy1 = grf_y_dB(ss, env_val, axis_cp->axis);
		  draw_line(ax, ix0, iy0, ix1, iy1);
		  if (axis_cp->printing) ps_draw_line(axis_cp, ix0, iy0, ix1, iy1);
		  if ((dots) && (index != mus_position(ce)))
		    {
		      set_current_point(j++, ix1, iy1);
		      draw_arc(ax, ix1, iy1, size);
		      index = mus_position(ce);
		    }
		}
	      if (curx < ex1)
		{
		  iy0 = iy1;
		  ix0 = ix1;
		  ix1 = grf_x(ex1, axis_cp->axis);
		  iy1 = grf_y_dB(ss, e->data[e->pts * 2 - 1], axis_cp->axis);
		  draw_line(ax, ix0, iy0, ix1, iy1);
		  if (axis_cp->printing) ps_draw_line(axis_cp, ix0, iy0, ix1, iy1);
		}
	      if (dots)
		{
		  set_current_point(j++, ix1, iy1);
		  draw_arc(ax, ix1, iy1, size);
		}
	      mus_free(ce);
	    }
	}
    }
}

void view_envs(snd_state *ss, int env_window_width, int env_window_height)
{
  /* divide space available into a grid (if needed) that shows all currently defined envelopes */
  /* I suppose if there were several hundred envelopes, we'd need a scrollable viewer... */
  int cols, rows, i, j, width, height, x, y, k;
  if (all_envs_top > 1)
    {
      cols = snd_round(sqrt((Float)(all_envs_top * env_window_width) / (Float)env_window_height));
      rows = snd_round((Float)all_envs_top/(Float)cols);
      if ((rows * cols) < all_envs_top) rows++;
    }
  else
    {
      cols = 1;
      rows = 1;
    }
  width = (int)((Float)env_window_width / (Float)cols);
  height = (int)((Float)env_window_height / (Float)rows);
  k = 0;
  for (i = 0, x = 0; i < cols; i++, x += width)
    for (j = 0, y = 0; j < rows; j++, y += height)
      {
	display_enved_env_with_selection(ss, all_envs[k], all_names[k], x, y, width, height, 0);
	k++;
	if (k == all_envs_top) return;
      }
}

int hit_env(int xe, int ye, int env_window_width, int env_window_height)
{
  int cols, rows, i, j, width, height, x, y, k;
  if (all_envs_top == 0)
    return(-1);
  else
    {
      if (all_envs_top == 1)
	return(0);
      else
	{
	  cols = snd_round(sqrt((Float)(all_envs_top * env_window_width) / (Float)env_window_height));
	  rows = snd_round((Float)all_envs_top / (Float)cols);
	  if ((rows * cols) < all_envs_top) rows++;
	  width = (int)((Float)env_window_width / (Float)cols);
	  height = (int)((Float)env_window_height / (Float)rows);
	  k = 0;
	  for (i = 0, x = width; i < cols; i++, x += width)
	    if (x > xe)
	      for (j = 0, y = height; j < rows; j++, y += height)
		{
		  if (y > ye) return(k);
		  k++;
		}
	    else k += rows;
	}
    }
  return(0);
}

void do_enved_edit(env *new_env)
{
  int i;
  if (env_list_top == env_list_size)
    {
      env_list_size += 16;
      if (env_list)
	{
	  env_list = (env **)REALLOC(env_list, env_list_size * sizeof(env *));
	  for (i = env_list_top; i < env_list_size; i++) env_list[i] = NULL;
	}
      else env_list = (env **)CALLOC(env_list_size, sizeof(env *));
    }
  /* clear out current edit list above this edit */
  for (i = env_list_top; i < env_list_size; i++)
    if (env_list[i]) 
      env_list[i] = free_env(env_list[i]);
  env_list[env_list_top] = copy_env(new_env);
  env_list_top++;
}

void redo_env_edit(void)
{
  if (env_list)
    {
      if ((env_list_top < env_list_size) && 
	  (env_list[env_list_top])) 
	{
	  env_list_top++;
	  set_enved_undo_sensitive(TRUE);
	  set_enved_revert_sensitive(TRUE);
	}
      if ((env_list_top == env_list_size) || 
	  (env_list[env_list_top] == NULL)) 
	set_enved_redo_sensitive(FALSE);
      set_enved_save_sensitive(TRUE);
    }
}

void undo_env_edit(void)
{
  if (env_list)
    {
      if (env_list_top > 0)
	{
	  env_list_top--;
	  set_enved_redo_sensitive(TRUE);
	}
      if (env_list_top == 0)
	{
	  set_enved_undo_sensitive(FALSE);
	  set_enved_revert_sensitive(FALSE);
	}
      set_enved_save_sensitive(TRUE);
    }
}

void revert_env_edit(void)
{
  if (env_list)
    {
      if (env_list_top > 1) 
	env_list_top = 1; 
      else 
	{
	  env_list_top = 0;
	  set_enved_undo_sensitive(FALSE);
	  set_enved_revert_sensitive(FALSE);
	  set_enved_save_sensitive(FALSE);
	}
    }
}

int find_env(char *name)
{ /* -1 upon failure */
  int i;
  if ((all_envs) && (name))
    for (i = 0; i < all_envs_top; i++)
      if ((all_names[i]) &&
	  (strcmp(name, all_names[i]) == 0)) 
	return(i);
  return(-1);
}

int enved_all_envs_top(void) {return(all_envs_top);}
char *enved_all_names(int n) {return(all_names[n]);}
void set_enved_env_list_top(int n) {env_list_top = n;}
env *enved_all_envs(int pos) {return(all_envs[pos]);}

static void add_envelope(snd_state *ss, char *name, env *val)
{
  int i;
  if (all_envs_top == all_envs_size)
    {
      all_envs_size += 16;
      if (all_envs)
	{
	  all_envs = (env **)REALLOC(all_envs, all_envs_size * sizeof(env *));
	  all_names = (char **)REALLOC(all_names, all_envs_size * sizeof(char *));
	  for (i = all_envs_size-16; i < all_envs_size; i++) {all_names[i] = NULL; all_envs[i] = NULL;}
	}
      else
	{
	  all_envs = (env **)CALLOC(all_envs_size, sizeof(env *));
	  all_names = (char **)CALLOC(all_envs_size, sizeof(char *));
	}
    }
  all_envs[all_envs_top] = val;
  if (all_names[all_envs_top]) FREE(all_names[all_envs_top]);
  all_names[all_envs_top] = copy_string(name);
  all_envs_top++;
  if (enved_dialog_is_active())
    {
      set_enved_show_sensitive(TRUE);
      make_scrolled_env_list(ss);
    }
}

void delete_envelope(snd_state *ss, char *name)
{
  int i, pos;
  pos = find_env(name);
  if (pos != -1)
    {
      if (all_names[pos]) FREE(all_names[pos]);
      for (i = pos; i < all_envs_size - 1; i++)
	{
	  all_envs[i] = all_envs[i + 1]; 
	  all_envs[i + 1] = NULL;
	  all_names[i] = all_names[i + 1]; 
	  all_names[i + 1] = NULL;
	}
      all_envs_top--;
      if (enved_dialog_is_active())
	{
	  if (all_envs_top > 0)
	    set_enved_show_sensitive(TRUE);
	  make_scrolled_env_list(ss);
	}
    }
}

void alert_envelope_editor(snd_state *ss, char *name, env *val)
{
  /* whenever an envelope is defined or setf'd, we get notification through this function */
  int i;
  if (val == NULL) return;
  i = find_env(name);
  if (i != -1)
    {
      if (all_envs[i]) free_env(all_envs[i]);
      all_envs[i] = val;
    }
  else add_envelope(ss, name, val);
}


void enved_show_background_waveform(snd_state *ss, chan_info *axis_cp, axis_info *gray_ap, int apply_to_mix, int apply_to_selection)
{
  int samps, srate, pts = 0, id = INVALID_MIX_ID, old_time_graph_type = GRAPH_TIME_ONCE, mixing = 0;
  axis_info *ap, *active_ap = NULL;
  chan_info *active_channel = NULL, *ncp;

  if (!(any_selected_sound(ss))) return;
  set_grf_points(-1, 0, 0, 0); /* this is a kludge to handle one-sided graphs (snd-xchn.c) */
  ap = axis_cp->axis;
  gray_ap->x_axis_x0 = ap->x_axis_x0;
  gray_ap->x_axis_x1 = ap->x_axis_x1;
  gray_ap->y_axis_y0 = ap->y_axis_y0;
  gray_ap->y_axis_y1 = ap->y_axis_y1;
  if ((apply_to_mix) && 
      ((ss->selected_mix != NO_SELECTION) || 
       (mixes() == 1)))
    {
      if (mixes() == 0) return;
      if (ss->selected_mix != NO_SELECTION) 
	id = ss->selected_mix; 
      else
	{
	  id = any_mix_id();
	  if (id != NO_SELECTION) select_mix_from_id(id);
	}
      samps = mix_length(id);
      ncp = mix_channel_from_id(id);
      srate = SND_SRATE(ncp->sound);
      gray_ap->losamp = 0;
      gray_ap->hisamp = samps - 1;
      gray_ap->y0 = -1.0;
      gray_ap->y1 = 1.0;
      gray_ap->x0 = 0.0;
      gray_ap->x1 = (Float)samps / (Float)srate;
      mixing = 1;
    }
  else
    {
      active_channel = current_channel(ss);
      active_ap = active_channel->axis;
      if (apply_to_selection)
	{
	  if (!(selection_is_active())) return;
	  /* show current channel overall view in gray scale */
	  samps = selection_len();
	  srate = selection_srate();
	  gray_ap->losamp = selection_beg(NULL);
	  gray_ap->hisamp = gray_ap->losamp + samps - 1;
	  gray_ap->x0 = (Float)(gray_ap->losamp) / (Float)srate;
	  gray_ap->x1 = (Float)(gray_ap->hisamp) / (Float)srate;
	  gray_ap->y0 = -1.0;
	  gray_ap->y1 = 1.0;
	}
      else
	{
	  /* show current channel overall view in gray scale */
	  samps = current_ed_samples(active_channel);
	  srate = SND_SRATE(active_channel->sound);
	  gray_ap->losamp = 0;
	  gray_ap->hisamp = samps - 1;
	  if (active_channel->time_graph_type == GRAPH_TIME_AS_WAVOGRAM)
	    {
	      gray_ap->y0 = -1.0;
	      gray_ap->y1 = 1.0;
	    }
	  else
	    {
	      gray_ap->y0 = active_ap->y0;
	      gray_ap->y1 = active_ap->y1;
	    }
	  gray_ap->x0 = 0.0;
	  gray_ap->x1 = (Float)samps / (Float)srate;
	}
    }
  gray_ap->x_scale = ((double)(gray_ap->x_axis_x1 - gray_ap->x_axis_x0)) / ((double)(gray_ap->x1 - gray_ap->x0));
  gray_ap->y_scale = (gray_ap->y_axis_y1 - gray_ap->y_axis_y0) / (gray_ap->y1 - gray_ap->y0);
  gray_ap->x_base = (double)(gray_ap->x_axis_x0 - gray_ap->x0 * gray_ap->x_scale);
  gray_ap->y_base = (Float)(gray_ap->y_axis_y0 - gray_ap->y0 * gray_ap->y_scale);
  if (mixing)
    {
      axis_cp->axis = gray_ap;
      pts = display_mix_waveform_at_zero(axis_cp, id);
      axis_cp->axis = ap;
    }
  else
    {
      active_channel->axis = gray_ap;
      old_time_graph_type = active_channel->time_graph_type;
      active_channel->time_graph_type = GRAPH_TIME_ONCE;
      pts = make_graph(active_channel, NULL, ss);
      active_channel->time_graph_type = old_time_graph_type;
      active_channel->axis = active_ap;
    }
  if (pts > 0) draw_both_grfs(gray_ap->ax, pts);
}

int enved_button_press_display(snd_state *ss, axis_info *ap, env *active_env, int evx, int evy)
{
  int pos, env_pos;
  Float x, y;
  pos = hit_point(ss, current_xs, current_ys, active_env->pts, evx, evy);
  x = ungrf_x(ap, evx);
  y = ungrf_y_dB(ss, ap, evy);
  if (enved_clip_p(ss))
    {
      if (y < ap->y0) y = ap->y0;
      if (y > ap->y1) y = ap->y1;
    }
  if (pos == -1)
    {
      if (x < ap->x0)
	{
	  pos = 0;
	  x = ap->x0;
	}
      else 
	if (x > ap->x1) 
	  {
	    pos = active_env->pts - 1;
	    x = ap->x1;
	  }
    }
  env_pos = pos;
  /* if not -1, then user clicked existing point -- wait for drag/release to decide what to do */
  if (pos == -1) 
    {
      pos = place_point(current_xs, active_env->pts, evx);
      /* place returns left point index of current segment or pts if off left end */
      /* in this case, user clicked in middle of segment, so add point there */
      if (check_enved_hook(active_env, pos, x, y, ENVED_ADD_POINT) == 0)
	add_point(active_env, pos + 1, x, y);
      env_pos = pos + 1;
      set_enved_click_to_delete(0);
      env_redisplay(ss);
    }
  else set_enved_click_to_delete(1);
  enved_display_point_label(ss, x, y);
  return(env_pos);
}

env *enved_next_env(void)
{
  if (env_list_top > 0) 
    return(copy_env(env_list[env_list_top - 1])); 
  else return(NULL);
}

char *env_name_completer(char *text)
{
  int i, j, len, curlen, matches = 0;
  char *current_match = NULL;
  if ((all_envs) && (text) && (*text))
    {
      len = strlen(text);
      for (i = 0; i < all_envs_top; i++)
	if (strncmp(text, all_names[i], len) == 0)
	  {
	    matches++;
	    add_possible_completion(all_names[i]);
	    if (current_match == NULL)
	      current_match = copy_string(all_names[i]);
	    else 
	      {
		curlen = strlen(current_match);
		for (j = 0; j < curlen; j++)
		  if (current_match[j] != all_names[i][j])
		    {
		      current_match[j] = '\0';
		      break;
		    }
	      }
	  }
    }
  set_completion_matches(matches);
  if ((current_match) && (*current_match))
    return(current_match);
  return(copy_string(text));
}

void save_envelope_editor_state(FILE *fd)
{
  char *estr;
  int i;
  for (i = 0; i < all_envs_top; i++)
    {
      estr = env_to_string(all_envs[i]);
      if (estr)
	{
	  fprintf(fd, "(if (not (defined? '%s)) (defvar %s %s))", all_names[i], all_names[i], estr);
	  /* or...
	   *   perhaps this should set! a currently defined envelope back to its state upon save?
	   *   I'm not sure how people want to use this feature.
	   */
	  if (all_envs[i]->base != 1.0)
	    fprintf(fd, " (set! (env-base \"%s\") %.4f)", 
		    all_names[i], all_envs[i]->base);
	  fprintf(fd, "\n");
	  FREE(estr);
	}
    }
}

env *scm2env(SCM res)
{
  SCM el, lst;
  int i, len = 0;
  Float *data;
  env *rtn = NULL;
  if (LIST_P_WITH_LENGTH(res, len))
    {
      if (len > 0)
	{
	  data = (Float *)CALLOC(len, sizeof(Float));
	  for (i = 0, lst = res; i < len; i++, lst = SCM_CDR(lst))
	    {
	      el = SCM_CAR(lst);
	      if (NUMBER_P(el))
		data[i] = TO_C_DOUBLE(el);
	      else data[i] = 0.0;
	    }
	  rtn = make_envelope(data, len);
	  FREE(data);
	}
      return(rtn);
    }
  return(NULL);
}

static int x_increases(SCM res)
{
  int i, len;
  SCM lst;
  Float x, nx;
  len = LIST_LENGTH(res);
  x = TO_C_DOUBLE(SCM_CAR(res));
  for (i = 2, lst = SCM_CDDR(res); i < len; i += 2, lst = SCM_CDDR(lst))
    {
      nx = TO_C_DOUBLE(SCM_CAR(lst));
      if (x >= nx) return(0);
      x = nx;
    }
  return(1);
}

/* these make it possible for the user to type names or expressions wherever a value is possible */
env *string2env(char *str) 
{
  SCM res;
  int len;
  res = snd_catch_any(eval_str_wrapper, str, "string->env");
  if (LIST_P_WITH_LENGTH(res, len))
    {
      if ((len % 2) == 0)
	{
	  if (x_increases(res))
	    return(scm2env(res));
	  else snd_error("x axis points not increasing: %s", str);
	}
      else snd_error("odd length envelope? %s", str);
    }
  else snd_error("%s is not a list", str);
  return(NULL);
}

env *name_to_env(char *str)
{
  /* called (at user interface level) to see if str is a known envelope -- return its current value or nil if unknown */
  /* get str as list var and turn into env */
  return(scm2env(SND_LOOKUP(str)));
}

static SCM g_define_envelope(SCM a, SCM b)
{
  #define H_define_envelope "(" S_define_envelope " name data) defines 'name' to be the envelope 'data', a list of breakpoints"
  ASSERT_TYPE(STRING_P(a), a, SCM_ARG1, S_define_envelope, "a string");
  if (LIST_P(b)) 
    alert_envelope_editor(get_global_state(), 
			  TO_C_STRING(a), 
			  scm2env(b));
  return(SCM_BOOL_F);
}

static SCM array_to_list(Float *arr, int i, int len)
{
  if (i < (len - 1))
    return(CONS(TO_SCM_DOUBLE(arr[i]), 
		   array_to_list(arr, i + 1, len)));
  else return(CONS(TO_SCM_DOUBLE(arr[i]), 
		      SCM_EOL));
}

SCM env2scm (env *e)
{
  if (e) 
    return(array_to_list(e->data, 0, e->pts * 2));
  return(SCM_EOL);
}

void add_or_edit_symbol(char *name, env *val)
{
  /* called from envelope editor -- pass new definition into scheme */
  SCM e;
  char *buf, *tmpstr = NULL;
  buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
  e = SND_LOOKUP(name);
  if ((BOUND_P(e)) && (LIST_P(e)))
    mus_snprintf(buf, PRINT_BUFFER_SIZE, "(set! %s %s)", 
	    name, 
	    tmpstr = env_to_string(val));
  else mus_snprintf(buf, PRINT_BUFFER_SIZE, "(define %s %s)", 
	       name, 
	       tmpstr = env_to_string(val));
  snd_catch_any(eval_str_wrapper, buf, buf);
  FREE(buf);
  if (tmpstr) FREE(tmpstr);
}

env *get_env(SCM e, SCM base, char *origin) /* list or vector in e */
{
  Float *buf = NULL;
  int i, len = 0;
  env *newenv = NULL;
  SCM *vdata;
  SCM lst;
  ASSERT_TYPE(((VECTOR_P(e)) || (LIST_P_WITH_LENGTH(e, len))), e, SCM_ARG1, origin, "a vector or a list");
  if (VECTOR_P(e))
    {
      len = VECTOR_LENGTH(e);
      if (len == 0)
	mus_misc_error(origin, "null env", e);
      buf = (Float *)CALLOC(len, sizeof(Float));
      vdata = SCM_VELTS(e);
      for (i = 0; i < len; i++) 
	buf[i] = TO_C_DOUBLE(vdata[i]);
    }
  else
    {
      if (len == 0)
	mus_misc_error(origin, "null env", e);
      buf = (Float *)CALLOC(len, sizeof(Float));
      for (i = 0, lst = e; i < len; i++, lst = SCM_CDR(lst)) 
	buf[i] = TO_C_DOUBLE(SCM_CAR(lst));
    }
  newenv = make_envelope(buf, len);
  if (NUMBER_P(base)) 
    newenv->base = TO_C_DOUBLE(base); 
  else newenv->base = 1.0;
  if (buf) FREE(buf);
  return(newenv);
}

static SCM g_save_envelopes(SCM filename)
{
  #define H_save_envelopes "(" S_save_envelopes " filename) saves the envelopes known to the envelope editor in filename"
  char *name = NULL;
  FILE *fd;
  ASSERT_TYPE((STRING_P(filename) || (FALSE_P(filename)) || (NOT_BOUND_P(filename))), filename, SCM_ARGn, S_save_envelopes, "a string or #f");
  if (STRING_P(filename)) 
    name = mus_expand_filename(TO_C_STRING(filename));
  else name = copy_string("envs.save");
  fd = fopen(name, "w");
  if (fd) save_envelope_editor_state(fd);
  if (name) FREE(name);
  if ((!fd) || (fclose(fd) != 0))
    ERROR(CANNOT_SAVE,
	  SCM_LIST3(TO_SCM_STRING(S_save_envelopes),
		    filename,
		    TO_SCM_STRING(strerror(errno))));
  return(filename);
}

static SCM enved_hook;

int check_enved_hook(env *e, int pos, Float x, Float y, int reason)
{
  SCM result = SCM_BOOL_F;
  SCM procs, env_list;
  int env_changed = 0, len = 0;
  if (HOOKED(enved_hook))
    {
      /* if hook procedure returns a list, that is the new contents of the
       * envelope -- if its length doesn't match current, we need to remake
       * current. Otherwise return 0, and assume the caller will handle default
       */
      procs = SCM_HOOK_PROCEDURES (enved_hook);
      env_list = env2scm(e);
      while (NOT_NULL_P(procs))
	{
	  result = APPLY(SCM_CAR(procs), 
			 SCM_LIST5(env_list,
				   TO_SMALL_SCM_INT(pos),
				   TO_SCM_DOUBLE(x),
				   TO_SCM_DOUBLE(y),
				   TO_SMALL_SCM_INT(reason)),
			 S_enved_hook);
	  procs = SCM_CDR (procs);
	  if ((NOT_FALSE_P(result)) && 
	      (LIST_P_WITH_LENGTH(result, len)))
	    {
	      /* remake env and (if not null procs) env_list */
	      /* each successive hook procedure gets the on-going (changing) envelope */
	      int i;
	      SCM lst;
	      if (len > e->data_size)
		{
		  FREE(e->data);
		  e->data = (Float *)CALLOC(len, sizeof(Float));
		  e->data_size = len;
		}
	      e->pts = len / 2;
	      for (i = 0, lst = result; i < len; i++, lst = SCM_CDR(lst))
		e->data[i] = TO_C_DOUBLE(SCM_CAR(lst));
	      if (NOT_NULL_P(procs))
		env_list = env2scm(e);
	      env_changed = 1;
	    }
	}
    }
  return(env_changed); /* 0 = default action */
}

static SCM g_enved_base(void) {return(TO_SCM_DOUBLE(enved_base(get_global_state())));}
static SCM g_set_enved_base(SCM val) 
{
  #define H_enved_base "(" S_enved_base ") -> envelope editor exponential base value (1.0)"
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARGn, "set-" S_enved_base, "a number"); 
  set_enved_base(get_global_state(), mus_fclamp(0.0, TO_C_DOUBLE(val), 300000.0));
  return(TO_SCM_DOUBLE(enved_base(get_global_state())));
}

static SCM g_enved_power(void) {return(TO_SCM_DOUBLE(enved_power(get_global_state())));}
static SCM g_set_enved_power(SCM val) 
{
  #define H_enved_power "(" S_enved_power ") -> envelope editor base scale range (9.0^power)"
  ASSERT_TYPE(NUMBER_P(val), val, SCM_ARGn, "set-" S_enved_power, "a number"); 
  set_enved_power(get_global_state(), mus_fclamp(0.0, TO_C_DOUBLE(val), 10.0));
  return(TO_SCM_DOUBLE(enved_power(get_global_state())));
}

static SCM g_enved_clip_p(void) {return(TO_SCM_BOOLEAN(enved_clip_p(get_global_state())));}
static SCM g_set_enved_clip_p(SCM on)
{
  #define H_enved_clip_p "(" S_enved_clip_p ") -> envelope editor 'clip' button setting; \
if clipping, the motion of the mouse is restricted to the current graph bounds."

  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(on), on, SCM_ARGn, "set-" S_enved_clip_p, "a boolean");
  set_enved_clip_p(get_global_state(), TO_C_BOOLEAN_OR_T(on)); 
  return(TO_SCM_BOOLEAN(enved_clip_p(get_global_state())));
}

static SCM g_enved_exp_p(void) {return(TO_SCM_BOOLEAN(enved_exp_p(get_global_state())));}
static SCM g_set_enved_exp_p(SCM val) 
{
  #define H_enved_exp_p "(" S_enved_exp_p ") -> envelope editor 'exp' and 'lin' buttons; \
if enved-exping, the connecting segments use exponential curves rather than straight lines."

  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(val), val, SCM_ARGn, "set-" S_enved_exp_p, "a boolean");
  set_enved_exp_p(get_global_state(), TO_C_BOOLEAN_OR_T(val)); 
  return(TO_SCM_BOOLEAN(enved_clip_p(get_global_state())));
}

static SCM g_enved_target(void) {return(TO_SCM_INT(enved_target(get_global_state())));}
static SCM g_set_enved_target(SCM val) 
{
  int n; 
  #define H_enved_target "(" S_enved_target ") determines how the envelope is applied to data in the envelope editor; \
choices are " S_enved_amplitude ", " S_enved_srate ", and " S_enved_spectrum

  ASSERT_TYPE(INTEGER_P(val), val, SCM_ARGn, "set-" S_enved_target, "an integer"); 
  n = mus_iclamp(ENVED_AMPLITUDE,
	     TO_C_INT(val),
	     ENVED_SRATE); 
  set_enved_target(get_global_state(), n); 
  return(TO_SCM_INT(enved_target(get_global_state())));
}

static SCM g_enved_wave_p(void) {return(TO_SCM_BOOLEAN(enved_wave_p(get_global_state())));}
static SCM g_set_enved_wave_p(SCM val) 
{
  #define H_enved_wave_p "(" S_enved_wave_p ") -> #t if the envelope editor is displaying the waveform to be edited"
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(val), val, SCM_ARGn, "set-" S_enved_wave_p, "a boolean");
  set_enved_wave_p(get_global_state(), TO_C_BOOLEAN_OR_T(val));
  return(TO_SCM_BOOLEAN(enved_wave_p(get_global_state())));
}

static SCM g_enved_in_dB(void) {return(TO_SCM_BOOLEAN(enved_in_dB(get_global_state())));}
static SCM g_set_enved_in_dB(SCM val) 
{
  #define H_enved_in_dB "(" S_enved_in_dB ") -> #t if the envelope editor is using dB"
  ASSERT_TYPE(BOOLEAN_IF_BOUND_P(val), val, SCM_ARGn, "set-" S_enved_in_dB, "a boolean");
  set_enved_in_dB(get_global_state(), TO_C_BOOLEAN_OR_T(val)); 
  return(TO_SCM_BOOLEAN(enved_in_dB(get_global_state())));
}

static SCM g_enved_filter_order(void) {return(TO_SCM_INT(enved_filter_order(get_global_state())));}
static SCM g_set_enved_filter_order(SCM val) 
{
  #define H_enved_filter_order "(" S_enved_filter_order ") -> envelope editor's FIR filter order (40)"
  ASSERT_TYPE(INTEGER_P(val), val, SCM_ARGn, "set-" S_enved_filter_order, "an integer"); 
  set_enved_filter_order(get_global_state(), TO_C_INT(val));
  return(TO_SCM_INT(enved_filter_order(get_global_state())));
}

static SCM g_enved_dialog(void) 
{
  #define H_enved_dialog "(" S_enved_dialog ") fires up the Envelope Editor"
  return(SND_WRAP(create_envelope_editor(get_global_state()))); 
}


void g_init_env(SCM local_doc)
{
  #define H_enved_amplitude "The value for " S_enved_target " that sets the envelope editor 'amp' button."
  #define H_enved_spectrum "The value for " S_enved_target " that sets the envelope editor 'flt' button."
  #define H_enved_srate "The value for " S_enved_target " that sets the envelope editor 'src' button."

  DEFINE_VAR(S_enved_amplitude,       ENVED_AMPLITUDE, H_enved_amplitude);
  DEFINE_VAR(S_enved_spectrum,        ENVED_SPECTRUM,  H_enved_spectrum);
  DEFINE_VAR(S_enved_srate,           ENVED_SRATE,     H_enved_srate);

  define_procedure_with_setter(S_enved_base, SCM_FNC g_enved_base, H_enved_base,
			       "set-" S_enved_base, SCM_FNC g_set_enved_base, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_enved_power, SCM_FNC g_enved_power, H_enved_power,
			       "set-" S_enved_power, SCM_FNC g_set_enved_power, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_enved_clip_p, SCM_FNC g_enved_clip_p, H_enved_clip_p,
			       "set-" S_enved_clip_p, SCM_FNC g_set_enved_clip_p, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_enved_exp_p, SCM_FNC g_enved_exp_p, H_enved_exp_p,
			       "set-" S_enved_exp_p, SCM_FNC g_set_enved_exp_p, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_enved_target, SCM_FNC g_enved_target, H_enved_target,
			       "set-" S_enved_target, SCM_FNC g_set_enved_target, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_enved_wave_p, SCM_FNC g_enved_wave_p, H_enved_wave_p,
			       "set-" S_enved_wave_p, SCM_FNC g_set_enved_wave_p, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_enved_in_dB, SCM_FNC g_enved_in_dB, H_enved_in_dB,
			       "set-" S_enved_in_dB, SCM_FNC g_set_enved_in_dB, local_doc, 0, 0, 0, 1);

  define_procedure_with_setter(S_enved_filter_order, SCM_FNC g_enved_filter_order, H_enved_filter_order,
			       "set-" S_enved_filter_order, SCM_FNC g_set_enved_filter_order, local_doc, 0, 0, 0, 1);

  DEFINE_PROC(S_enved_dialog,    g_enved_dialog, 0, 0, 0,     H_enved_dialog);
  DEFINE_PROC(S_save_envelopes,  g_save_envelopes, 0, 1, 0,   H_save_envelopes);
  DEFINE_PROC(S_define_envelope, g_define_envelope, 2, 0, 0,  H_define_envelope);

  DEFINE_VAR(S_enved_add_point,    ENVED_ADD_POINT,    S_enved_hook " 'reason' arg when point is added");
  DEFINE_VAR(S_enved_delete_point, ENVED_DELETE_POINT, S_enved_hook " 'reason' arg when point is deleted");
  DEFINE_VAR(S_enved_move_point,   ENVED_MOVE_POINT,   S_enved_hook " 'reason' arg when point is moved");

  #define H_enved_hook S_enved_hook " (env pt new-x new-y reason)\n\
Each time a breakpoint is changed in the envelope editor, this hook \
is called; if it returns a list, that list defines the new envelope, \
otherwise the breakpoint is moved (but not beyond the neighboring \
breakpoint), leaving other points untouched.  The kind of change \
is 'reason' which can be enved-point-moved, enved-point-deleted, \
or enved-point-added.  This hook makes it possible to define attack \
and decay portions in the envelope editor, or use functions such as \
stretch-envelope from env.scm: \n\
 (add-hook! enved-hook\n\
   (lambda (env pt x y reason)\n\
     (if (= reason enved-move-point)\n\
         (let* ((old-x (list-ref env (* pt 2)))\n\
                (new-env (stretch-envelope env old-x x)))\n\
           (list-set! new-env (+ (* pt 2) 1) y)\n\
           new-env)\n\
         #f)))"

  enved_hook = MAKE_HOOK(S_enved_hook, 5, H_enved_hook);
}
