#include "snd.h"

Float un_dB(Float py)
{
  /* used only by envelope editor (snd-xenv etc) */
  return((py <= ss->min_dB) ? 0.0 : pow(10.0, py * .05));
}

/* mus_free(e) */
env *free_env(env *e)
{
  if (e)
    {
      if (e->data) FREE(e->data);
      FREE(e);
    }
  return(NULL);
}

/* mus_copy(e) ideally */
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
      return(ne);
    }
  return(NULL);
}

bool envs_equal(env *e1, env *e2)
{
  int i;
  if ((e1 == NULL) || (e2 == NULL)) return(false);
  if (e1->pts != e2->pts) return(false);
  for (i = 0; i < e1->pts * 2; i++)
    if (e1->data[i] != e2->data[i])
      return(false);
  return(true);
}

/* mus_describe(e) */
char *env_to_string(env *e)
{
  int i, j, len;
  bool first = true;
  char *expr_buf;
  char *news = NULL;
  if (e)
    {
      len = 4 + (e->pts * 2 * 16);
      news = (char *)CALLOC(len, sizeof(char));
#if HAVE_RUBY
      news[0] = '[';
#else
      news[0] = '\'';
      news[1] = '(';
#endif
      expr_buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      for (i = 0, j = 0; i < e->pts; i++, j += 2)
	{
#if HAVE_RUBY
	  mus_snprintf(expr_buf, PRINT_BUFFER_SIZE, "%s%.3f, %.3f", (first) ? "" : ", ", e->data[j], e->data[j + 1]);
#else
	  mus_snprintf(expr_buf, PRINT_BUFFER_SIZE, "%s%.3f %.3f", (first) ? "" : " ", e->data[j], e->data[j + 1]);
#endif
	  news = snd_strcat(news, expr_buf, &len);
	  first = false;
	}
      FREE(expr_buf);
#if HAVE_RUBY
      news = snd_strcat(news, "]", &len);
#else
      news = snd_strcat(news, ")", &len);
#endif
    }
  else
    {
#if HAVE_RUBY
      news = copy_string("false");
#else
      news = copy_string("#f");
#endif
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
  return(e);
}

Float interp_env(env *e, Float x)
{
  int i;
  if (e)
    {
      if ((e->pts == 1) || (x <= e->data[0])) return(e->data[1]);
      if (x >= e->data[e->data_size - 2]) return(e->data[e->data_size - 1]);
      for (i = 0; i < e->data_size - 1; i += 2)
	{
	  if (e->data[i] == x)
	    return(e->data[i + 1]);
	  else
	    {
	      if (e->data[i + 2] > x)
		return(e->data[i + 1] + 
		       (e->data[i + 3] - e->data[i + 1]) * (x - e->data[i]) / (e->data[i + 2] - e->data[i]));
	    }
	}
    }
  return(0.0);
}

#if DEBUGGING && HAVE_GUILE
static XEN g_interp_env(XEN e, XEN val)
{
  Float y;
  env *temp;
  temp = xen_to_env(e);
  y = interp_env(temp, XEN_TO_C_DOUBLE(val));
  temp = free_env(temp);
  return(C_TO_XEN_DOUBLE(y));
}
#endif

env *normalize_x_axis(env *e)
{
  /* make sure env goes from 0 to 1 x-wise; e is changed */
  Float x0, x1;
  int i;
  if (!e) return(NULL);
  x0 = e->data[0];
  x1 = e->data[e->data_size - 2];
  if ((x0 == 0.0) && (x1 == 1.0))
    return(e);
  if ((e->pts == 1) || (x0 == x1))
    {
      e->data[0] = 0.0;
      return(e);
    }
  for (i = 0; i < e->data_size - 1; i += 2)
    e->data[i] = (e->data[i] - x0) / (x1 - x0);
  return(e);
}

env *window_env(env *e, off_t local_beg, off_t local_dur, off_t e_beg, off_t e_dur)
{
  env *local_e = NULL;
  int i, j = 0;
  Float x0, x1, e_x_range, e_x0, e_x1, local_x0, local_x1;
  /* return env representing e from local beg for local dur */
  if ((local_beg == e_beg) && (local_dur == e_dur))
    return(normalize_x_axis(copy_env(e))); /* fixup 0..1? */
  /* only need to interpolate start and end breaks, copy in between */
  x0 = (double)(local_beg - e_beg) / (double)e_dur;
  x1 = (double)(local_beg + local_dur - e_beg) / (double)e_dur;
  e_x0 = e->data[0];
  e_x1 = e->data[e->data_size - 2];
  e_x_range = e_x1 - e_x0;
  local_x0 = e_x0 + x0 * e_x_range;
  local_x1 = e_x0 + x1 * e_x_range;
  local_e = (env *)CALLOC(1, sizeof(env));
  local_e->data = (Float *)CALLOC(e->data_size, sizeof(Float));
  /* this may leave wasted space, but worst case the two envs are the same size */
  for (i = 0; i < e->data_size - 1; i += 2)
    {
      if (e->data[i] >= local_x0)
	{
	  if (j == 0)
	    {
	      if (e->data[i] == local_x0)
		{
		  local_e->data[0] = local_x0;
		  local_e->data[1] = e->data[i + 1];
		}
	      else
		{
		  local_e->data[0] = local_x0;
		  local_e->data[1] = e->data[i - 1] + (e->data[i + 1] - e->data[i - 1]) * (local_x0 - e->data[i - 2]) / (e->data[i] - e->data[i - 2]);
		}
	      j = 2;
	    }
	  if ((e->data[i] < local_x1) && (e->data[i] != local_x0))
	    {
	      local_e->data[j] = e->data[i];
	      local_e->data[j + 1] = e->data[i + 1];
	      if (e->data[i] == local_x1) break; /* hit end point */
	      j += 2;
	    }
	}
      if (e->data[i] >= local_x1)
	{
	  local_e->data[j] = local_x1;
	  local_e->data[j + 1] = e->data[i - 1] + (e->data[i + 1] - e->data[i - 1]) * (local_x1 - e->data[i - 2]) / (e->data[i] - e->data[i - 2]);
	  j += 2;
	  break;
	}
    }
  if (j == 0)
    return(free_env(local_e));
  local_e->pts = j / 2;
  local_e->data_size = j;
  return(normalize_x_axis(local_e));
}

#if DEBUGGING && HAVE_GUILE
static XEN g_window_env(XEN e, XEN b1, XEN d1, XEN b2, XEN d2)
{
  XEN res;
  env *temp1 = NULL, *temp2 = NULL;
  temp1 = xen_to_env(e);
  temp2 = window_env(temp1,
		     XEN_TO_C_OFF_T(b1),
		     XEN_TO_C_OFF_T(d1),
		     XEN_TO_C_OFF_T(b2),
		     XEN_TO_C_OFF_T(d2));
  res = env_to_xen(temp2);
  temp1 = free_env(temp1);
  temp2 = free_env(temp2);
  return(res);
}
#endif

env *multiply_envs(env *e1, env *e2, Float maxx)
{
  /* assume at this point that x axes are 0..1 */
  int i = 2, j = 2, k = 2;
  Float x = 0.0, y0, y1;
  env *e = NULL;
  e = (env *)CALLOC(1, sizeof(env));
  e->data_size = (e1->pts + e2->pts) * 2;
  e->data = (Float *)CALLOC(e->data_size, sizeof(Float));
  e->data[0] = 0.0;
  e->data[1] = e1->data[1] * e2->data[1];
  while (true)
    {
      x += maxx;
      if (x >= e1->data[i]) x = e1->data[i];
      if (x >= e2->data[j]) x = e2->data[j];
      if (k >= e->data_size)
	{
	  e->data_size += 16;
	  e->data = (Float *)REALLOC(e->data, e->data_size * sizeof(Float));
	}
      if (x == e1->data[i])
	{
	  y0 = e1->data[i + 1];
	  if (i < e1->data_size - 1) i += 2;
	}
      else
	y0 = e1->data[i - 1] + (x - e1->data[i - 2]) * (e1->data[i + 1] - e1->data[i - 1]) / (e1->data[i] - e1->data[i - 2]);
      
      if (x == e2->data[j])
	{
	  y1 = e2->data[j + 1];
	  if (j < e2->data_size - 1) j += 2;
	}
      else
	y1 = e2->data[j - 1] + (x - e2->data[j - 2]) * (e2->data[j + 1] - e2->data[j - 1]) / (e2->data[j] - e2->data[j - 2]);

      e->data[k] = x;
      e->data[k + 1] = y0 * y1;
      k += 2;
      if (x >= 1.0) break;
    }
  if (k == 0)
    return(free_env(e));
  e->data_size = k;
  e->pts = k / 2;
  return(e);
}

#if DEBUGGING && HAVE_GUILE
static XEN g_multiply_envs(XEN e1, XEN e2, XEN maxx)
{
  XEN res;
  env *temp1 = NULL, *temp2 = NULL, *temp3 = NULL;
  temp1 = xen_to_env(e1);
  temp2 = xen_to_env(e2);
  temp3 = multiply_envs(temp1, temp2, XEN_TO_C_DOUBLE(maxx));
  res = env_to_xen(temp3);
  temp1 = free_env(temp1);
  temp2 = free_env(temp2);
  temp3 = free_env(temp3);
  return(res);
}
#endif

env *invert_env(env *e)
{
  env *new_e;
  int i;
  new_e = copy_env(e);
  for (i = 1; i < new_e->data_size; i += 2)
    new_e->data[i] = 1.0 - new_e->data[i];
  return(new_e);
}

#if DEBUGGING && HAVE_GUILE
static XEN g_invert_env(XEN e)
{
  env *temp1, *temp2;
  XEN res;
  temp1 = xen_to_env(e);
  temp2 = invert_env(temp1);
  res = env_to_xen(temp2);
  free_env(temp1);
  free_env(temp2);
  return(res);
}
#endif


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
    }
  e->data[pos * 2] = x;
  e->data[pos * 2 + 1] = y;
  e->pts++;
}

void move_point(env *e, int pos, Float x, Float y)
{
  e->data[pos * 2] = x;
  e->data[pos * 2 + 1] = y;
}

void delete_point(env *e, int pos)
{
  int i, j;
  for (i = pos, j = pos * 2; i < e->pts - 1; i++, j += 2)
    {
      e->data[j] = e->data[j + 2];
      e->data[j + 1] = e->data[j + 3];
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

static int hit_point(int *cxs, int *cys, int points, int x, int y)
{
  int i;
  for (i = 0; i < points; i++)
    if ((x == cxs[i]) ||
	(((x > (cxs[i] - ss->enved_point_size)) && 
	  (x < (cxs[i] + ss->enved_point_size))) &&
	 ((y > (cys[i] - ss->enved_point_size)) && 
	  (y < (cys[i] + ss->enved_point_size)))))
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
  return(e);
}


/* -------- FILTER/MIX ENVELOPE -------- */

typedef struct {
  int *current_xs;
  int *current_ys;
  int current_size;
  axis_info *axis;
  Tempus down_time;
  bool env_dragged;
  int env_pos;
  bool click_to_delete;
  bool edited;
} env_ed;

void *new_env_editor(void)
{
  env_ed *edp;
  edp = (env_ed *)CALLOC(1, sizeof(env_ed));
  edp->current_xs = (int *)CALLOC(8, sizeof(int));
  edp->current_ys = (int *)CALLOC(8, sizeof(int));
  edp->axis = (axis_info *)CALLOC(1, sizeof(axis_info));
  edp->current_size = 8;
  edp->env_dragged = false;
  edp->env_pos = 0;
  edp->click_to_delete = false;
  edp->edited = false;
  return((void *)edp);
}

void edp_reset(void *spf)
{
  env_ed *edp = (env_ed *)spf;
  if (edp)
    {
      edp->edited = false;
      edp->env_dragged = false;
      edp->env_pos = 0;
      edp->click_to_delete = false;
    }
}

static void edp_set_current_point(env_ed *edp, int pos, int x, int y)
{
  if (pos == edp->current_size)
    {
      edp->current_size += 8;
      edp->current_xs = (int *)REALLOC(edp->current_xs, edp->current_size * sizeof(int));
      edp->current_ys = (int *)REALLOC(edp->current_ys, edp->current_size * sizeof(int));
    }
  edp->current_xs[pos] = x;
  edp->current_ys[pos] = y;
}

static short edp_grf_y_dB(Float val, axis_info *ap, bool use_dB)
{
  if (use_dB)
    return(grf_y(in_dB(ss->min_dB, ss->lin_dB, val), ap));
  else return(grf_y(val, ap));
}

static double edp_ungrf_y_dB(axis_info *ap, int y, bool use_dB)
{
  if (use_dB)
    return(un_dB(ungrf_y(ap, y)));
  else return(ungrf_y(ap, y));
}

axis_info *edp_ap(void *spf)
{
  return(((env_ed *)spf)->axis);
}

#define EXP_SEGLEN 4
#define MIN_FILTER_GRAPH_HEIGHT 20

bool edp_display_graph(void *spf, const char *name, axis_context *ax, 
		       int x, int y, int width, int height, env *e, bool use_dB, bool with_dots)
{
  axis_info *ap;
  env_ed *edp = (env_ed *)spf;
  int i, j, k;
  Float ex0, ey0, ex1, ey1, val;
  int ix0, ix1, iy0, iy1, size = 4, lx0, lx1, ly0, ly1;
  Float curx, xincr;
  int dur;
  if ((e == NULL) || (edp == NULL)) return(false);
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
  if (use_dB) 
    {
      ey0 = ss->min_dB; 
      ey1 = 0.0;
    }
  ap = edp->axis;
  ap->ax = ax;
  if (with_dots)
    init_env_axes(ap, name, x, y, width, height, ex0, ex1, ey0, ey1, false);
  ix1 = grf_x(e->data[0], ap);
  iy1 = edp_grf_y_dB(e->data[1], ap, use_dB);
  if (with_dots)
    {
      if (e->pts < 100)
	size = ss->enved_point_size;
      else size = (int)(ss->enved_point_size * 0.4);
      edp_set_current_point(edp, 0, ix1, iy1);
      draw_arc(ax, ix1, iy1, size);
    }
  if (use_dB)
    {
      for (j = 1, i = 2; i < e->pts * 2; i += 2, j++)
	{
	  ix0 = ix1;
	  iy0 = iy1;
	  ix1 = grf_x(e->data[i], ap);
	  iy1 = edp_grf_y_dB(e->data[i + 1], ap, use_dB);
	  if (with_dots)
	    {
	      edp_set_current_point(edp, j, ix1, iy1);
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
	      Float yval, yincr;
	      dur = (ix1 - ix0) / EXP_SEGLEN;
	      xincr = (e->data[i] - e->data[i - 2]) / (Float)dur;
	      curx = e->data[i - 2] + xincr;
	      lx1 = ix0;
	      ly1 = iy0;
	      yval = e->data[i - 1];
	      yincr = (e->data[i + 1] - yval) / (Float)dur;
	      yval += yincr;
	      for (k = 1; k < dur; k++, curx += xincr, yval += yincr)
		{
		  lx0 = lx1;
		  ly0 = ly1;
		  lx1 = grf_x(curx, ap);
		  ly1 = grf_y(in_dB(ss->min_dB, ss->lin_dB, yval), ap);
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
	  if (with_dots)
	    {
	      edp_set_current_point(edp, j, ix1, iy1);
	      draw_arc(ax, ix1, iy1, size);
	    }
	  draw_line(ax, ix0, iy0, ix1, iy1);
	}
    }
  return(edp->edited);
}

void edp_handle_point(void *spf, int evx, int evy, Tempus motion_time, env *e, bool use_dB, Float xmax)
{
  env_ed *edp = (env_ed *)spf;
  axis_info *ap;
  Float x0, x1, x, y;
  if ((e == NULL) || (edp == NULL)) return;
  if ((motion_time - edp->down_time) < 100) return;
  edp->env_dragged = true;
  edp->click_to_delete = false;
  ap = edp->axis;
  x = ungrf_x(ap, evx);
  if (edp->env_pos > 0) 
    x0 = e->data[edp->env_pos * 2 - 2]; 
  else x0 = 0.0;
  if (edp->env_pos < e->pts) 
    x1 = e->data[edp->env_pos * 2 + 2]; 
  else x1 = xmax; /* x1 = 1.0; */
  if (x < x0) x = x0;
  if (x > x1) x = x1;
  if (edp->env_pos == 0) x = e->data[0];
  if (edp->env_pos == (e->pts - 1)) x = e->data[(e->pts - 1) * 2];
  y = ungrf_y(ap, evy);
  if (y < 0.0) y = 0.0;
  if (y < ap->y0) y = ap->y0;
  if (y > ap->y1) y = ap->y1;
  if (use_dB) y = un_dB(y);
  move_point(e, edp->env_pos, x, y);
  edp->edited = true;
}

bool edp_handle_press(void *spf, int evx, int evy, Tempus time, env *e, bool use_dB, Float xmax)
{
  int pos;
  Float x, y;
  env_ed *edp = (env_ed *)spf;
  axis_info *ap;
  ap = edp->axis;
  edp->down_time = time;
  edp->env_dragged = false;
  pos = hit_point(edp->current_xs, edp->current_ys, e->pts, evx, evy);
  x = ungrf_x(ap, evx);
  y = edp_ungrf_y_dB(ap, evy, use_dB);
  if (y < 0.0) y = 0.0;
  if (pos == -1)
    {
      if (x <= 0.0)
	{
	  pos = 0;
	  x = 0.0;
	}
      else 
	if (x >= xmax) /* (x > 1.0) */
	  {
	    pos = e->pts - 1;
	    x = xmax; /* x = 1.0; */
	  }
    }
  edp->env_pos = pos;
  /* if not -1, then user clicked existing point -- wait for drag/release to decide what to do */
  if (pos == -1) 
    {
      pos = place_point(edp->current_xs, e->pts, evx);
      add_point(e, pos + 1, x, y);
      edp->env_pos = pos + 1;
      edp->click_to_delete = false;
    }
  else edp->click_to_delete = true;
  edp->edited = true;
  return(pos == -1);
}

void edp_handle_release(void *spf, env *e)
{
  env_ed *edp = (env_ed *)spf;
  if ((edp->click_to_delete) && 
      (!(edp->env_dragged)) && 
      ((edp->env_pos > 0) && 
       (edp->env_pos < (e->pts - 1))))
    delete_point(e, edp->env_pos);
  edp->env_pos = 0;
  edp->env_dragged = false;
  edp->click_to_delete = false;
}

void edp_edited(void *spf)
{
  env_ed *edp = (env_ed *)spf;
  edp->edited = true;
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

void init_env_axes(axis_info *ap, const char *name, int x_offset, int ey0, int width, int height, 
		   Float xmin, Float xmax, Float ymin, Float ymax, bool printing)
{
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
  make_axes_1(ap, X_AXIS_IN_SECONDS, 1, SHOW_ALL_AXES, printing, true);
  /* if this is too small for an axis, it still sets up the fields needed for grf_x|y, so tiny envelope graphs will work */
}


static short grf_y_dB(Float val, axis_info *ap)
{
  if (enved_in_dB(ss))
    return(grf_y(in_dB(ss->min_dB, ss->lin_dB, val), ap));
  else return(grf_y(val, ap));
}

static double ungrf_y_dB(axis_info *ap, int y)
{
  if (enved_in_dB(ss))
    return(un_dB(ungrf_y(ap, y)));
  else return(ungrf_y(ap, y));
}

void display_enved_env(env *e, axis_context *ax, char *name, int x0, int y0, int width, int height, bool dots, Float base, bool printing)
{
  int i, j, k;
  Float ex0, ey0, ex1, ey1, val;
  int ix0, ix1, iy0, iy1, size = 0, lx0, lx1, ly0, ly1, index = 0;
  Float env_val, curx, xincr;
  mus_any *ce;
  int dur;
  axis_info *ap;
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

  ap = enved_make_axis(name, ax, x0, y0, width, height, ex0, ex1, ey0, ey1, printing); /* ax used only for GC here */
  if (e)
    {
      ix1 = grf_x(e->data[0], ap);
      iy1 = grf_y_dB(e->data[1], ap);
      if (dots)
	{
	  if (e->pts < 100)
	    size = ss->enved_point_size;
	  else size = (int)(ss->enved_point_size * 0.4);
	  set_current_point(0, ix1, iy1);
	  draw_arc(ax, ix1, iy1, size);
	}
      if (base == 1.0)
	{
	  if (enved_in_dB(ss))
	    {
	      for (j = 1, i = 2; i < e->pts * 2; i += 2, j++)
		{
		  ix0 = ix1;
		  iy0 = iy1;
		  ix1 = grf_x(e->data[i], ap);
		  iy1 = grf_y_dB(e->data[i + 1], ap);
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
		      Float yval, yincr;
		      dur = (ix1 - ix0) / EXP_SEGLEN;
		      xincr = (e->data[i] - e->data[i - 2]) / (Float)dur;
		      curx = e->data[i - 2] + xincr;
		      lx1 = ix0;
		      ly1 = iy0;
		      yval = e->data[i - 1];
		      yincr = (e->data[i + 1] - yval) / (Float)dur;
		      yval += yincr;
		      for (k = 1; k < dur; k++, curx += xincr, yval += yincr)
			{
			  lx0 = lx1;
			  ly0 = ly1;
			  lx1 = grf_x(curx, ap);
			  ly1 = grf_y(in_dB(ss->min_dB, ss->lin_dB, yval), ap);
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
		  if (dots)
		    {
		      set_current_point(j, ix1, iy1);
		      draw_arc(ax, ix1, iy1, size);
		    }
		  draw_line(ax, ix0, iy0, ix1, iy1);
		  if (printing) ps_draw_line(ap, ix0, iy0, ix1, iy1);
		}
	    }
	}
      else
	{
	  if (base <= 0.0)
	    {
	      for (j = 1, i = 2; i < e->pts * 2; i += 2, j++)
		{
		  ix0 = ix1;
		  iy0 = iy1;
		  ix1 = grf_x(e->data[i], ap);
		  iy1 = grf_y_dB(e->data[i + 1], ap);
		  if (dots)
		    {
		      set_current_point(j, ix1, iy1);
		      draw_arc(ax, ix1, iy1, size);
		    }
		  draw_line(ax, ix0, iy0, ix1, iy0);
		  draw_line(ax, ix1, iy0, ix1, iy1);
		  if (printing) 
		    {
		      ps_draw_line(ap, ix0, iy0, ix1, iy0);
		      ps_draw_line(ap, ix1, iy0, ix1, iy1);
		    }
		}
	    }
	  else
	    {
	      if (dots)
		for (j = 1, i = 2; i < e->pts * 2; i += 2, j++)
		  set_current_point(j, grf_x(e->data[i], ap), grf_y(e->data[i + 1], ap));

	      /* exponential case */
	      dur = width / EXP_SEGLEN;
	      ce = mus_make_env(e->data, e->pts, 1.0, 0.0, base, 0.0, 0, dur - 1, NULL);
	      if (ce == NULL) return;
	      if (dur < e->pts) dur = e->pts;
	      env_val = mus_env(ce);
	      ix1 = grf_x(0.0, ap);
	      iy1 = grf_y_dB(env_val, ap);
	      xincr = (ex1 - ex0) / (Float)dur;
	      j = 1;
	      for (i = 1, curx = ex0 + xincr; i < dur; i++, curx += xincr)
		{
		  iy0 = iy1;
		  ix0 = ix1;
		  env_val = mus_env(ce);
		  ix1 = grf_x(curx, ap);
		  iy1 = grf_y_dB(env_val, ap);
		  draw_line(ax, ix0, iy0, ix1, iy1);
		  if (printing) ps_draw_line(ap, ix0, iy0, ix1, iy1);
		  if ((dots) && (index != mus_position(ce)))
		    {
		      index = mus_position(ce);
		      if (index < (e->pts - 1))
			draw_arc(ax, ix1, iy1, size);
		    }
		}
	      if (curx < ex1)
		{
		  iy0 = iy1;
		  ix0 = ix1;
		  ix1 = grf_x(ex1, ap);
		  iy1 = grf_y_dB(e->data[e->pts * 2 - 1], ap);
		  draw_line(ax, ix0, iy0, ix1, iy1);
		  if (printing) ps_draw_line(ap, ix0, iy0, ix1, iy1);
		}
	      if (dots)
		draw_arc(ax, ix1, iy1, size);
	      mus_free(ce);
	    }
	}
    }
}

void view_envs(int env_window_width, int env_window_height, bool printing)
{
  /* divide space available into a grid (if needed) that shows all currently defined envelopes */
  /* I suppose if there were several hundred envelopes, we'd need a scrollable viewer... */
  int cols, rows, i, j, width, height, x, y, k;
  if (all_envs_top > 1)
    {
      cols = snd_round(sqrt((Float)(all_envs_top * env_window_width) / (Float)env_window_height));
      rows = snd_round((Float)all_envs_top / (Float)cols);
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
	display_enved_env_with_selection(all_envs[k], all_names[k], x, y, width, height, 0, 1.0, printing);
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
	  set_enved_undo_sensitive(true);
	  set_enved_revert_sensitive(true);
	}
      if ((env_list_top == env_list_size) || 
	  (env_list[env_list_top] == NULL)) 
	set_enved_redo_sensitive(false);
      set_enved_save_sensitive(true);
    }
}

void undo_env_edit(void)
{
  if (env_list)
    {
      if (env_list_top > 0)
	{
	  env_list_top--;
	  set_enved_redo_sensitive(true);
	}
      if (env_list_top == 0)
	{
	  set_enved_undo_sensitive(false);
	  set_enved_revert_sensitive(false);
	}
      set_enved_save_sensitive(true);
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
	  set_enved_undo_sensitive(false);
	  set_enved_revert_sensitive(false);
	  set_enved_save_sensitive(false);
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

static void add_envelope(char *name, env *val)
{
  int i;
  if (all_envs_top == all_envs_size)
    {
      all_envs_size += 16;
      if (all_envs)
	{
	  all_envs = (env **)REALLOC(all_envs, all_envs_size * sizeof(env *));
	  all_names = (char **)REALLOC(all_names, all_envs_size * sizeof(char *));
	  for (i = all_envs_size - 16; i < all_envs_size; i++) {all_names[i] = NULL; all_envs[i] = NULL;}
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
      set_enved_show_sensitive(true);
      make_scrolled_env_list();
    }
}

void delete_envelope(char *name)
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
	    set_enved_show_sensitive(true);
	  make_scrolled_env_list();
	}
    }
}

void alert_envelope_editor(char *name, env *val)
{
  /* whenever an envelope is defined or setf'd, we get notification through this function */
  int i;
  if (val == NULL) return;
  i = find_env(name);
  if (i != -1)
    {
      free_env(all_envs[i]);
      all_envs[i] = val;
    }
  else add_envelope(name, val);
}

void enved_show_background_waveform(axis_info *ap, axis_info *gray_ap, bool apply_to_selection, bool show_fft, bool printing)
{
  int srate, pts = 0;
  graph_type_t old_time_graph_type = GRAPH_ONCE;
  off_t samps;
  axis_info *active_ap = NULL;
  chan_info *active_channel = NULL;
  if (!(any_selected_sound())) return;
  set_grf_points(-1, 0, 0, 0); /* this is a kludge to handle one-sided graphs (snd-xchn.c) */
  gray_ap->x_axis_x0 = ap->x_axis_x0;
  gray_ap->x_axis_x1 = ap->x_axis_x1;
  gray_ap->y_axis_y0 = ap->y_axis_y0;
  gray_ap->y_axis_y1 = ap->y_axis_y1;
  active_channel = current_channel();
  if (active_channel == NULL) return;
  active_channel->printing = printing;
  if (show_fft)
    {
      if (!apply_to_selection)
	{
	  if ((active_channel->fft) &&
	      (active_channel->transform_size >= CURRENT_SAMPLES(active_channel)))
	    {
	      gray_ap->losamp = 0;
	      gray_ap->hisamp = active_channel->transform_size - 1;
	      gray_ap->y0 = 0.0;
	      gray_ap->y1 = 1.0;
	      gray_ap->x0 = 0.0;
	      gray_ap->x1 = SND_SRATE(active_channel->sound) / 2;
	      init_axis_scales(gray_ap);
	      make_fft_graph(active_channel, gray_ap, gray_ap->ax, false);
	      /* last arg makes sure we don't call any hooks in make_fft_graph */
	    }
	}
    }
  else
    {
      active_ap = active_channel->axis;
      if (apply_to_selection)
	{
	  if (!(selection_is_active())) return;
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
	  samps = CURRENT_SAMPLES(active_channel);
	  srate = SND_SRATE(active_channel->sound);
	  gray_ap->losamp = 0;
	  gray_ap->hisamp = samps - 1;
	  if (active_channel->time_graph_type == GRAPH_AS_WAVOGRAM)
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
      init_axis_scales(gray_ap);
      active_channel->axis = gray_ap;
      old_time_graph_type = active_channel->time_graph_type;
      active_channel->time_graph_type = GRAPH_ONCE;
      pts = make_background_graph(active_channel);
      active_channel->time_graph_type = old_time_graph_type;
      active_channel->axis = active_ap;
      if (pts > 0) draw_both_grfs(gray_ap->ax, pts);
    }
  active_channel->printing = false;
}

int enved_button_press_display(axis_info *ap, env *active_env, int evx, int evy)
{
  int pos, env_pos;
  Float x, y;
  pos = hit_point(current_xs, current_ys, active_env->pts, evx, evy);
  x = ungrf_x(ap, evx);
  y = ungrf_y_dB(ap, evy);
  if (enved_clip_p(ss))
    {
      if (y < ap->y0) y = ap->y0;
      if (y > ap->y1) y = ap->y1;
    }
  if (pos == -1)
    {
      if (x <= ap->x0)
	{
	  pos = 0;
	  x = ap->x0;
	}
      else 
	if (x >= ap->x1) 
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
      set_enved_click_to_delete(false);
      env_redisplay();
    }
  else set_enved_click_to_delete(true);
  enved_display_point_label(x, y);
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
#if HAVE_GUILE
	  fprintf(fd, "(if (not (defined? '%s)) (defvar %s %s))", all_names[i], all_names[i], estr);
#endif
#if HAVE_RUBY
	  fprintf(fd, "%s = %s", all_names[i], estr); /* auto-defined if necessary */
#endif
	  /* or...
	   *   perhaps this should set! a currently defined envelope back to its state upon save?
	   *   I'm not sure how people want to use this feature.
	   */
	  fprintf(fd, "\n");
	  FREE(estr);
	}
    }
}

env *xen_to_env(XEN res)
{
  XEN el, lst;
  int i, len = 0;
  Float *data;
  env *rtn = NULL;
  if (XEN_LIST_P(res))
    {
      len = XEN_LIST_LENGTH(res);
      if (len > 0)
	{
	  data = (Float *)CALLOC(len, sizeof(Float));
	  for (i = 0, lst = XEN_COPY_ARG(res); i < len; i++, lst = XEN_CDR(lst))
	    {
	      el = XEN_CAR(lst);
	      if (XEN_NUMBER_P(el))
		data[i] = XEN_TO_C_DOUBLE(el);
	      else data[i] = 0.0;
	    }
	  rtn = make_envelope(data, len);
	  FREE(data);
	}
    }
  return(rtn);
}

static bool x_increases(XEN res)
{
  int i, len;
  XEN lst;
  Float x, nx;
  len = XEN_LIST_LENGTH(res);
  x = XEN_TO_C_DOUBLE(XEN_CAR(res));
  for (i = 2, lst = XEN_CDDR(XEN_COPY_ARG(res)); i < len; i += 2, lst = XEN_CDDR(lst))
    {
      nx = XEN_TO_C_DOUBLE(XEN_CAR(lst));
      if (x >= nx) return(false);
      x = nx;
    }
  return(true);
}

#if (!HAVE_EXTENSION_LANGUAGE)
  #define ENV_BUFFER_SIZE 128
  static int env_buffer_size = 0;
  static Float *env_buffer = NULL;
  static char env_white_space[5] = {' ', '(', ')', '\t', '\''};
#endif

env *string2env(char *str) 
{
#if HAVE_EXTENSION_LANGUAGE
  XEN res;
  int len = 0;
  res = snd_catch_any(eval_str_wrapper, str, "string->env");
  if (XEN_LIST_P_WITH_LENGTH(res, len))
    {
      if ((len % 2) == 0)
	{
	  if (x_increases(res))
	    return(xen_to_env(res));
	  else snd_error(_("x axis points not increasing: %s"), str);
	}
      else snd_error(_("odd length envelope? %s"), str);
    }
  else snd_error(_("%s is not a list"), str);
  return(NULL);
#else
  char *tok;
  int i;
  float f;
  if ((str) && (*str))
    {
      i = 0;
      if (env_buffer_size == 0)
	{
	  env_buffer_size = ENV_BUFFER_SIZE;
	  env_buffer = (Float *)CALLOC(ENV_BUFFER_SIZE, sizeof(Float));
	}
      if ((*str) == '\'') str++;
      if ((*str) == '(') str++;
      tok = strtok(str, env_white_space);
      while (tok)
	{
	  sscanf(tok, "%f", &f);
	  env_buffer[i] = (Float)f;
	  i++;
	  if (i == env_buffer_size)
	    {
	      env_buffer_size *= 2;
	      env_buffer = (Float *)REALLOC(env_buffer, env_buffer_size * sizeof(Float));
	    }
	  tok = strtok(NULL, env_white_space);
	}
      if ((i == 0) || (i & 1)) 
	snd_error(_("odd length envelope? %s"), str);
      return(make_envelope(env_buffer, i));
    }
  return(NULL);
#endif
}

env *name_to_env(char *str)
{
#if HAVE_GUILE
  return(xen_to_env(XEN_NAME_AS_C_STRING_TO_VALUE(str)));
#else
  return(xen_to_env(XEN_EVAL_C_STRING(str)));
#endif
}

static XEN g_define_envelope(XEN a, XEN b)
{
  #define H_define_envelope "(" S_define_envelope " name data): define 'name' to be the envelope 'data', a list of breakpoints"
  XEN_ASSERT_TYPE(XEN_STRING_P(a), a, XEN_ARG_1, S_define_envelope, "a string");
  if (XEN_LIST_P(b)) 
    alert_envelope_editor(XEN_TO_C_STRING(a), 
			  xen_to_env(b));
  return(b);
}

XEN env_to_xen(env *e)
{
  if (e) 
    return(mus_array_to_list(e->data, 0, e->pts * 2));
  return(XEN_EMPTY_LIST);
}

void add_or_edit_symbol(char *name, env *val)
{
  /* called from envelope editor -- pass new definition into scheme */
#if (!HAVE_RUBY)
  XEN e;
#endif
  char *buf, *tmpstr = NULL;
  int len;
  tmpstr = env_to_string(val);
  len = snd_strlen(tmpstr) + snd_strlen(name) + 32;
  buf = (char *)CALLOC(len, sizeof(char));
#if HAVE_RUBY
  mus_snprintf(buf, len, "%s = %s", name, tmpstr);
#else
  e = XEN_NAME_AS_C_STRING_TO_VALUE(name);
  mus_snprintf(buf, len, "(%s %s %s)", 
	       ((XEN_BOUND_P(e)) && (XEN_LIST_P(e))) ? "set!" : "define",
	       name, 
	       tmpstr);
#endif
  if (tmpstr) FREE(tmpstr);
  snd_catch_any(eval_str_wrapper, buf, buf);
  FREE(buf);
}

env *get_env(XEN e, const char *origin) /* list in e */
{
  Float *buf = NULL;
  int i, len = 0;
  env *newenv = NULL;
  XEN lst;
  XEN_ASSERT_TYPE(XEN_LIST_P_WITH_LENGTH(e, len), e, XEN_ARG_1, origin, "a list");
  if (len == 0)
    XEN_ERROR(NO_DATA,
	      XEN_LIST_3(C_TO_XEN_STRING(origin), 
			 C_TO_XEN_STRING("null env"), 
			 e));
  buf = (Float *)CALLOC(len, sizeof(Float));
  for (i = 0, lst = XEN_COPY_ARG(e); i < len; i++, lst = XEN_CDR(lst)) 
    buf[i] = XEN_TO_C_DOUBLE_OR_ELSE(XEN_CAR(lst), 0.0);
  newenv = make_envelope(buf, len);
  if (buf) FREE(buf);
  return(newenv);
}

static XEN g_save_envelopes(XEN filename)
{
  #define H_save_envelopes "(" S_save_envelopes " filename): save the envelopes known to the envelope editor in filename"
  char *name = NULL;
  FILE *fd;
  XEN_ASSERT_TYPE((XEN_STRING_P(filename) || (XEN_FALSE_P(filename)) || (XEN_NOT_BOUND_P(filename))), filename, XEN_ONLY_ARG, S_save_envelopes, "a string or #f");
  if (XEN_STRING_P(filename)) 
    name = mus_expand_filename(XEN_TO_C_STRING(filename));
  else name = copy_string("envs.save");
  fd = FOPEN(name, "w");
  if (fd) save_envelope_editor_state(fd);
  if (name) FREE(name);
  if ((!fd) || (FCLOSE(fd) != 0))
    XEN_ERROR(CANNOT_SAVE,
	      XEN_LIST_3(C_TO_XEN_STRING(S_save_envelopes),
			 filename,
			 C_TO_XEN_STRING(strerror(errno))));
  return(filename);
}

static XEN enved_hook;

bool check_enved_hook(env *e, int pos, Float x, Float y, enved_point_t reason)
{
  XEN result = XEN_FALSE;
  XEN procs, env_list;
  bool env_changed = false;
  int len = 0;
  if (XEN_HOOKED(enved_hook))
    {
      /* if hook procedure returns a list, that is the new contents of the
       * envelope -- if its length doesn't match current, we need to remake
       * current. Otherwise return 0, and assume the caller will handle default
       */
      procs = XEN_HOOK_PROCEDURES(enved_hook);
      env_list = env_to_xen(e);
#if HAVE_GUILE
      while (XEN_NOT_NULL_P(procs))
	{
	  result = XEN_APPLY(XEN_CAR(procs), 
			     XEN_LIST_5(env_list,
					C_TO_SMALL_XEN_INT(pos),
					C_TO_XEN_DOUBLE(x),
					C_TO_XEN_DOUBLE(y),
					C_TO_SMALL_XEN_INT((int)reason)),
			     S_enved_hook);
	  procs = XEN_CDR (procs);
#else
	  result = XEN_APPLY(procs, 
			     XEN_LIST_5(env_list,
					C_TO_SMALL_XEN_INT(pos),
					C_TO_XEN_DOUBLE(x),
					C_TO_XEN_DOUBLE(y),
					C_TO_SMALL_XEN_INT((int)reason)),
			     S_enved_hook);
#endif

	  if ((XEN_NOT_FALSE_P(result)) && 
	      (XEN_LIST_P_WITH_LENGTH(result, len)))
	    {
	      /* remake env and (if not null procs) env_list */
	      /* each successive hook procedure gets the on-going (changing) envelope */
	      int i;
	      XEN lst;
	      if (len > e->data_size)
		{
		  FREE(e->data);
		  e->data = (Float *)CALLOC(len, sizeof(Float));
		  e->data_size = len;
		}
	      e->pts = len / 2;
	      for (i = 0, lst = result; i < len; i++, lst = XEN_CDR(lst))
		e->data[i] = XEN_TO_C_DOUBLE(XEN_CAR(lst));
	      if (XEN_NOT_NULL_P(procs))
		env_list = env_to_xen(e);
	      env_changed = true;
	    }
#if HAVE_GUILE
	}
#endif
    }
  return(env_changed); /* 0 = default action */
}

static XEN g_enved_base(void) {return(C_TO_XEN_DOUBLE(enved_base(ss)));}
static XEN g_set_enved_base(XEN val) 
{
  #define H_enved_base "(" S_enved_base "): envelope editor exponential base value (1.0)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_enved_base, "a number"); 
  set_enved_base(mus_fclamp(0.0, XEN_TO_C_DOUBLE(val), 300000.0));
  return(C_TO_XEN_DOUBLE(enved_base(ss)));
}

static XEN g_enved_power(void) {return(C_TO_XEN_DOUBLE(enved_power(ss)));}
static XEN g_set_enved_power(XEN val) 
{
  #define H_enved_power "(" S_enved_power "): envelope editor base scale range (9.0^power)"
  XEN_ASSERT_TYPE(XEN_NUMBER_P(val), val, XEN_ONLY_ARG, S_setB S_enved_power, "a number"); 
  set_enved_power(mus_fclamp(0.0, XEN_TO_C_DOUBLE(val), 10.0));
  return(C_TO_XEN_DOUBLE(enved_power(ss)));
}

static XEN g_enved_clip_p(void) {return(C_TO_XEN_BOOLEAN(enved_clip_p(ss)));}
static XEN g_set_enved_clip_p(XEN on)
{
  #define H_enved_clip_p "(" S_enved_clip_p "): envelope editor clip button setting; \
if clipping, the motion of the mouse is restricted to the current graph bounds."

  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(on), on, XEN_ONLY_ARG, S_setB S_enved_clip_p, "a boolean");
  set_enved_clip_p(XEN_TO_C_BOOLEAN(on)); 
  return(C_TO_XEN_BOOLEAN(enved_clip_p(ss)));
}

static XEN g_enved_exp_p(void) {return(C_TO_XEN_BOOLEAN(enved_exp_p(ss)));}
static XEN g_set_enved_exp_p(XEN val) 
{
  #define H_enved_exp_p "(" S_enved_exp_p "): envelope editor 'exp' and 'lin' buttons; \
if enved-exping, the connecting segments use exponential curves rather than straight lines."

  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_enved_exp_p, "a boolean");
  set_enved_exp_p(XEN_TO_C_BOOLEAN(val)); 
  return(C_TO_XEN_BOOLEAN(enved_clip_p(ss)));
}

static XEN g_enved_target(void) {return(C_TO_XEN_INT((int)enved_target(ss)));}
static XEN g_set_enved_target(XEN val) 
{
  enved_target_t n; 

  #define H_enved_target "(" S_enved_target "): where (amplitude, frequency, etc) the envelope is applied in the envelope editor; \
choices are " S_enved_amplitude ", " S_enved_srate ", and " S_enved_spectrum

  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_enved_target, "an integer"); 
  n = (enved_target_t)XEN_TO_C_INT(val);
  if ((n < ENVED_AMPLITUDE) || (n > ENVED_SRATE))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_enved_target, 1, val, "~A, but must be " S_enved_amplitude ", " S_enved_srate ", or " S_enved_spectrum);

  set_enved_target(n); 
  return(C_TO_XEN_INT((int)enved_target(ss)));
}

static XEN g_enved_wave_p(void) {return(C_TO_XEN_BOOLEAN(enved_wave_p(ss)));}
static XEN g_set_enved_wave_p(XEN val) 
{
  #define H_enved_wave_p "(" S_enved_wave_p "): #t if the envelope editor is displaying the waveform to be edited"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_enved_wave_p, "a boolean");
  set_enved_wave_p(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(enved_wave_p(ss)));
}

static XEN g_enved_in_dB(void) {return(C_TO_XEN_BOOLEAN(enved_in_dB(ss)));}
static XEN g_set_enved_in_dB(XEN val) 
{
  #define H_enved_in_dB "(" S_enved_in_dB "): #t if the envelope editor is using dB"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_enved_in_dB, "a boolean");
  set_enved_in_dB(XEN_TO_C_BOOLEAN(val)); 
  return(C_TO_XEN_BOOLEAN(enved_in_dB(ss)));
}

static XEN g_enved_filter_order(void) {return(C_TO_XEN_INT(enved_filter_order(ss)));}
static XEN g_set_enved_filter_order(XEN val) 
{
  #define H_enved_filter_order "(" S_enved_filter_order "): envelope editor's FIR filter order (40)"
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_enved_filter_order, "an integer"); 
  set_enved_filter_order(XEN_TO_C_INT(val));
  return(C_TO_XEN_INT(enved_filter_order(ss)));
}

static XEN g_enved_dialog(void) 
{
  #define H_enved_dialog "(" S_enved_dialog "): start the Envelope Editor"
  widget_t w;
  w = create_envelope_editor();
  return(XEN_WRAP_WIDGET(w));
}

#ifdef XEN_ARGIFY_1
XEN_NARGIFY_0(g_enved_base_w, g_enved_base)
XEN_NARGIFY_1(g_set_enved_base_w, g_set_enved_base)
XEN_NARGIFY_0(g_enved_power_w, g_enved_power)
XEN_NARGIFY_1(g_set_enved_power_w, g_set_enved_power)
XEN_NARGIFY_0(g_enved_clip_p_w, g_enved_clip_p)
XEN_NARGIFY_1(g_set_enved_clip_p_w, g_set_enved_clip_p)
XEN_NARGIFY_0(g_enved_exp_p_w, g_enved_exp_p)
XEN_NARGIFY_1(g_set_enved_exp_p_w, g_set_enved_exp_p)
XEN_NARGIFY_0(g_enved_target_w, g_enved_target)
XEN_NARGIFY_1(g_set_enved_target_w, g_set_enved_target)
XEN_NARGIFY_0(g_enved_wave_p_w, g_enved_wave_p)
XEN_NARGIFY_1(g_set_enved_wave_p_w, g_set_enved_wave_p)
XEN_NARGIFY_0(g_enved_in_dB_w, g_enved_in_dB)
XEN_NARGIFY_1(g_set_enved_in_dB_w, g_set_enved_in_dB)
XEN_NARGIFY_0(g_enved_filter_order_w, g_enved_filter_order)
XEN_NARGIFY_1(g_set_enved_filter_order_w, g_set_enved_filter_order)
XEN_NARGIFY_0(g_enved_dialog_w, g_enved_dialog)
XEN_ARGIFY_1(g_save_envelopes_w, g_save_envelopes)
XEN_NARGIFY_2(g_define_envelope_w, g_define_envelope)
#else
#define g_enved_base_w g_enved_base
#define g_set_enved_base_w g_set_enved_base
#define g_enved_power_w g_enved_power
#define g_set_enved_power_w g_set_enved_power
#define g_enved_clip_p_w g_enved_clip_p
#define g_set_enved_clip_p_w g_set_enved_clip_p
#define g_enved_exp_p_w g_enved_exp_p
#define g_set_enved_exp_p_w g_set_enved_exp_p
#define g_enved_target_w g_enved_target
#define g_set_enved_target_w g_set_enved_target
#define g_enved_wave_p_w g_enved_wave_p
#define g_set_enved_wave_p_w g_set_enved_wave_p
#define g_enved_in_dB_w g_enved_in_dB
#define g_set_enved_in_dB_w g_set_enved_in_dB
#define g_enved_filter_order_w g_enved_filter_order
#define g_set_enved_filter_order_w g_set_enved_filter_order
#define g_enved_dialog_w g_enved_dialog
#define g_save_envelopes_w g_save_envelopes
#define g_define_envelope_w g_define_envelope
#endif

void g_init_env(void)
{
  #define H_enved_amplitude "The value for " S_enved_target " that sets the envelope editor 'amp' button."
  #define H_enved_spectrum "The value for " S_enved_target " that sets the envelope editor 'flt' button."
  #define H_enved_srate "The value for " S_enved_target " that sets the envelope editor 'src' button."

  XEN_DEFINE_CONSTANT(S_enved_amplitude, ENVED_AMPLITUDE, H_enved_amplitude);
  XEN_DEFINE_CONSTANT(S_enved_spectrum,  ENVED_SPECTRUM,  H_enved_spectrum);
  XEN_DEFINE_CONSTANT(S_enved_srate,     ENVED_SRATE,     H_enved_srate);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_base,   g_enved_base_w,   H_enved_base,   S_setB S_enved_base,   g_set_enved_base_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_power,  g_enved_power_w,  H_enved_power,  S_setB S_enved_power,  g_set_enved_power_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_clip_p, g_enved_clip_p_w, H_enved_clip_p, S_setB S_enved_clip_p, g_set_enved_clip_p_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_exp_p,  g_enved_exp_p_w,  H_enved_exp_p,  S_setB S_enved_exp_p,  g_set_enved_exp_p_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_target, g_enved_target_w, H_enved_target, S_setB S_enved_target, g_set_enved_target_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_wave_p, g_enved_wave_p_w, H_enved_wave_p, S_setB S_enved_wave_p, g_set_enved_wave_p_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_in_dB,  g_enved_in_dB_w,  H_enved_in_dB,  S_setB S_enved_in_dB,  g_set_enved_in_dB_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_filter_order, g_enved_filter_order_w, H_enved_filter_order,
				   S_setB S_enved_filter_order, g_set_enved_filter_order_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE(S_enved_dialog,    g_enved_dialog_w, 0, 0, 0,     H_enved_dialog);
  XEN_DEFINE_PROCEDURE(S_save_envelopes,  g_save_envelopes_w, 0, 1, 0,   H_save_envelopes);
  XEN_DEFINE_PROCEDURE(S_define_envelope, g_define_envelope_w, 2, 0, 0,  H_define_envelope);

  XEN_DEFINE_CONSTANT(S_enved_add_point,    ENVED_ADD_POINT,    S_enved_hook " 'reason' arg when point is added");
  XEN_DEFINE_CONSTANT(S_enved_delete_point, ENVED_DELETE_POINT, S_enved_hook " 'reason' arg when point is deleted");
  XEN_DEFINE_CONSTANT(S_enved_move_point,   ENVED_MOVE_POINT,   S_enved_hook " 'reason' arg when point is moved");

  #define H_enved_hook S_enved_hook " (env pt new-x new-y reason): \
called each time a breakpoint is changed in the envelope editor; \
if it returns a list, that list defines the new envelope, \
otherwise the breakpoint is moved (but not beyond the neighboring \
breakpoint), leaving other points untouched.  The kind of change that triggered the hook \
is 'reason' which can be " S_enved_move_point ", " S_enved_delete_point ", \
or " S_enved_add_point ".  This hook makes it possible to define attack \
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

  XEN_DEFINE_HOOK(enved_hook, S_enved_hook, 5, H_enved_hook);

#if DEBUGGING && HAVE_GUILE
  XEN_DEFINE_PROCEDURE("interp-env", g_interp_env, 2, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE("window-env", g_window_env, 5, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE("multiply-envs", g_multiply_envs, 3, 0, 0, NULL);
  XEN_DEFINE_PROCEDURE("invert-env", g_invert_env, 1, 0, 0, NULL);
#endif
}
