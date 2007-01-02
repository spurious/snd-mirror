#include "snd.h"

#define ENVED_DOT_SIZE 10

env *free_env(env *e)
{
  if (e)
    {
      if (e->data) {FREE(e->data); e->data = NULL;}
      FREE(e);
    }
  return(NULL);
}

env *copy_env(env *e)
{
  if (e)
    {
      env *ne;
      ne = (env *)CALLOC(1, sizeof(env));
      ne->pts = e->pts;
      ne->data_size = e->pts * 2;
      ne->data = (Float *)MALLOC(ne->data_size * sizeof(Float));
      memcpy((void *)(ne->data), (void *)(e->data), ne->data_size * sizeof(Float));
      ne->base = e->base;
      return(ne);
    }
  return(NULL);
}

bool envs_equal(env *e1, env *e2)
{
  /* snd-mix.c check for set mix amp env no-op */
  int i;
  if ((e1 == NULL) || (e2 == NULL)) return(false);
  if (e1->pts != e2->pts) return(false);
  for (i = 0; i < e1->pts * 2; i++)
    if (e1->data[i] != e2->data[i])
      return(false);
  if (e1->base != e2->base) return(false); /* 1 and 0 are possibilities here */
  return(true);
}

char *env_to_string(env *e)
{
  char *news = NULL;
  if (e)
    {
      int i, j, len;
      bool first = true;
      char *expr_buf;
      len = 4 + (e->pts * 2 * 16);
      news = (char *)CALLOC(len, sizeof(char));
#if HAVE_RUBY
      news[0] = '[';
#endif
#if HAVE_SCHEME
      news[0] = '\'';
      news[1] = '(';
#endif
#if HAVE_FORTH
      news[0] = '\'';
      news[1] = '(';
      news[2] = ' ';
#endif
      expr_buf = (char *)CALLOC(PRINT_BUFFER_SIZE, sizeof(char));
      for (i = 0, j = 0; i < e->pts; i++, j += 2)
	{
	  if (fabs(e->data[j + 1]) < .0000001) e->data[j + 1] = 0.0; /* try to get rid of -0.000 */
#if HAVE_RUBY
	  mus_snprintf(expr_buf, PRINT_BUFFER_SIZE, "%s%.3f, %.3f", (first) ? "" : ", ", e->data[j], e->data[j + 1]);
#endif
#if HAVE_SCHEME || HAVE_FORTH
	  mus_snprintf(expr_buf, PRINT_BUFFER_SIZE, "%s%.3f %.3f", (first) ? "" : " ", e->data[j], e->data[j + 1]);
#endif
	  news = snd_strcat(news, expr_buf, &len);
	  first = false;
	}
      FREE(expr_buf);
#if HAVE_RUBY
      news = snd_strcat(news, "]", &len);
#endif
#if HAVE_SCHEME
      news = snd_strcat(news, ")", &len);
#endif
#if HAVE_FORTH
      news = snd_strcat(news, " )", &len);
#endif
    }
  else news = copy_string(PROC_FALSE);
  return(news);
}

env *make_envelope_with_offset_and_scaler(Float *env_buffer, int len, Float offset, Float scaler)
{
  env *e;
  int i, flen;
  if (len == 2) flen = 4; else flen = len;
  e = (env *)CALLOC(1, sizeof(env));
  e->data = (Float *)CALLOC(flen, sizeof(Float));
  e->data_size = flen;
  e->pts = flen / 2;
  for (i = 0; i < len; i += 2) 
    {
      e->data[i] = env_buffer[i];
      e->data[i + 1] = offset + scaler * env_buffer[i + 1];
    }
  if ((flen == 4) && (len == 2))  /* fixup degenerate envelope */
    {
      e->data[2] = e->data[0] + 1.0; 
      e->data[3] = e->data[1];
    }
  e->base = 1.0;
  return(e);
}

env *make_envelope(Float *env_buffer, int len)
{
  return(make_envelope_with_offset_and_scaler(env_buffer, len, 0.0, 1.0));
}

static env *normalize_x_axis(env *e)
{
  /* make sure env goes from 0 to 1 x-wise; e is changed */
  Float x0, x1, scl;
  int i, j;
  if (!e) return(NULL);
  x0 = e->data[0];
  x1 = e->data[e->pts * 2 - 2];
  if ((x0 == 0.0) && (x1 == 1.0))
    return(e);
  if ((e->pts == 1) || (x0 == x1))
    {
      e->data[0] = 0.0;
      return(e);
    }
  scl = 1.0 / (x1 - x0);
  for (i = 0, j = 0; i < e->pts; i++, j += 2)
    e->data[j] = (e->data[j] - x0) * scl;
  return(e);
}

static int add_breakpoint(env *e, int pos, Float x, Float y)
{
  if (pos >= e->data_size)
    {
      e->data_size += 16;
      e->data = (Float *)REALLOC(e->data, e->data_size * sizeof(Float));
    }
  e->data[pos] = x;
  e->data[pos + 1] = y;
  return(pos + 2);
}

env *window_env(env *e, off_t local_beg, off_t local_dur, off_t e_beg, off_t e_dur, Float maxx)
{
  env *local_e = NULL;
  int i, j = 0, k;
  mus_any *me;
  Float x0, x1, e_x_range, e_x0, e_x1, local_x0, local_x1, lx0;
  /* return env representing e from local beg for local dur */
  if ((local_beg < e_beg) || 
      (local_dur > e_dur) ||
      ((local_beg == e_beg) && (local_dur == e_dur)))
    return(normalize_x_axis(copy_env(e))); /* fixup 0..1? */
  /* only need to interpolate start and end breaks, copy in between */
  x0 = (double)(local_beg - e_beg) / (double)e_dur;
  x1 = (double)(local_beg + local_dur - e_beg) / (double)e_dur;
  e_x0 = e->data[0];
  e_x1 = e->data[e->pts * 2 - 2];
  e_x_range = e_x1 - e_x0;
  local_x0 = e_x0 + x0 * e_x_range;
  local_x1 = e_x0 + x1 * e_x_range;
  local_e = (env *)CALLOC(1, sizeof(env));
  local_e->data = (Float *)CALLOC(e->pts * 2, sizeof(Float));
  local_e->data_size = e->pts * 2;
  lx0 = local_x0 + maxx;
  me = mus_make_env(e->data, e->pts, 1.0, 0.0, e->base, 0.0, 0, 1000 * e->pts, NULL);
  for (k = 0, i = 0; k < e->pts; k++, i += 2)
    {
      if (e->data[i] >= local_x0)
	{
	  if (j == 0)
	    {
	      if (e->data[i] == local_x0)
		j = add_breakpoint(local_e, 0, local_x0, e->data[i + 1]);
	      else j = add_breakpoint(local_e, 0, local_x0, mus_env_interp(local_x0, me));
	    }
	  if ((e->data[i] < local_x1) && (e->data[i] != local_x0))
	    {
	      if (e->base != 1.0)
		{
		  while ((lx0 + .001) < e->data[i])
		    {
		      j = add_breakpoint(local_e, j, lx0, mus_env_interp(lx0, me));
		      lx0 += maxx;
		    }
		  lx0 = e->data[i] + maxx;
		}
	      j = add_breakpoint(local_e, j, e->data[i], e->data[i + 1]);
	      if (e->data[i] == local_x1) break; /* hit end point */
	    }
	}
      if (e->data[i] >= local_x1)
	{
	  if (e->base != 1.0)
	    {
	      while ((lx0 + .001) < local_x1)
		{
		  j = add_breakpoint(local_e, j, lx0, mus_env_interp(lx0, me));
		  lx0 += maxx;
		}
	    }
	  j = add_breakpoint(local_e, j, local_x1, mus_env_interp(local_x1, me));
	  break;
	}
    }
  mus_free(me);
  if (j == 0)
    return(free_env(local_e));
  local_e->pts = j / 2;
  local_e->base = e->base;
  return(normalize_x_axis(local_e));
}

#if MUS_DEBUGGING && HAVE_SCHEME
static XEN g_window_env(XEN e, XEN b1, XEN d1, XEN b2, XEN d2, XEN maxx)
{
  XEN res;
  env *temp1 = NULL, *temp2 = NULL;
  temp1 = xen_to_env(e);
  temp2 = window_env(temp1,
		     XEN_TO_C_OFF_T(b1),
		     XEN_TO_C_OFF_T(d1),
		     XEN_TO_C_OFF_T(b2),
		     XEN_TO_C_OFF_T(d2),
		     (XEN_NUMBER_P(maxx)) ? XEN_TO_C_DOUBLE(maxx) : 1.0);
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
  mus_any *me1, *me2;
  e = (env *)CALLOC(1, sizeof(env));
  e->data_size = (e1->pts + e2->pts) * 2;
  e->data = (Float *)CALLOC(e->data_size, sizeof(Float));
  e->data[0] = 0.0;
  e->data[1] = e1->data[1] * e2->data[1];
  me1 = mus_make_env(e1->data, e1->pts, 1.0, 0.0, e1->base, 0.0, 0, 1000 * e1->pts, NULL);
  me2 = mus_make_env(e2->data, e2->pts, 1.0, 0.0, e2->base, 0.0, 0, 1000 * e2->pts, NULL);
  while (true)
    {
      x += maxx;
      if (x >= e1->data[i]) x = e1->data[i];
      if (x >= e2->data[j]) x = e2->data[j];
      if (x == e1->data[i])
	{
	  y0 = e1->data[i + 1];
	  if (i < ((e1->pts * 2) - 1)) i += 2;
	}
      else y0 = mus_env_interp(x, me1);
      if (x == e2->data[j])
	{
	  y1 = e2->data[j + 1];
	  if (j < ((e2->pts * 2) - 1)) j += 2;
	}
      else y1 = mus_env_interp(x, me2);
      k = add_breakpoint(e, k, x, y0 * y1);
      if ((x + (maxx * .01)) >= 1.0) break;
    }
  if (k == 0)
    return(free_env(e));
  e->pts = k / 2;
  e->base = 1.0;
  mus_free(me1);
  mus_free(me2);
  return(e);
}

#if MUS_DEBUGGING && HAVE_SCHEME
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
  int i, k;
  new_e = copy_env(e);
  for (k = 0, i = 1; k < new_e->pts; k++, i += 2)
    new_e->data[i] = 1.0 - new_e->data[i];
  if ((e->base != 0.0) && (e->base != 1.0))
    new_e->base = 1.0 / e->base;
  return(new_e);
}

#if MUS_DEBUGGING && HAVE_SCHEME
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


static void add_point(env *e, int pos, Float x, Float y)
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

static void move_point(env *e, int pos, Float x, Float y)
{
  e->data[pos * 2] = x;
  e->data[pos * 2 + 1] = y;
}

static void delete_point(env *e, int pos)
{
  int i, j;
  for (i = pos, j = pos * 2; i < e->pts - 1; i++, j += 2)
    {
      e->data[j] = e->data[j + 2];
      e->data[j + 1] = e->data[j + 3];
    }
  e->pts--;
}

static int place_point(int *cxs, int points, int x, env *e, Float bx)
{
  int i;
  for (i = 0; i < points; i++)
    {
      if (x == cxs[i])
	{
	  /* use true values to disambiguate */
	  if (e->data[i * 2] > bx)
	    return(i - 1);
	  else return(i);
	}
      else
	{
	  if (x < cxs[i]) 
	    return(i - 1);
	}
    }
  return(points);
}

static int hit_point(int *cxs, int *cys, int points, int x, int y)
{
  /* enved dot size (10) is big enough that we need to search for the closest dot
   *   but I think we can assume that the x's are in order
   */
  int i, cur_i = -1, cur_min_x = 1000, cur_min_y = 1000, lim_x;
  lim_x = x + ENVED_DOT_SIZE;
  for (i = 0; (i < points) && (cxs[i] <= lim_x); i++)
    if (((x > (cxs[i] - ENVED_DOT_SIZE)) && 
	 (x < (cxs[i] + ENVED_DOT_SIZE))) &&
	((y > (cys[i] - ENVED_DOT_SIZE)) && 
	 (y < (cys[i] + ENVED_DOT_SIZE))))
      {
	if (abs(x - cxs[i]) <= cur_min_x)
	  {
	    if (abs(y - cys[i]) < cur_min_y)
	      {
		cur_i = i;
		cur_min_x = abs(x - cxs[i]);
		cur_min_y = abs(y - cys[i]);
	      }
	  }
      }
  return(cur_i);
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

bool default_env_p(env *e)
{
  if (e == NULL) return(true);
  if (e->pts != 2) return(false);
  return((snd_feq(e->data[0], 0.0)) &&
	 (snd_feq(e->data[1], 1.0)) &&
	 (snd_feq(e->data[2], 1.0)) &&
	 (snd_feq(e->data[3], 1.0)));
}

env_editor *new_env_editor(void)
{
  env_editor *edp;
  edp = (env_editor *)CALLOC(1, sizeof(env_editor));
  edp->current_xs = (int *)CALLOC(8, sizeof(int));
  edp->current_ys = (int *)CALLOC(8, sizeof(int));
  edp->axis = (axis_info *)CALLOC(1, sizeof(axis_info));
  edp->current_size = 8;
  edp->env_dragged = false;
  edp->env_pos = 0;
  edp->click_to_delete = false;
  edp->edited = false;
  edp->clip_p = true;
  edp->in_dB = false;
  return(edp);
}

static void env_editor_set_current_point(env_editor *edp, int pos, int x, int y)
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

static short env_editor_grf_y_dB(env_editor *edp, Float val)
{
  if (edp->in_dB)
    return(grf_y(in_dB(min_dB(ss), ss->lin_dB, val), edp->axis));
  else return(grf_y(val, edp->axis));
}

static Float un_dB(Float py)
{
  return((py <= min_dB(ss)) ? 0.0 : pow(10.0, py * .05));
}

double env_editor_ungrf_y_dB(env_editor *edp, int y)
{
  if (edp->in_dB)
    return(un_dB(ungrf_y(edp->axis, y)));
  else return(ungrf_y(edp->axis, y));
}


#define EXP_SEGLEN 4
typedef enum {ENVED_ADD_POINT,ENVED_DELETE_POINT,ENVED_MOVE_POINT} enved_point_t;
static bool check_enved_hook(env *e, int pos, Float x, Float y, enved_point_t reason);

/* enved display can call mus_make_env which can throw 'mus-error, so we need local protection */
static mus_error_handler_t *old_error_handler;
static void local_mus_error(int type, char *msg)
{
  snd_error(msg);
}

void env_editor_display_env(env_editor *edp, env *e, axis_context *ax, const char *name, 
			    int x0, int y0, int width, int height, printing_t printing)
{
  int i, j, k;
  Float ex0, ey0, ex1, ey1, val;
  int ix0, ix1, iy0, iy1, size = 0, lx0, lx1, ly0, ly1, index = 0;
  Float env_val, curx, xincr;
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
      if ((edp->with_dots) && (ey1 < 1.0)) ey1 = 1.0;
    }
  else
    {
      if (edp != ss->enved) return;
      ex0 = 0.0;
      ex1 = 1.0;
      ey0 = 0.0;
      ey1 = 1.0;
    }
  if (edp->in_dB)
    {
      ey0 = min_dB(ss); 
      ey1 = 0.0;
    }
  if (edp == ss->enved)
    {
      /* don't free edp->axis! */
      ap = enved_make_axis(name, ax, x0, y0, width, height, ex0, ex1, ey0, ey1, printing); /* ax used only for GC here */
      edp->axis = ap;
    }
  else 
    {
      ap = edp->axis;
      ap->ax = ax;
      if (edp->with_dots)
	init_env_axes(ap, name, x0, y0, width, height, ex0, ex1, ey0, ey1, NOT_PRINTING);
    }
  if (!(ax->wn)) return;
  if (e)
    {
      ix1 = grf_x(e->data[0], ap);
      iy1 = env_editor_grf_y_dB(edp, e->data[1]);
      if (edp->with_dots)
	{
	  if (e->pts < 100)
	    size = ENVED_DOT_SIZE;
	  else size = (int)(ENVED_DOT_SIZE * 0.4);
	  env_editor_set_current_point(edp, 0, ix1, iy1);
	  draw_arc(ax, ix1, iy1, size);
	}
      if (e->base == 1.0)
	{
	  if (edp->in_dB)
	    {
	      for (j = 1, i = 2; i < e->pts * 2; i += 2, j++)
		{
		  ix0 = ix1;
		  iy0 = iy1;
		  ix1 = grf_x(e->data[i], ap);
		  iy1 = env_editor_grf_y_dB(edp, e->data[i + 1]);
		  if (edp->with_dots)
		    {
		      env_editor_set_current_point(edp, j, ix1, iy1);
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
		      /* interpolate so the display looks closer to dB */
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
			  ly1 = grf_y(in_dB(min_dB(ss), ss->lin_dB, yval), ap);
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
		  if (edp->with_dots)
		    {
		      env_editor_set_current_point(edp, j, ix1, iy1);
		      draw_arc(ax, ix1, iy1, size);
		    }
		  draw_line(ax, ix0, iy0, ix1, iy1);
		  if (printing) ps_draw_line(ap, ix0, iy0, ix1, iy1);
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
		  ix1 = grf_x(e->data[i], ap);
		  iy1 = env_editor_grf_y_dB(edp, e->data[i + 1]);
		  if (edp->with_dots)
		    {
		      env_editor_set_current_point(edp, j, ix1, iy1);
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
	      mus_any *ce;
	      if (edp->with_dots)
		for (j = 1, i = 2; i < e->pts * 2; i += 2, j++)
		  env_editor_set_current_point(edp, j, grf_x(e->data[i], ap), grf_y(e->data[i + 1], ap));

	      /* exponential case */
	      dur = width / EXP_SEGLEN;
	      old_error_handler = mus_error_set_handler(local_mus_error);
	      ce = mus_make_env(e->data, e->pts, 1.0, 0.0, e->base, 0.0, 0, dur - 1, NULL);
	      mus_error_set_handler(old_error_handler);
	      if (ce == NULL) return;
	      if (dur < e->pts) dur = e->pts;
	      env_val = mus_env(ce);
	      ix1 = grf_x(0.0, ap);
	      iy1 = env_editor_grf_y_dB(edp, env_val);
	      xincr = (ex1 - ex0) / (Float)dur;
	      j = 1;
	      for (i = 1, curx = ex0 + xincr; i < dur; i++, curx += xincr)
		{
		  iy0 = iy1;
		  ix0 = ix1;
		  env_val = mus_env(ce);
		  ix1 = grf_x(curx, ap);
		  iy1 = env_editor_grf_y_dB(edp, env_val);
		  draw_line(ax, ix0, iy0, ix1, iy1);
		  if (printing) ps_draw_line(ap, ix0, iy0, ix1, iy1);
		  if ((edp->with_dots) && (index != mus_position(ce)))
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
		  iy1 = env_editor_grf_y_dB(edp, e->data[e->pts * 2 - 1]);
		  draw_line(ax, ix0, iy0, ix1, iy1);
		  if (printing) ps_draw_line(ap, ix0, iy0, ix1, iy1);
		}
	      if (edp->with_dots)
		draw_arc(ax, ix1, iy1, size);
	      mus_free(ce);
	    }
	}
    }
}

void env_editor_button_motion(env_editor *edp, int evx, int evy, Tempus motion_time, env *e)
{
  axis_info *ap;
  Float x0, x1, x, y;
  if ((e == NULL) || (edp == NULL)) return;
  if ((motion_time - edp->down_time) < ss->click_time) return;
  edp->env_dragged = true;
  edp->click_to_delete = false;
  ap = edp->axis;
  x = ungrf_x(ap, evx);
  if (edp->env_pos > 0) 
    x0 = e->data[edp->env_pos * 2 - 2]; 
  else x0 = e->data[0];
  if (edp->env_pos < (e->pts - 1))
    x1 = e->data[edp->env_pos * 2 + 2]; /* looking for next point on right to avoid crossing it */
  else x1 = e->data[e->pts * 2 - 2];
  if (x < x0) x = x0;
  if (x > x1) x = x1;
  if (edp->env_pos == 0) x = e->data[0];
  if (edp->env_pos == (e->pts - 1)) x = e->data[(e->pts - 1) * 2];
  y = ungrf_y(ap, evy);
  if ((edp->clip_p) || (edp->in_dB))
    {
      if (y < ap->y0) y = ap->y0;
      if (y > ap->y1) y = ap->y1;
    }
  if (edp->in_dB) y = un_dB(y);
  if ((edp != ss->enved) || (check_enved_hook(e, edp->env_pos, x, y, ENVED_MOVE_POINT) == 0))
    move_point(e, edp->env_pos, x, y);
  edp->edited = true;
}

bool env_editor_button_press(env_editor *edp, int evx, int evy, Tempus time, env *e)
{
  int pos;
  Float x, y;
  axis_info *ap;
  ap = edp->axis;
  edp->down_time = time;
  edp->env_dragged = false;
  pos = hit_point(edp->current_xs, edp->current_ys, e->pts, evx, evy);
  x = ungrf_x(ap, evx);
  y = env_editor_ungrf_y_dB(edp, evy);
  if (edp->clip_p)
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
	{
	if (x >= ap->x1) 
	  {
	    pos = e->pts - 1;
	    x = ap->x1;
	  }
	}
    }
  edp->env_pos = pos;
  /* if not -1, then user clicked existing point -- wait for drag/release to decide what to do */
  if (pos == -1) 
    {
      pos = place_point(edp->current_xs, e->pts, evx, e, x);
      /* place returns left point index of current segment or pts if off left end */
      /* in this case, user clicked in middle of segment, so add point there */
      if ((edp != ss->enved) || (check_enved_hook(e, pos, x, y, ENVED_ADD_POINT) == 0))
	add_point(e, pos + 1, x, y);
      edp->env_pos = pos + 1;
      edp->click_to_delete = false;
    }
  else edp->click_to_delete = true;
  edp->edited = true;
  return(pos == -1);
}

void env_editor_button_release(env_editor *edp, env *e)
{
  if ((edp->click_to_delete) && 
      (!(edp->env_dragged)) && 
      (edp->env_pos > 0) && 
      (edp->env_pos < (e->pts - 1)) &&
      ((edp != ss->enved) || (check_enved_hook(e, edp->env_pos, 0, 0, ENVED_DELETE_POINT) == 0)))
    delete_point(e, edp->env_pos);
  prepare_enved_edit(e);
  edp->env_pos = 0;
  edp->env_dragged = false;
  edp->click_to_delete = false;
}



/* -------- (main) ENVELOPE EDITOR FUNCTIONS -------- */

static int env_list_size = 0;    /* current size of env edits list */
static int env_list_top = 0;     /* one past current active position in list */
static env **env_list = NULL;    /* env edits list (for local undo/redo/revert) */

static env **all_envs = NULL;    /* all envs, either loaded or created in editor */
static char **all_names = NULL;  /* parallel names */
static int all_envs_size = 0;    /* size of this array */
static int all_envs_top = 0;     /* one past pointer to last entry in this array */

void init_env_axes(axis_info *ap, const char *name, int x_offset, int ey0, int width, int height, 
		   Float xmin, Float xmax, Float ymin, Float ymax, printing_t printing)
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
  make_axes_1(ap, X_AXIS_IN_SECONDS, 1, SHOW_ALL_AXES, printing, WITH_X_AXIS, NO_GRID, WITH_LINEAR_AXES, grid_density(ss));
  /* if this is too small for an axis, it still sets up the fields needed for grf_x|y, so tiny envelope graphs will work */
}

void view_envs(int env_window_width, int env_window_height, printing_t printing)
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
	display_enved_env_with_selection(all_envs[k], all_names[k], x, y, width, height, 0, printing);
	k++;
	if (k == all_envs_top) return;
      }
}

int hit_env(int xe, int ye, int env_window_width, int env_window_height)
{
  if (all_envs_top == 0)
    return(-1);
  else
    {
      if (all_envs_top == 1)
	return(0);
      else
	{
	  int cols, rows, i, j, width, height, x, y, k;
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

void prepare_enved_edit(env *new_env)
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
	  /* set_enved_revert_sensitive(false); */
	}
      set_enved_save_sensitive(true);
    }
}

void revert_env_edit(void)
{
  if (env_list)
    {
      if (env_list_top > 0)
	set_enved_redo_sensitive(true);
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

static int find_env(const char *name)
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
  if (all_envs_top == all_envs_size)
    {
      all_envs_size += 16;
      if (all_envs)
	{
	  int i;
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
  int pos;
  pos = find_env(name);
  if (pos != -1)
    {
      int i;
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
  /* whenever an envelope is defined, we get notification through this function */
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

typedef struct {
  int size;
  Float *data;
  Float scale;
} enved_fft;

typedef struct enved_ffts {
  int size;
  enved_fft **ffts;
} enved_ffts;

static enved_fft *free_enved_fft(enved_fft *ef)
{
  if (ef)
    {
      if (ef->data) FREE(ef->data);
      ef->data = NULL;
      FREE(ef);
    }
  return(NULL);
}

void free_enved_spectra(chan_info *cp)
{
  if (cp->enved_spectra)
    {
      enved_ffts *efs;
      int i;
      efs = cp->enved_spectra;
      for (i = 0; i < efs->size; i++)
	efs->ffts[i] = free_enved_fft(efs->ffts[i]);
      FREE(efs->ffts);
      FREE(efs);
      cp->enved_spectra = NULL;
    }
}

void release_dangling_enved_spectra(chan_info *cp, int edpt)
{
  if (cp->enved_spectra)
    {
      enved_ffts *efs;
      int i;
      efs = cp->enved_spectra;
      if (edpt < efs->size)
	for (i = edpt; i < efs->size; i++)
	  efs->ffts[i] = free_enved_fft(efs->ffts[i]);
    }
}

void reflect_enved_spectra_change(chan_info *cp)
{
  if ((cp->enved_spectra) && 
      (cp == current_channel()) &&
      (enved_target(ss) == ENVED_SPECTRUM))
    env_redisplay();
}

static enved_fft *new_enved_fft(chan_info *cp)
{
  enved_ffts *efs;
  if (cp->enved_spectra == NULL)
    cp->enved_spectra = (enved_ffts *)CALLOC(1, sizeof(enved_ffts));
  efs = cp->enved_spectra;
  if (efs->size <= cp->edit_ctr)
    {
      if (efs->ffts)
	{
	  int i, old_size;
	  old_size = efs->size;
	  efs->ffts = (enved_fft **)REALLOC(efs->ffts, (cp->edit_ctr + 1) * sizeof(enved_fft *));
	  for (i = old_size; i <= cp->edit_ctr; i++) efs->ffts[i] = NULL;
	}
      else efs->ffts = (enved_fft **)CALLOC(cp->edit_ctr + 1, sizeof(enved_fft *));
      efs->size = cp->edit_ctr + 1;
    }
  if (efs->ffts[cp->edit_ctr] == NULL)
    efs->ffts[cp->edit_ctr] = (enved_fft *)CALLOC(1, sizeof(enved_fft));
  return(efs->ffts[cp->edit_ctr]);
}

#define DEFAULT_ENVED_MAX_FFT_SIZE 1048576
static int enved_max_fft_size = DEFAULT_ENVED_MAX_FFT_SIZE;

static enved_fft *make_enved_spectrum(chan_info *cp)
{
  enved_fft *ef;
  ef = new_enved_fft(cp);
  if ((ef) && (ef->size == 0)) /* otherwise it is presumably already available */
    {
      int i, fsize, data_len;
      Float data_max = 0.0;
      snd_fd *sf;
      data_len = (int)(CURRENT_SAMPLES(cp)); /* known to be int here (size check below) */
      if (data_len == 0) return(NULL);
      sf = init_sample_read(0, cp, READ_FORWARD);
      if (sf == NULL) return(NULL);
      fsize = snd_to_int_pow2(data_len);
      if (fsize <= enved_max_fft_size)
	{
	  ef->size = fsize;
	  ef->data = (Float *)MALLOC(fsize * sizeof(Float));
	}
      fourier_spectrum(sf, ef->data, ef->size, data_len, NULL);
      free_snd_fd(sf);
      for (i = 0; i < ef->size; i++) 
	if (ef->data[i] > data_max) 
	  data_max = ef->data[i];
      if (data_max > 0.0) ef->scale = data_max;
    }
  return(ef);
}

static void display_enved_spectrum(chan_info *cp, enved_fft *ef, axis_info *ap)
{
  if (ef)
    {
      Float incr, x = 0.0;
      int i = 0, j = 0, hisamp;
      Float samples_per_pixel, xf = 0.0, ina, ymax;
      ap->losamp = 0;
      ap->hisamp = ef->size - 1;
      ap->y0 = 0.0;
      ap->y1 = ef->scale;
      ap->x0 = 0.0;
      ap->x1 = SND_SRATE(cp->sound) / 2;
      init_axis_scales(ap);
      hisamp = (int)(ef->size / 2);
      incr = (Float)SND_SRATE(cp->sound) / (Float)(ef->size);
      samples_per_pixel = (Float)((double)hisamp / (Float)(ap->x_axis_x1 - ap->x_axis_x0));
      if (samples_per_pixel < 4.0)
	{
	  for (i = 0, x = 0.0; i < hisamp; i++, x += incr)
	    set_grf_point(grf_x(x, ap), i, grf_y(ef->data[i], ap));
	  draw_grf_points(1, ap->ax, i, ap, 0.0, GRAPH_LINES);
	}
      else
	{
	  ymax = -1.0;
	  while (i < hisamp)
	    {
	      ina = ef->data[i++];
	      if (ina > ymax) ymax = ina;
	      xf += 1.0;
	      if (xf > samples_per_pixel)
		{
		  set_grf_point(grf_x(x, ap), j++, grf_y(ymax, ap));
		  x += (incr * samples_per_pixel); 
		  xf -= samples_per_pixel;
		  ymax = -1.0;
		}
	    }
	  draw_grf_points(1, ap->ax, j, ap, 0.0, GRAPH_LINES);
	}
    }
}


void enved_show_background_waveform(axis_info *ap, axis_info *gray_ap, bool apply_to_selection, bool show_fft, printing_t printing)
{
  int srate, pts = 0;
  graph_type_t old_time_graph_type = GRAPH_ONCE;
  off_t samps;
  printing_t old_printing;
  bool two_sided = false;
  axis_info *active_ap = NULL;
  chan_info *active_channel = NULL;
  if (!(any_selected_sound())) return;
  gray_ap->x_axis_x0 = ap->x_axis_x0;
  gray_ap->x_axis_x1 = ap->x_axis_x1;
  gray_ap->y_axis_y0 = ap->y_axis_y0;
  gray_ap->y_axis_y1 = ap->y_axis_y1;
  active_channel = current_channel();
  if ((!active_channel) || (!(active_channel->active))) return;
  old_printing = active_channel->printing;
  active_channel->printing = printing;
  if (show_fft)
    {
      if (enved_max_fft_size < transform_size(ss)) enved_max_fft_size = transform_size(ss);
      if (enved_max_fft_size < active_channel->transform_size) enved_max_fft_size = active_channel->transform_size;
      if ((!apply_to_selection) &&
	  (CURRENT_SAMPLES(active_channel) <= enved_max_fft_size))
	display_enved_spectrum(active_channel, make_enved_spectrum(active_channel), gray_ap);
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
	  gray_ap->x0 = (double)(gray_ap->losamp) / (double)srate;
	  gray_ap->x1 = (double)(gray_ap->hisamp) / (double)srate;
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
	  gray_ap->x1 = (double)samps / (double)srate;
	}
      init_axis_scales(gray_ap);
      active_channel->axis = gray_ap;
      old_time_graph_type = active_channel->time_graph_type;
      active_channel->time_graph_type = GRAPH_ONCE;
      pts = make_background_graph(active_channel, srate, &two_sided);
      active_channel->time_graph_type = old_time_graph_type;
      active_channel->axis = active_ap;
      if (pts > 0) 
	{
	  if (two_sided)
	    draw_both_grf_points(1, gray_ap->ax, pts, GRAPH_LINES);
	  else draw_grf_points(1, gray_ap->ax, pts, gray_ap, 0.0, GRAPH_LINES);
	}
    }
  active_channel->printing = old_printing;
}

env *enved_next_env(void)
{
  if (env_list_top > 0) 
    return(copy_env(env_list[env_list_top - 1])); 
  else return(NULL);
}

char *env_name_completer(char *text, void *data)
{
  int matches = 0;
  char *current_match = NULL;
  if ((all_envs) && (text) && (*text))
    {
      int i, j, len, curlen;
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
  int i;
  for (i = 0; i < all_envs_top; i++)
    {
      char *estr;
      estr = env_to_string(all_envs[i]);
      if (estr)
	{
#if HAVE_SCHEME
	  fprintf(fd, "(%s %s %s %.4f)\n", S_define_envelope, all_names[i], estr, all_envs[i]->base);
#endif
#if HAVE_RUBY
	  fprintf(fd, "%s(\"%s\", %s, %.4f)\n", xen_scheme_procedure_to_ruby(S_define_envelope), all_names[i], estr, all_envs[i]->base);
#endif
#if HAVE_FORTH
	  fprintf(fd, "\"%s\" %s %.4f %s drop\n", all_names[i], estr, all_envs[i]->base, S_define_envelope);
#endif
	  FREE(estr);
	}
    }
}

env *xen_to_env(XEN res)
{
  env *rtn = NULL;
  if (XEN_LIST_P(res))
    {
      int len = 0;
      len = XEN_LIST_LENGTH(res);
      if (len > 0)
	{
	  int i;
	  Float *data;
	  XEN lst;
	  data = (Float *)CALLOC(len, sizeof(Float));
	  for (i = 0, lst = XEN_COPY_ARG(res); i < len; i++, lst = XEN_CDR(lst))
	    {
	      XEN el;
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
  Float x;
  len = XEN_LIST_LENGTH(res);
  x = XEN_TO_C_DOUBLE(XEN_CAR(res));
  for (i = 2, lst = XEN_CDDR(XEN_COPY_ARG(res)); i < len; i += 2, lst = XEN_CDDR(lst))
    {
      Float nx;
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

env *string_to_env(char *str) 
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
  char *tok, *tmp;
  int i;
  float f;
  if ((str) && (*str))
    {
      char *old_tmp;
      tmp = copy_string(str);
      old_tmp = tmp;
      i = 0;
      if (env_buffer_size == 0)
	{
	  env_buffer_size = ENV_BUFFER_SIZE;
	  env_buffer = (Float *)CALLOC(ENV_BUFFER_SIZE, sizeof(Float));
	}
      if ((*tmp) == '\'') tmp++;
      if ((*tmp) == '(') tmp++;
      tok = strtok(tmp, env_white_space);
      while (tok)
	{
	  if (!(sscanf(tok, "%f", &f)))
	    {
	      snd_error("%s in env list is not a number", tok);
	      return(NULL);
	    }
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
      FREE(old_tmp);
      return(make_envelope(env_buffer, i));
    }
  return(NULL);
#endif
}

env *position_to_env(int pos)
{
  if (pos < 0) return(NULL);
  return(copy_env(all_envs[pos]));
}

env *name_to_env(const char *str)
{
  env *e;
  int pos;
  pos = find_env(str);
  if (pos >= 0) return(copy_env(all_envs[pos]));
#if HAVE_SCHEME || HAVE_FORTH
  e = xen_to_env(XEN_NAME_AS_C_STRING_TO_VALUE(str));
#else
  e = xen_to_env(XEN_EVAL_C_STRING((char *)str));
#endif
  return(e);
}

#if HAVE_RUBY
#define SND_ENV_MAX_VARS 100
static XEN snd_env_array[SND_ENV_MAX_VARS];
static int env_index = -1;
#endif

static XEN g_define_envelope(XEN name, XEN data, XEN base)
{
  env *e;
  char *ename;
  #define H_define_envelope "(" S_define_envelope " name data :optional base): load 'name' with associated 'data', a list of breakpoints \
into the envelope editor."
  XEN_ASSERT_TYPE(XEN_STRING_P(name) || XEN_SYMBOL_P(name), name, XEN_ARG_1, S_define_envelope, "a string or symbol");
  XEN_ASSERT_TYPE(XEN_LIST_P(data), data, XEN_ARG_2, S_define_envelope, "a list of breakpoints");
  XEN_ASSERT_TYPE(XEN_NUMBER_IF_BOUND_P(base) || XEN_FALSE_P(base), base, XEN_ARG_3, S_define_envelope, "a float or " PROC_FALSE);
  if (XEN_STRING_P(name))
    ename = XEN_TO_C_STRING(name);
  else ename = XEN_SYMBOL_TO_C_STRING(name);
  e = xen_to_env(data);
  if (!e) return(XEN_FALSE);
  if (XEN_NUMBER_P(base))
    e->base = XEN_TO_C_DOUBLE(base);
#if HAVE_RUBY
  alert_envelope_editor(xen_scheme_global_variable_to_ruby(ename), e);
  if (env_index >= SND_ENV_MAX_VARS)
    env_index = 0;
  else
    env_index++;
  XEN_DEFINE_VARIABLE(ename, snd_env_array[env_index], data); /* need global C variable */
  return(snd_env_array[env_index]);
#endif
#if HAVE_SCHEME || HAVE_FORTH
  {
    XEN temp;
    alert_envelope_editor(ename, e);
    XEN_DEFINE_VARIABLE(ename, temp, data); /* already gc protected */
    return(temp);
  }
#endif
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
#if HAVE_RUBY
  char *buf, *tmpstr = NULL;
  int len;
  if (!val) return;
  tmpstr = env_to_string(val);
  len = snd_strlen(tmpstr) + snd_strlen(name) + 32;
  buf = (char *)CALLOC(len, sizeof(char));
  mus_snprintf(buf, len, "%s = %s", name, tmpstr);
  if (tmpstr) FREE(tmpstr);
  snd_catch_any(eval_str_wrapper, buf, buf);
  FREE(buf);
#endif
#if HAVE_GUILE
  XEN e;
  if (!val) return;
  if (XEN_DEFINED_P(name))
    {
      e = XEN_NAME_AS_C_STRING_TO_VARIABLE(name);
      XEN_VARIABLE_SET(e, env_to_xen(val));
    }
  else XEN_DEFINE_VARIABLE(name, e, env_to_xen(val));
#endif
#if HAVE_FORTH || HAVE_GAUCHE
  XEN e;
  if (!val) return;
  if (XEN_DEFINED_P(name))
    e = XEN_VARIABLE_SET(name, env_to_xen(val));
  else XEN_DEFINE_VARIABLE(name, e, env_to_xen(val)); /* may need to click 'save' for this to take effect (Gauche) */
#endif
}

env *get_env(XEN e, const char *origin) /* list in e */
{
  int i, len = 0;
  env *new_env;
  XEN_ASSERT_TYPE(XEN_LIST_P_WITH_LENGTH(e, len), e, XEN_ARG_1, origin, "a list");
  if (len == 0)
    XEN_ERROR(NO_DATA,
	      XEN_LIST_3(C_TO_XEN_STRING(origin), 
			 C_TO_XEN_STRING("null env"), 
			 e));
  new_env = xen_to_env(e);
  for (i = 2; i < new_env->pts * 2; i += 2)
    if (new_env->data[i - 2] > new_env->data[i])
      {
	XEN msg;
	char buf[1024];
	mus_snprintf(buf, 1024, "%s: env at breakpoint %d: x axis value %f > %f", origin, i / 2, new_env->data[i - 2], new_env->data[i]);
	msg = C_TO_XEN_STRING(buf);
	free_env(new_env);
	XEN_ERROR(XEN_ERROR_TYPE("env-error"),
		  XEN_LIST_3(C_TO_XEN_STRING(S_filter_channel),
			     msg,
			     e));
      }
  return(new_env);
}

static XEN g_save_envelopes(XEN filename)
{
  #define H_save_envelopes "(" S_save_envelopes " :optional filename): save the envelopes known to the envelope editor in filename"
  char *name = NULL;
  FILE *fd;
  XEN_ASSERT_TYPE((XEN_STRING_P(filename) || (XEN_FALSE_P(filename)) || (XEN_NOT_BOUND_P(filename))), 
		  filename, XEN_ONLY_ARG, S_save_envelopes, "a string or " PROC_FALSE);
  if (XEN_STRING_P(filename)) 
    name = mus_expand_filename(XEN_TO_C_STRING(filename));
  else name = copy_string("envs.save");
  fd = FOPEN(name, "w");
  if (fd) 
    {
      save_envelope_editor_state(fd);
      snd_fclose(fd, name);
    }
  if (name) FREE(name);
  if (!fd)
    {
      XEN_ERROR(CANNOT_SAVE,
		XEN_LIST_3(C_TO_XEN_STRING(S_save_envelopes),
			   filename,
			   C_TO_XEN_STRING(snd_open_strerror())));
    }
  return(filename);
}

static XEN enved_hook;

static bool check_enved_hook(env *e, int pos, Float x, Float y, enved_point_t reason)
{
  bool env_changed = false;
  if (XEN_HOOKED(enved_hook))
    {
      int len = 0;
      XEN result = XEN_FALSE;
      XEN procs, env_list;
      /* if hook procedure returns a list, that is the new contents of the
       * envelope -- if its length doesn't match current, we need to remake
       * current. Otherwise return 0, and assume the caller will handle default
       */
      procs = XEN_HOOK_PROCEDURES(enved_hook);
      env_list = env_to_xen(e);
      while (XEN_NOT_NULL_P(procs))
	{
	  result = XEN_APPLY(XEN_CAR(procs), 
			     XEN_LIST_5(env_list,
					C_TO_XEN_INT(pos),
					C_TO_XEN_DOUBLE(x),
					C_TO_XEN_DOUBLE(y),
					C_TO_XEN_INT((int)reason)),
			     S_enved_hook);
	  procs = XEN_CDR (procs);
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
	      for (i = 0, lst = XEN_COPY_ARG(result); i < len; i++, lst = XEN_CDR(lst))
		e->data[i] = XEN_TO_C_DOUBLE(XEN_CAR(lst));
	      if (XEN_NOT_NULL_P(procs))
		env_list = env_to_xen(e);
	      env_changed = true;
	    }
	}
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

static XEN g_enved_style(void) {return(C_TO_XEN_INT(enved_style(ss)));}
static XEN g_set_enved_style(XEN val) 
{
  #define H_enved_style "(" S_enved_style "): envelope editor breakpoint connection choice: can \
be " S_envelope_linear ", or " S_envelope_exponential

  int choice;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_enved_style, S_envelope_linear ", or " S_envelope_exponential);
  choice = XEN_TO_C_INT(val);
  if ((choice == ENVELOPE_LINEAR) || (choice == ENVELOPE_EXPONENTIAL))
    {
      set_enved_style((env_type_t)choice);
      reflect_enved_style();
    }
  else XEN_OUT_OF_RANGE_ERROR(S_enved_style, XEN_ONLY_ARG, val, "must be " S_envelope_linear ", or " S_envelope_exponential);
  return(val);
}

static XEN g_enved_target(void) {return(C_TO_XEN_INT((int)enved_target(ss)));}
static XEN g_set_enved_target(XEN val) 
{
  enved_target_t n;
  int in_n;

  #define H_enved_target "(" S_enved_target "): where (amplitude, frequency, etc) the envelope is applied in the envelope editor; \
choices are " S_enved_amplitude ", " S_enved_srate ", and " S_enved_spectrum

  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_enved_target, "an integer"); 
  in_n = XEN_TO_C_INT(val);
  if (in_n < 0) /* weird -- C++ needs this extra layer */
    XEN_OUT_OF_RANGE_ERROR(S_setB S_enved_target, 1, val, "~A, but must be " S_enved_amplitude ", " S_enved_srate ", or " S_enved_spectrum);
  n = (enved_target_t)in_n;
  if (n > ENVED_SRATE)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_enved_target, 1, val, "~A, but must be " S_enved_amplitude ", " S_enved_srate ", or " S_enved_spectrum);
  set_enved_target(n); 
  return(C_TO_XEN_INT((int)enved_target(ss)));
}

static XEN g_enved_wave_p(void) {return(C_TO_XEN_BOOLEAN(enved_wave_p(ss)));}
static XEN g_set_enved_wave_p(XEN val) 
{
  #define H_enved_wave_p "(" S_enved_wave_p "): " PROC_TRUE " if the envelope editor is displaying the waveform to be edited"
  XEN_ASSERT_TYPE(XEN_BOOLEAN_P(val), val, XEN_ONLY_ARG, S_setB S_enved_wave_p, "a boolean");
  set_enved_wave_p(XEN_TO_C_BOOLEAN(val));
  return(C_TO_XEN_BOOLEAN(enved_wave_p(ss)));
}

static XEN g_enved_in_dB(void) {return(C_TO_XEN_BOOLEAN(enved_in_dB(ss)));}
static XEN g_set_enved_in_dB(XEN val) 
{
  #define H_enved_in_dB "(" S_enved_in_dB "): " PROC_TRUE " if the envelope editor is using dB"
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
XEN_NARGIFY_0(g_enved_style_w, g_enved_style)
XEN_NARGIFY_1(g_set_enved_style_w, g_set_enved_style)
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
XEN_ARGIFY_3(g_define_envelope_w, g_define_envelope)
#if MUS_DEBUGGING && HAVE_SCHEME
  XEN_ARGIFY_6(g_window_env_w, g_window_env)
  XEN_NARGIFY_1(g_invert_env_w, g_invert_env)
  XEN_NARGIFY_3(g_multiply_envs_w, g_multiply_envs)
#endif
#else
#define g_enved_base_w g_enved_base
#define g_set_enved_base_w g_set_enved_base
#define g_enved_power_w g_enved_power
#define g_set_enved_power_w g_set_enved_power
#define g_enved_clip_p_w g_enved_clip_p
#define g_set_enved_clip_p_w g_set_enved_clip_p
#define g_enved_style_w g_enved_style
#define g_set_enved_style_w g_set_enved_style
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
#if MUS_DEBUGGING && HAVE_SCHEME
  #define g_window_env_w g_window_env
  #define g_invert_env_w g_invert_env
  #define g_multiply_envs_w g_multiply_envs
#endif
#endif

void g_init_env(void)
{
  #define H_enved_amplitude "The value for " S_enved_target " that sets the envelope editor 'amp' button."
  #define H_enved_spectrum "The value for " S_enved_target " that sets the envelope editor 'flt' button."
  #define H_enved_srate "The value for " S_enved_target " that sets the envelope editor 'src' button."

  XEN_DEFINE_CONSTANT(S_enved_amplitude, ENVED_AMPLITUDE, H_enved_amplitude);
  XEN_DEFINE_CONSTANT(S_enved_spectrum,  ENVED_SPECTRUM,  H_enved_spectrum);
  XEN_DEFINE_CONSTANT(S_enved_srate,     ENVED_SRATE,     H_enved_srate);

  XEN_DEFINE_CONSTANT(S_envelope_linear,      ENVELOPE_LINEAR,      S_enved_style " choice: linear connections between breakpoints");
  XEN_DEFINE_CONSTANT(S_envelope_exponential, ENVELOPE_EXPONENTIAL, S_enved_style " choice: exponential connections between breakpoints");

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_base,   g_enved_base_w,   H_enved_base,   S_setB S_enved_base,   g_set_enved_base_w,    0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_power,  g_enved_power_w,  H_enved_power,  S_setB S_enved_power,  g_set_enved_power_w,   0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_clip_p, g_enved_clip_p_w, H_enved_clip_p, S_setB S_enved_clip_p, g_set_enved_clip_p_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_style,  g_enved_style_w,  H_enved_style,  S_setB S_enved_style,  g_set_enved_style_w,   0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_target, g_enved_target_w, H_enved_target, S_setB S_enved_target, g_set_enved_target_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_wave_p, g_enved_wave_p_w, H_enved_wave_p, S_setB S_enved_wave_p, g_set_enved_wave_p_w,  0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_in_dB,  g_enved_in_dB_w,  H_enved_in_dB,  S_setB S_enved_in_dB,  g_set_enved_in_dB_w,   0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_enved_filter_order, g_enved_filter_order_w, H_enved_filter_order,
				   S_setB S_enved_filter_order, g_set_enved_filter_order_w,  0, 0, 1, 0);

  XEN_DEFINE_PROCEDURE(S_enved_dialog,    g_enved_dialog_w,    0, 0, 0, H_enved_dialog);
  XEN_DEFINE_PROCEDURE(S_save_envelopes,  g_save_envelopes_w,  0, 1, 0, H_save_envelopes);


#if HAVE_SCHEME
  XEN_DEFINE_PROCEDURE(S_define_envelope "-1", g_define_envelope_w, 2, 1, 0, H_define_envelope);
  XEN_EVAL_C_STRING("(defmacro define-envelope (a . b) `(define-envelope-1 ',a ,@b))");
  XEN_EVAL_C_STRING("(defmacro defvar (a b) `(define-envelope-1 ',a ,b))");
#else
  XEN_DEFINE_PROCEDURE(S_define_envelope, g_define_envelope_w, 2, 1, 0, H_define_envelope);
#endif

  XEN_DEFINE_CONSTANT(S_enved_add_point,      ENVED_ADD_POINT,      S_enved_hook " 'reason' arg when point is added");
  XEN_DEFINE_CONSTANT(S_enved_delete_point,   ENVED_DELETE_POINT,   S_enved_hook " 'reason' arg when point is deleted");
  XEN_DEFINE_CONSTANT(S_enved_move_point,     ENVED_MOVE_POINT,     S_enved_hook " 'reason' arg when point is moved");

#if HAVE_SCHEME
  #define H_enved_hook S_enved_hook " (env pt new-x new-y reason): \
called each time a breakpoint is changed in the envelope editor; \
if it returns a list, that list defines the new envelope, \
otherwise the breakpoint is moved (but not beyond the neighboring \
breakpoint), leaving other points untouched.  The kind of change that triggered the hook \
is 'reason' which can be " S_enved_move_point ", " S_enved_delete_point ", \
or " S_enved_add_point ".  This hook makes it possible to define attack \
and decay portions in the envelope editor, or use functions such as \
stretch-envelope from env.scm: \n\
 (add-hook! " S_enved_hook "\n\
   (lambda (env pt x y reason)\n\
     (if (= reason " S_enved_move_point ")\n\
         (let* ((old-x (list-ref env (* pt 2)))\n\
                (new-env (stretch-envelope env old-x x)))\n\
           (list-set! new-env (+ (* pt 2) 1) y)\n\
           new-env)\n\
         #f)))"
#endif
#if HAVE_RUBY
  #define H_enved_hook S_enved_hook " (env pt new-x new-y reason): \
called each time a breakpoint is changed in the envelope editor; \
if it returns a list, that list defines the new envelope, \
otherwise the breakpoint is moved (but not beyond the neighboring \
breakpoint), leaving other points untouched.  The kind of change that triggered the hook \
is 'reason' which can be " S_enved_move_point ", " S_enved_delete_point ", \
or " S_enved_add_point ".  This hook makes it possible to define attack \
and decay portions in the envelope editor."
#endif
#if HAVE_FORTH
  #define H_enved_hook S_enved_hook " (env pt new-x new-y reason): \
called each time a breakpoint is changed in the envelope editor; \
if it returns a list, that list defines the new envelope, \
otherwise the breakpoint is moved (but not beyond the neighboring \
breakpoint), leaving other points untouched.  The kind of change that triggered the hook \
is 'reason' which can be " S_enved_move_point ", " S_enved_delete_point ", \
or " S_enved_add_point ".  This hook makes it possible to define attack \
and decay portions in the envelope editor, or use functions such as \
stretch-envelope from env.fth: \n\
" S_enved_hook " lambda: <{ en pt x y reason }>\n\
  reason " S_enved_move_point " = if\n\
    en old-x  en pt 2* list@  x stretch-envelope  pt 2* 1+ y list!\n\
  else\n\
    #f\n\
  then\n\
; add-hook!"
#endif

  enved_hook = XEN_DEFINE_HOOK(S_enved_hook, 5, H_enved_hook);

  ss->enved = new_env_editor();
  FREE(ss->enved->axis);
  ss->enved->axis = NULL;
  ss->enved->in_dB = DEFAULT_ENVED_IN_DB;
  ss->enved->clip_p = DEFAULT_ENVED_CLIP_P;

#if MUS_DEBUGGING && HAVE_SCHEME
  XEN_DEFINE_PROCEDURE("window-env", g_window_env_w, 5, 1, 0, "internal testing function");
  XEN_DEFINE_PROCEDURE("multiply-envs", g_multiply_envs_w, 3, 0, 0, "internal testing function");
  XEN_DEFINE_PROCEDURE("invert-env", g_invert_env_w, 1, 0, 0, "internal testing function");
#endif
}
