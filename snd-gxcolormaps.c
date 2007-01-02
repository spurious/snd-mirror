#include "snd.h"

typedef struct {
  int size;
  char *name;
  unsigned short *r, *g, *b;
  XEN lambda;
  int gc_loc;
  Float** (*make_rgb)(int size, XEN func);
} cmap;

static cmap **cmaps = NULL;
static int cmaps_size = 0;

#define NO_SUCH_COLORMAP XEN_ERROR_TYPE("no-such-colormap")

bool is_colormap(int n)
{
  return((n >= 0) &&
	 (n < cmaps_size) && 
	 (cmaps[n]));
}

char *colormap_name(int n) 
{
  if (is_colormap(n))
    return(cmaps[n]->name); 
  return(NULL);
}

int num_colormaps(void)
{
  int i;
  for (i = cmaps_size - 1; i >= 0; i--)
    if (cmaps[i])
      return(i + 1);
  return(0);
}

static cmap *delete_cmap(int index)
{
  if (is_colormap(index))
    {
      cmap *c;
      c = cmaps[index];
      if (c->r) FREE(c->r);
      if (c->g) FREE(c->g);
      if (c->b) FREE(c->b);
      if (c->name) FREE(c->name);
      if (XEN_PROCEDURE_P(c->lambda))
	snd_unprotect_at(c->gc_loc);
      FREE(c);
      cmaps[index] = NULL;
    }
  return(NULL);
}

static unsigned short *Floats_to_ushorts(int size, Float *data)
{
  int i;
  unsigned short *new_data;
  new_data = (unsigned short *)CALLOC(size, sizeof(unsigned short));
  for (i = 0; i < size; i++)
    new_data[i] = (unsigned short)floor(65535 * data[i]);
  return(new_data);
}

void get_current_color(int index, int n, unsigned short *r, unsigned short *g, unsigned short *b)
{
  if (is_colormap(index))
    {
      cmap *c;
      c = cmaps[index];
      if (color_map_size(ss) != c->size)
	{
	  Float **rgb;
	  int i;
	  /* release old colormap data */
	  if (c->r) FREE(c->r);
	  if (c->g) FREE(c->g);
	  if (c->b) FREE(c->b);
	  c->size = color_map_size(ss);
	  /* make new data */
	  rgb = (*(c->make_rgb))(c->size, c->lambda);
	  c->r = Floats_to_ushorts(c->size, rgb[0]);
	  c->g = Floats_to_ushorts(c->size, rgb[1]);
	  c->b = Floats_to_ushorts(c->size, rgb[2]);
	  for (i = 0; i < 3; i++) FREE(rgb[i]);
	  FREE(rgb);
	}
      if (n < c->size)
	{
	  (*r) = c->r[n];
	  (*g) = c->g[n];
	  (*b) = c->b[n];
	}
    }
}

static cmap *new_cmap(const char *name, int size, Float **rgb)
{
  cmap *c = NULL;
  c = (cmap *)CALLOC(1, sizeof(cmap));
  c->name = copy_string(name);
  c->size = size;
  c->r = Floats_to_ushorts(size, rgb[0]);
  c->g = Floats_to_ushorts(size, rgb[1]);
  c->b = Floats_to_ushorts(size, rgb[2]);
  return(c);
}

static cmap *make_builtin_cmap(int size, const char *name, Float** (*make_rgb)(int size, XEN ignored))
{
  Float **rgb;
  cmap *c = NULL;
  rgb = make_rgb(size, XEN_FALSE);
  if (rgb)
    {
      int i;
      c = new_cmap(name, size, rgb);
      c->make_rgb = make_rgb;
      c->lambda = XEN_FALSE;
      c->gc_loc = NOT_A_GC_LOC;
      for (i = 0; i < 3; i++) FREE(rgb[i]);
      FREE(rgb);
    }
  return(c);
}

static Float **make_base_rgb(int size)
{
  Float **rgb;
  int i;
  rgb = (Float **)CALLOC(3, sizeof(Float *));
  for (i = 0; i < 3; i++) rgb[i] = (Float *)CALLOC(size, sizeof(Float));
  return(rgb);
}

static char *add_colormap_func_error_msg = NULL;
static bool add_colormap_func_hit_error = false;
static void add_colormap_func_error(const char *msg, void *data)
{
  add_colormap_func_hit_error = true;
  add_colormap_func_error_msg = copy_string(msg); /* msg itself is freed by the error handler in snd-xen.c */
}

static Float **make_xen_colormap(int size, XEN lambda)
{
  XEN xrgb;
  Float **rgb = NULL;
  add_colormap_func_hit_error = false;
  redirect_xen_error_to(add_colormap_func_error, NULL);
  xrgb = XEN_CALL_1(lambda,
		    C_TO_XEN_INT(size),
		    S_add_colormap);
  redirect_xen_error_to(NULL, NULL);
  if (add_colormap_func_hit_error)
    {
      XEN str;
      if (add_colormap_func_error_msg)
	{
	  str = C_TO_XEN_STRING(add_colormap_func_error_msg);
	  FREE(add_colormap_func_error_msg);
	  add_colormap_func_error_msg = NULL;
	}
      else str = XEN_FALSE;
      XEN_ERROR(XEN_ERROR_TYPE("colormap-error"),
		XEN_LIST_3(C_TO_XEN_STRING(S_add_colormap),
			   C_TO_XEN_STRING("function error:"),
			   str));
    }
  if (XEN_LIST_P(xrgb))
    {
      vct *xr, *xg, *xb;
      int i, gc_loc;
      /* user-defined colormap func returns a list of 3 vcts (r g b) */
      gc_loc = snd_protect(xrgb);
      if (!(mus_vct_p(XEN_LIST_REF(xrgb, 0)))) 
	XEN_ERROR(XEN_ERROR_TYPE("colormap-error"),
		  XEN_LIST_3(C_TO_XEN_STRING(S_add_colormap),
			     C_TO_XEN_STRING("function did not return a list of vcts!"),
			     xrgb));
      xr = XEN_TO_VCT(XEN_LIST_REF(xrgb, 0));
      if (xr->length < size)
	XEN_ERROR(XEN_ERROR_TYPE("colormap-error"),
		  XEN_LIST_3(C_TO_XEN_STRING(S_add_colormap),
			     C_TO_XEN_STRING("function did not return a list of vcts of the correct size"),
			     xrgb));
      xg = XEN_TO_VCT(XEN_LIST_REF(xrgb, 1));
      xb = XEN_TO_VCT(XEN_LIST_REF(xrgb, 2));
      rgb = make_base_rgb(size);
      for (i = 0; i < size; i++)
	{
	  rgb[0][i] = xr->data[i];
	  rgb[1][i] = xg->data[i];
	  rgb[2][i] = xb->data[i];
	}
      snd_unprotect_at(gc_loc);
    }
  else XEN_ERROR(XEN_ERROR_TYPE("colormap-error"),
		 XEN_LIST_3(C_TO_XEN_STRING(S_add_colormap),
			    C_TO_XEN_STRING("colormap func must return a list of 3 vcts"),
			    lambda));
  return(rgb);
}

static int add_colormap(char *name, XEN func)
{
  cmap *c;
  Float **rgb;
  int index = -1, i, loc;
  for (i = 0; i < cmaps_size; i++)
    if (!(cmaps[i]))
      {
	index = i;
	break;
      }
  if (index == -1) /* no free slot */
    {
      index = cmaps_size;
      cmaps_size += 8;
      cmaps = (cmap **)REALLOC(cmaps, cmaps_size * sizeof(cmap *));
      for (i = index; i < cmaps_size; i++) cmaps[i] = NULL;
    }
  loc = snd_protect(func);
  rgb = make_xen_colormap(color_map_size(ss), func);
  c = new_cmap(name, color_map_size(ss), rgb);
  c->make_rgb = make_xen_colormap;
  c->lambda = func;
  c->gc_loc = loc;
  for (i = 0; i < 3; i++) FREE(rgb[i]);
  FREE(rgb);
  cmaps[index] = c;
  return(index);
}


static Float **make_black_and_white_colormap(int size, XEN ignored)
{
  /* (r 0) (g 0) (b 0) */
  return(make_base_rgb(size));
}

/* colormap functions taken mostly from (GPL) octave-forge code written by Kai Habel <kai.habel@gmx.de> */

static Float **make_gray_colormap(int size, XEN ignored)
{
  /* (r x) (g x) (b x) */
  Float **rgb;
  int i;
  Float x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (Float)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = x;
      rgb[1][i] = x;
      rgb[2][i] = x;
    }
  return(rgb);
}

static Float **make_autumn_colormap(int size, XEN ignored)
{
  /* (r 1.0) (g x) (b 0.0) */
  Float **rgb;
  int i;
  Float x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (Float)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = 1.0;
      rgb[1][i] = x;
      rgb[2][i] = 0.0;
    }
  return(rgb);
}

static Float **make_spring_colormap(int size, XEN ignored)
{
  /* (r 1.0) (g x) (b (- 1.0 x)) */
  Float **rgb;
  int i;
  Float x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (Float)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = 1.0;
      rgb[1][i] = x;
      rgb[2][i] = 1.0 - x;
    }
  return(rgb);
}

static Float **make_winter_colormap(int size, XEN ignored)
{
  /* (r 0.0) (g x) (b (- 1.0 (/ x 2))) */
  Float **rgb;
  int i;
  Float x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (Float)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = 0.0;
      rgb[1][i] = x;
      rgb[2][i] = 1.0 - (x * 0.5);
    }
  return(rgb);
}

static Float **make_summer_colormap(int size, XEN ignored)
{
  /* (r x) (g (+ 0.5 (/ x 2))) (b 0.4) */
  Float **rgb;
  int i;
  Float x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (Float)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = x;
      rgb[1][i] = 0.5 + (0.5 * x);
      rgb[2][i] = 0.4;
    }
  return(rgb);
}

static Float **make_cool_colormap(int size, XEN ignored)
{
  /* (r x) (g (- 1.0 x)) (b 1.0) */
  Float **rgb;
  int i;
  Float x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (Float)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = x;
      rgb[1][i] = 1.0 - x;
      rgb[2][i] = 1.0;
    }
  return(rgb);
}

static Float **make_copper_colormap(int size, XEN ignored)
{
  /* (r (if (< x 4/5) (* 5/4 x) 1.0)) (g (* 4/5 x)) (b (* 1/2 x)) */
  Float **rgb;
  int i;
  Float x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (Float)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = (x < 0.8) ? (1.25 * x) : 1.0;
      rgb[1][i] = 0.8 * x;
      rgb[2][i] = 0.5 * x;
    }
  return(rgb);
}

static Float **make_flag_colormap(int size, XEN ignored)
{
  /* (r (if (or (= k 0) (= k 1)) 1.0 0.0)) (g (if (= k 1) 1.0 0.0)) (b (if (or (= k 1) (= k 2)) 1.0 0.0)) */
  Float **rgb;
  int i, k = 0;
  rgb = make_base_rgb(size);
  for (i = 0; i < size; i++)
    {
      rgb[0][i] = (k < 2) ? 1.0 : 0.0;
      rgb[1][i] = (k == 1) ? 1.0 : 0.0;
      rgb[2][i] = ((k == 1) || (k == 2)) ? 1.0 : 0.0;
      k++;
      if (k == 4) k = 0;
    }
  return(rgb);
}

static Float **make_prism_colormap(int size, XEN ignored)
{
  /* (r (list-ref (list 1 1 1 0 0 2/3) k)) (g (list-ref (list 0 1/2 1 1 0 0) k)) (b (list-ref (list 0 0 0 0 1 1) k)) */
  Float **rgb;
  int i, k = 0;
  Float rs[6] = {1.0, 1.0, 1.0, 0.0, 0.0, 0.6667};
  Float gs[6] = {0.0, 0.5, 1.0, 1.0, 0.0, 0.0};
  Float bs[6] = {0.0, 0.0, 0.0, 0.0, 1.0, 1.0};
  rgb = make_base_rgb(size);
  for (i = 0; i < size; i++)
    {
      rgb[0][i] = rs[k];
      rgb[1][i] = gs[k];
      rgb[2][i] = bs[k];
      k++;
      if (k == 6) k = 0;
    }
  return(rgb);
}

static Float **make_bone_colormap(int size, XEN ignored)
{
  /* (r (if (< x 3/4) (* 7/8 x) (- (* 11/8 x) 3/8)))
     (g (if (< x 3/8) (* 7/8 x) (if (< x 3/4) (- (* 29/24 x) 1/8) (+ (* 7/8 x) 1/8))))
     (b (if (< x 3/8) (* 29/24 x) (+ (* 7/8 x) 1/8)))
  */
  Float **rgb;
  int i;
  Float x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (Float)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = (x < .75) ? (x * .875) : ((x * 11.0 / 8.0) - .375);
      rgb[1][i] = (x < .375) ? (x * .875) : ((x < .75) ? ((x * 29.0 / 24.0) - .125) : ((x * .875) + .125));
      rgb[2][i] = (x < .375) ? (x * 29.0 / 24.0) : ((x * .875) + .125);
    }
  return(rgb);
}

static Float **make_hot_colormap(int size, XEN ignored)
{
  /* (mimicking matlab here, not octave)
     (r (if (< x 3/8) (* 8/3 x) 1.0))
     (g (if (< x 3/8) 0.0 (if (< x 3/4) (- (* 8/3 x) 1.0) 1.0)))
     (b (if (< x 3/4) 0.0 (- (* 4 x) 3)))
  */
  Float **rgb;
  int i;
  Float x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (Float)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = (x < .375) ? (x * 8.0 / 3.0) : 1.0;
      rgb[1][i] = (x < .375) ? 0.0 : ((x < .75) ? ((x * 8.0 / 3.0) - 1.0) : 1.0);
      rgb[2][i] = (x < .75) ? 0.0 : ((x * 4.0) - 3.0);
    }
  return(rgb);
}

static Float **make_jet_colormap(int size, XEN ignored)
{
  /* 
     (r (if (< x 3/8) 0.0 (if (< x 5/8) (- (* 4 x) 3/2) (if (< x 7/8) 1.0 (+ (* -4 x) 9/2)))))
     (g (if (< x 1/8) 0.0 (if (< x 3/8)	(- (* 4 x) 1/2)	(if (< x 5/8) 1.0 (if (< x 7/8)	(+ (* -4 x) 7/2) 0.0)))))
     (b (if (< x 1/8) (+ (* 4 x) 1/2) (if (< x 3/8) 1.0	(if (< x 5/8) (+ (* -4 x) 5/2) 0.0))))
  */
  Float **rgb;
  int i;
  Float x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (Float)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = (x < .375) ? 0.0 : ((x < .625) ? ((x * 4.0) - 1.5) : ((x < .875) ? 1.0 : ((x * -4.0) + 4.5)));
      rgb[1][i] = (x < .125) ? 0.0 : ((x < .375) ? ((x * 4.0) - 1.5) : ((x < .625) ? 1.0 : ((x < .875) ? ((x * -4.0) + 3.5) : 0.0)));
      rgb[2][i] = (x < .125) ? ((x * 4.0) + 0.5) : ((x < .375) ? 1.0 : ((x < .625) ? ((x * -4.0) + 2.5) : 0.0));
    }
  return(rgb);
}

static Float **make_pink_colormap(int size, XEN ignored)
{
  /* matlab uses log here, or something like that -- this version is quite different
     (r (if (< x 3/8) (* 14/9 x) (+ (* 2/3 x) 1/3)))
     (g (if (< x 3/8) (* 2/3 x) (if (< x 3/4) (- (* 14/9 x) 1/3) (+ (* 2/3 x) 1/3))))
     (b (if (< x 3/4) (* 2/3 x) (- (* 2 x) 1)))			
  */
  Float **rgb;
  int i;
  Float x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (Float)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = (x < .375) ? (x * 14.0 / 9.0) : ((x * 2.0 / 3.0) + 1.0 / 3.0);
      rgb[1][i] = (x < .375) ? (x * 2.0 / 3.0) : ((x < .75) ? ((x * 14.0 / 9.0) - 1.0 / 3.0) : ((x * 2.0 / 3.0) + 1.0 / 3.0));
      rgb[2][i] = (x < .75) ? (x * 2.0 / 3.0) : ((x * 2.0) - 1.0);
    }
  return(rgb);
}

static Float **make_rainbow_colormap(int size, XEN ignored)
{
  /* 
     (r (if (< x 2/5) 1.0 (if (< x 3/5) (+ (* -5 x) 3) (if (< x 4/5) 0.0 (- (* 10/3 x) 8/3)))))
     (g (if (< x 2/5) (* 5/2 x) (if (< x 3/5) 1.0 (if (< x 4/5) (+ (* -5 x) 4) 0.0))))
     (b (if (< x 3/5) 0.0 (if (< x 4/5)	(- (* 5 x) 3) 1.0)))
  */
  Float **rgb;
  int i;
  Float x, incr;
  rgb = make_base_rgb(size);
  incr = 1.0 / (Float)size;
  for (i = 0, x = 0.0; i < size; i++, x += incr)
    {
      rgb[0][i] = (x < .4) ? 1.0 : ((x < .6) ? ((x * -5.0) + 3.0) : ((x < .8) ? 0.0 : ((x * 10.0 / 3.0) - 8.0 / 3.0)));
      rgb[1][i] = (x < .4) ? (x * 2.5) : ((x < .6) ? 1.0 : ((x < .8) ? ((x * -5.0) + 4.0) : 0.0));
      rgb[2][i] = (x < .6) ? 0.0 : ((x < .8) ? ((x * 5.0) - 3.0) : 1.0);
    }
  return(rgb);
}

static XEN g_colormap_ref(XEN map, XEN pos)
{
  int index;
  Float x;
  #define H_colormap_ref "(" S_colormap_ref " index pos): (list r g b). Index is the colormap \
index (the value of " S_colormap " for example).  Pos is between 0.0 and 1.0."
  unsigned short r = 0, g = 0, b = 0;
  XEN_ASSERT_TYPE(XEN_INTEGER_P(map), map, XEN_ARG_1, S_colormap_ref, "an integer");
  XEN_ASSERT_TYPE(XEN_NUMBER_P(pos), pos, XEN_ARG_2, S_colormap_ref, "a number between 0.0 and 1.0");
  index = XEN_TO_C_INT(map);
  if (!(is_colormap(index)))
    XEN_ERROR(NO_SUCH_COLORMAP,
	      XEN_LIST_2(C_TO_XEN_STRING(S_colormap_ref),
			 map));
  x = XEN_TO_C_DOUBLE(pos);
  if ((x < 0.0) || (x > 1.0))
    XEN_OUT_OF_RANGE_ERROR(S_colormap_ref, 2, pos, "x must be between 0.0 and 1.0: ~A");
  get_current_color(index, (int)(color_map_size(ss) * x + 0.5), &r, &g, &b);
  return(XEN_LIST_3(C_TO_XEN_DOUBLE((float)r / 65535.0),
		    C_TO_XEN_DOUBLE((float)g / 65535.0),
		    C_TO_XEN_DOUBLE((float)b / 65535.0)));
}

/* can't use Colormap -- it's the X type name */

static XEN g_colormap(void) {return(C_TO_XEN_INT(color_map(ss)));}
static XEN g_set_colormap(XEN val) 
{
  int index;
  #define H_colormap "(" S_colormap "): current colormap choice. \
This should be an integer between 0 and the current colormap table top (default: 15).  The maps (from 0 to 15) are: \
black-and-white, gray, hot, cool, bone, copper, pink, jet, prism, autumn, winter, \
spring, summer, rainbow, and flag.  These names are defined in rgb.scm."
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_colormap, "an integer"); 
  index = XEN_TO_C_INT(val);
  if (!(is_colormap(index)))
    XEN_ERROR(NO_SUCH_COLORMAP,
	      XEN_LIST_2(C_TO_XEN_STRING(S_colormap),
			 val));
  set_color_map(index); /* this normally redisplays */
  return(C_TO_XEN_INT(color_map(ss)));
}

static XEN g_colormap_size(void) {return(C_TO_XEN_INT(color_map_size(ss)));}
static XEN g_set_colormap_size(XEN val) 
{
  int maps;
  #define H_colormap_size "(" S_colormap_size "): current colormap size; default is 512."
  XEN_ASSERT_TYPE(XEN_INTEGER_P(val), val, XEN_ONLY_ARG, S_setB S_colormap_size, "an integer"); 
  maps = XEN_TO_C_INT(val);
  if (maps < 0)
    XEN_OUT_OF_RANGE_ERROR(S_setB S_colormap_size, 1, val, "size ~A < 0?");
  if (maps > (1 << 26))
    XEN_OUT_OF_RANGE_ERROR(S_setB S_colormap_size, 1, val, "size ~A too large");
  set_color_map_size(maps);
  check_colormap_sizes(color_map_size(ss));
  return(C_TO_XEN_INT(color_map_size(ss)));
}

static XEN g_colormap_name(XEN index)
{
  int map;
  #define H_colormap_name "(" S_colormap_name " index) returns the colormap's name."
  XEN_ASSERT_TYPE(XEN_INTEGER_P(index), index, XEN_ONLY_ARG, S_colormap_name, "an integer"); 
  map = XEN_TO_C_INT(index);
  if (!(is_colormap(map)))
    XEN_ERROR(NO_SUCH_COLORMAP,
	      XEN_LIST_2(C_TO_XEN_STRING(S_colormap_name),
			 index));
  return(C_TO_XEN_STRING(cmaps[map]->name));
}

static XEN g_colormap_p(XEN index)
{
  #define H_colormap_p "(" S_colormap_p " index) -> " PROC_TRUE " if index represents a usable colormap."
  XEN_ASSERT_TYPE(XEN_INTEGER_P(index), index, XEN_ONLY_ARG, S_colormap_p, "an integer"); 
  return(C_TO_XEN_BOOLEAN(is_colormap(XEN_TO_C_INT(index))));
}

static XEN g_delete_colormap(XEN index)
{
  int map;
  #define H_delete_colormap "(" S_delete_colormap " index) removes the specified colormap."
  XEN_ASSERT_TYPE(XEN_INTEGER_P(index), index, XEN_ONLY_ARG, S_delete_colormap, "an integer"); 
  map = XEN_TO_C_INT(index);
  if (!(is_colormap(map)))
    XEN_ERROR(NO_SUCH_COLORMAP,
	      XEN_LIST_2(C_TO_XEN_STRING(S_delete_colormap),
			 index));
  delete_cmap(map);
  reflect_color_list(false);
  if (color_map(ss) == map) set_color_map(DEFAULT_COLOR_MAP);
  return(index);
}

#include "clm2xen.h"

static XEN g_add_colormap(XEN name, XEN func)
{
  int index;
  #define H_add_colormap "(" S_add_colormap " name func) adds the colormap created by func to the colormap table, \
returning the new index."
  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, XEN_ARG_1, S_add_colormap, "a string"); 
  XEN_ASSERT_TYPE(XEN_PROCEDURE_P(func) && (!mus_xen_p(func)), func, XEN_ARG_2, S_add_colormap, "a function of 2 args");
  if (!(procedure_arity_ok(func, 1)))
    return(snd_bad_arity_error(S_add_colormap, 
			       C_TO_XEN_STRING("func should take 1 arg"), 
			       func));
  index = add_colormap(XEN_TO_C_STRING(name), func);
  reflect_color_list(false);
  return(C_TO_XEN_INT(index));
}


#ifdef XEN_ARGIFY_1
XEN_NARGIFY_2(g_colormap_ref_w, g_colormap_ref)
XEN_NARGIFY_0(g_colormap_w, g_colormap)
XEN_NARGIFY_1(g_colormap_p_w, g_colormap_p)
XEN_NARGIFY_1(g_set_colormap_w, g_set_colormap)
XEN_NARGIFY_0(g_colormap_size_w, g_colormap_size)
XEN_NARGIFY_1(g_set_colormap_size_w, g_set_colormap_size)
XEN_NARGIFY_1(g_colormap_name_w, g_colormap_name)
XEN_NARGIFY_1(g_delete_colormap_w, g_delete_colormap)
XEN_NARGIFY_2(g_add_colormap_w, g_add_colormap)
#else
#define g_colormap_ref_w g_colormap_ref
#define g_colormap_w g_colormap
#define g_colormap_p_w g_colormap_p
#define g_set_colormap_w g_set_colormap
#define g_colormap_size_w g_colormap_size
#define g_set_colormap_size_w g_set_colormap_size
#define g_colormap_name_w g_colormap_name
#define g_delete_colormap_w g_delete_colormap
#define g_add_colormap_w g_add_colormap
#endif

enum {BLACK_AND_WHITE_COLORMAP, GRAY_COLORMAP, HOT_COLORMAP, COOL_COLORMAP, BONE_COLORMAP, COPPER_COLORMAP, PINK_COLORMAP, JET_COLORMAP, PRISM_COLORMAP,
      AUTUMN_COLORMAP, WINTER_COLORMAP, SPRING_COLORMAP, SUMMER_COLORMAP, RAINBOW_COLORMAP, FLAG_COLORMAP};

void g_init_gxcolormaps(void)
{
  cmaps_size = 16;
  cmaps = (cmap **)CALLOC(cmaps_size, sizeof(cmap *));
  /* these are just place-holders */
  cmaps[BLACK_AND_WHITE_COLORMAP] = make_builtin_cmap(1, _("black-and-white"), make_black_and_white_colormap); 
  cmaps[GRAY_COLORMAP] =    make_builtin_cmap(1, _("gray"), make_gray_colormap); 
  cmaps[AUTUMN_COLORMAP] =  make_builtin_cmap(1, _("autumn"), make_autumn_colormap); 
  cmaps[SPRING_COLORMAP] =  make_builtin_cmap(1, _("spring"), make_spring_colormap); 
  cmaps[WINTER_COLORMAP] =  make_builtin_cmap(1, _("winter"), make_winter_colormap); 
  cmaps[SUMMER_COLORMAP] =  make_builtin_cmap(1, _("summer"), make_summer_colormap); 
  cmaps[COOL_COLORMAP] =    make_builtin_cmap(1, _("cool"), make_cool_colormap); 
  cmaps[COPPER_COLORMAP] =  make_builtin_cmap(1, _("copper"), make_copper_colormap); 
  cmaps[FLAG_COLORMAP] =    make_builtin_cmap(1, _("flag"), make_flag_colormap); 
  cmaps[PRISM_COLORMAP] =   make_builtin_cmap(1, _("prism"), make_prism_colormap); 
  cmaps[BONE_COLORMAP] =    make_builtin_cmap(1, _("bone"), make_bone_colormap); 
  cmaps[HOT_COLORMAP] =     make_builtin_cmap(1, _("hot"), make_hot_colormap); 
  cmaps[JET_COLORMAP] =     make_builtin_cmap(1, _("jet"), make_jet_colormap); 
  cmaps[PINK_COLORMAP] =    make_builtin_cmap(1, _("pink"), make_pink_colormap); 
  cmaps[RAINBOW_COLORMAP] = make_builtin_cmap(1, _("rainbow"), make_rainbow_colormap); 

  XEN_DEFINE_PROCEDURE(S_colormap_p, g_colormap_p_w,           1, 0, 0, H_colormap_p);
  XEN_DEFINE_PROCEDURE(S_colormap_ref, g_colormap_ref_w,       2, 0, 0, H_colormap_ref);
  XEN_DEFINE_PROCEDURE(S_add_colormap, g_add_colormap_w,       2, 0, 0, H_add_colormap);
  XEN_DEFINE_PROCEDURE(S_colormap_name, g_colormap_name_w,     1, 0, 0, H_colormap_name);
  XEN_DEFINE_PROCEDURE(S_delete_colormap, g_delete_colormap_w, 1, 0, 0, H_delete_colormap);

  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_colormap,      g_colormap_w,      H_colormap,      S_setB S_colormap,      g_set_colormap_w,      0, 0, 1, 0);
  XEN_DEFINE_PROCEDURE_WITH_SETTER(S_colormap_size, g_colormap_size_w, H_colormap_size, S_setB S_colormap_size, g_set_colormap_size_w, 0, 0, 1, 0);
}
