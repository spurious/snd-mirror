#include "snd.h"

typedef struct {
  Float hi,lo; int max_ticks;
  Float flo,fhi,mlo,mhi,step,tenstep;
  int tens,min_label_width,max_label_width;
  int min_label_x,min_label_y,max_label_x,max_label_y,maj_tick_len,min_tick_len;
  char *min_label,*max_label;
} tick_descriptor;

static tick_descriptor *free_tick_descriptor (tick_descriptor *td)
{
  if (td)
    {
      if (td->min_label) {FREE(td->min_label); td->min_label = NULL;}
      if (td->max_label) {FREE(td->max_label); td->max_label = NULL;}
      FREE(td);
    }
  return(NULL);
}

static tick_descriptor *describe_ticks(tick_descriptor *gd_td, Float lo, Float hi, int max_ticks)
{
  /* given absolute (unchangeable) axis bounds lo and hi, and abolute maximum number of ticks to use, find a "pretty" tick placement */
  /* much of the work here involves floating point rounding problems.  We assume the tick labeller will round as well */
  tick_descriptor *td;

  int ten,hib,lob;
  double flog10,plog10;
  Float frac,ften,hilo_diff,eten,flt_ten,flt_ften;

  Float inside,mfdiv,mten,mften;
  int mticks,mdiv;

  if (!gd_td)
    td = (tick_descriptor *)CALLOC(1,sizeof(tick_descriptor));
  else 
    {
      td = gd_td;
      if ((td->hi == hi) && (td->lo == lo) && (td->max_ticks == max_ticks)) return(td);
    }
  td->hi = hi;
  td->lo = lo;
  td->max_ticks = max_ticks;
  hilo_diff = hi-lo;
  flt_ten=log10(hilo_diff);
  ten=(int)floor(flt_ten);
  frac=flt_ten-ten;
  if (frac>.9999) ten++;
  eten=pow(10,ten);
  hib=(int)floor(hi/eten);
  lob=(int)ceil(lo/eten);
  if (lob != hib) 
    {
      td->mlo=(Float)(lob*eten);
      td->mhi=(Float)(hib*eten);
    }
  else
    {
      /* try next lower power of ten */
      ften = eten*.1;
      flt_ften=(hi/ften);
      hib=(int)floor(flt_ften);
      frac=flt_ften-hib;
      if (frac > .9999) hib++;
      lob=(int)ceil(lo/ften);
      td->mlo=(Float)(lob*ften);
      td->mhi=(Float)(hib*ften);
    }
  inside = (td->mhi-td->mlo)/hilo_diff;
  mticks = (int)floor(inside*max_ticks);
  if (mticks <= 1) mdiv=1;
  if (mticks<3) mdiv=mticks;
  else if (mticks == 3) mdiv=2;
  else if (mticks < 6) mdiv=mticks;
  else if (mticks < 10) mdiv=5;
  else mdiv = (int)(10*floor(mticks/10));
  mfdiv = (td->mhi-td->mlo)/mdiv;
  flog10 = floor(log10(mfdiv));
  plog10 = pow(10,flog10);
  td->tens = (int)fabs(flog10);
  mten = (Float)(floor(4.0*(.00001+(mfdiv/plog10))))/4.0;
  if (mten < 1.0) mten = 1.0;
  if ((mten == 1.0) || (mten == 2.0) || (mten == 2.5) || (mten == 5.0)) ften = mten;
  else if (mten < 2.0) ften=2.0;
  else if (mten < 2.5) ften=2.5;
  else if (mten < 5.0) ften=5.0;
  else ften=10.0;
  td->tenstep = ften;
  mften = ften*plog10;
  td->step = mften;
  flt_ten=lo/mften;
  lob=(int)ceil(flt_ten);
  frac=lob-flt_ten;
  if (frac > .9999) lob--;
  td->flo=lob*mften;
  flt_ten=(hi/mften);
  hib=(int)floor(flt_ten);
  frac=flt_ten-hib;
  if (frac > .9999) hib++;
  td->fhi=hib*mften;
  return(td);
}

axis_info *free_axis_info(axis_info *ap)
{
  if (ap->x_ticks) ap->x_ticks = (void *)free_tick_descriptor((tick_descriptor *)(ap->x_ticks));
  if (ap->y_ticks) ap->y_ticks = (void *)free_tick_descriptor((tick_descriptor *)(ap->y_ticks));
  if (ap->ax) ap->ax = free_axis_context(ap->ax);
  if (ap->xlabel) {FREE(ap->xlabel); ap->xlabel = NULL;}
  if (ap->ylabel) {FREE(ap->ylabel); ap->ylabel = NULL;}
  FREE(ap);
  return(NULL);
}

static short shorten(int n)
{
  if (n>=-32768) 
    {
      if (n<32768) return(n);
      else return(32767);
    }
  else return(-32768);
}

short grf_x(double val, axis_info *ap)
{
  if (val >= ap->x1) return(ap->x_axis_x1);
  if (val <= ap->x0) return(ap->x_axis_x0);
  return((short)(ap->x_axis_x0 + (val - ap->x0) * ap->x_scale));
}

short grf_y(Float val, axis_info *ap)
{
  if (val >= ap->y1) return(ap->y_axis_y1);
  if (val <= ap->y0) return(ap->y_axis_y0);
  return((short)(ap->y_axis_y0 + (val - ap->y0) * ap->y_scale));
}

double ungrf_x(axis_info *ap, int x)
{
  return(ap->x0+(double)(x - ap->x_axis_x0)/ap->x_scale);
}

Float ungrf_y(axis_info *ap, int y)
{
  return(ap->y0+(Float)(y - ap->y_axis_y0)/ap->y_scale);
}

static short tick_grf_x(double val, axis_info *ap, int style, int srate)
{
  switch (style)
    {
    case X_IN_SECONDS: return(shorten((int)(ap->x_axis_x0+(val-ap->x0)*ap->x_scale))); break;
    case X_IN_SAMPLES: return(shorten((int)(ap->x_axis_x0+(val-ap->x0*srate)*ap->x_scale/srate))); break;
    case X_TO_ONE: return(shorten((int)(ap->x_axis_x0+(val-ap->x0/ap->xmax)*ap->x_scale*ap->xmax))); break;
    default: return(shorten((int)(ap->x_axis_x0+(val-ap->x0)*ap->x_scale))); break;
    }
}


enum {axis_x_bottom,axis_x_middle};

void make_axes_1(chan_info *cp, axis_info *ap, int x_style, int srate)
{
  int width,height;
  int axis_style;                 /* x_bottom or x_middle or xy_middle => |_ or |- or + */
  Float x_range,y_range,non_label_room;
  int axis_thickness,left_border_width,bottom_border_width,top_border_width,right_border_width;
  int inner_border_width,tick_label_width;
  int major_tick_length,minor_tick_length,x_tick_spacing,y_tick_spacing;
  int include_x_label,include_x_ticks,include_x_tick_labels,include_y_label,include_y_ticks,include_y_tick_labels;
  int x_label_width,x_label_height,y_label_height,y_label_width,x_number_height;
  int num_ticks;
  tick_descriptor *tdx = NULL,*tdy = NULL;
  int curx, cury, curdy, show_x_axis = 1;
  axis_context *ax;
  snd_info *sp;
  sp = cp->sound;
  ax = ap->ax;
  width = ap->width;
  height = ap->height;
  ap->graph_active = ((width > 4) || (height > 10));

  if (((sp) && (cp->show_axes == SHOW_NO_AXES)) || (width < 40) || (height < 40))
    {
      /* ap->graph_active = 0; */
      /* leave it set up for bare graph */
      if (height > 100)
	{
	  ap->y_axis_y0 = ap->y_offset + height - 20;
	  ap->y_axis_y1 = ap->y_offset + 10;
	}
      else
	{
	  ap->y_axis_y0 = ap->y_offset + (int)(0.8*height);
	  ap->y_axis_y1 = ap->y_offset + (int)(0.1*height);
	}
      if (width > 100)
	{
	  ap->x_axis_x1 = ap->graph_x0 + width - 10;
	  ap->x_axis_x0 = ap->graph_x0 + 20;
	}
      else
	{
	  ap->x_axis_x1 = ap->graph_x0 + (int)(0.9*width);
	  ap->x_axis_x0 = ap->graph_x0 + (int)(0.2*width);
	}
      ap->x_scale = ((double)(ap->x_axis_x1 - ap->x_axis_x0))/((double)(ap->x1 - ap->x0));
      ap->y_scale = (ap->y_axis_y1 - ap->y_axis_y0)/(ap->y1 - ap->y0);
      return;
    }

  show_x_axis = ((sp == NULL) || (sp->combining != CHANNELS_COMBINED) || (cp->show_axes == SHOW_ALL_AXES) || (cp->chan == (sp->nchans - 1)));
  /* sp is null in the control panel filter envelope display */

  left_border_width = 10;
  bottom_border_width = 14;
  top_border_width = 10;
  right_border_width = 14;
  inner_border_width = 5;
  x_tick_spacing = 20; if (width < 250) x_tick_spacing = 10 + (width/25);
  y_tick_spacing = 20; if (height < 250) y_tick_spacing = 10 + (height/25);
  major_tick_length = 9;
  minor_tick_length = 5;
  axis_thickness = 2;
  axis_style = axis_x_bottom;
  if (show_x_axis)
    {
      include_x_label = ((ap->xlabel) && ((height > 100) && (width > 100)));
      include_x_tick_labels = ((height > 60) && (width > 100));
      include_x_ticks = ((height > 40) && (width > 40));
    }
  else
    {
      include_x_label = 0;
      include_x_tick_labels = 0;
      include_x_ticks = 0;
    }
  /* include_y_label = ((ap->ylabel) && ((width > 100) && (height > 100))); */
  include_y_label = 0; /* it looks dumber and dumber... */
  if ((sp == NULL) || (cp->show_axes != SHOW_X_AXIS))
    {
      include_y_tick_labels = ((width > 100) && (height > 60));
      include_y_ticks = ((width > 100) && (height > 40));
    }
  else
    {
      include_y_tick_labels = 0;
      include_y_ticks = 0;
    }

  curx=left_border_width;
  cury=height-bottom_border_width;
  
  x_number_height = number_height(ax);
  x_label_height = 0;
  y_label_height = 0;
  x_label_width = 0;
  y_label_width = 0;

  if (include_y_label)
    {
      /* center y label rotated 90 degress */
      y_label_height = label_width(ax,ap->ylabel);
      if ((y_label_height+bottom_border_width+top_border_width) > ((int)(.8*height)))
	{
	  include_y_label = 0;
	  y_label_height = 0;
	  y_label_width = 0;
	}
      else
	{
	  y_label_width = label_height(ax);
	}
    }
  if (include_x_label)
    {
      x_label_width = label_width(ax,ap->xlabel);
      if ((x_label_width+curx+right_border_width) > width)
	{
	  include_x_label = 0;
	  x_label_width = 0;
	  x_label_height = 0;
	}
      else
	{
	  x_label_height = label_height(ax);
	}
    }
  else
    if (include_x_ticks) 
      x_label_height = label_height(ax);

  curdy = cury;
  if (include_y_label)
    {
      if (include_x_label) curdy -= (x_label_height+inner_border_width);
      if (include_x_ticks) curdy -= major_tick_length;
      if (include_x_tick_labels) curdy -= x_number_height;
      /* now curdy is basically how much y space we have */
      ap->y_label_x = curx+y_label_width;
      ap->y_label_y = (curdy+y_label_height)/2;
      curx += (y_label_width+inner_border_width);
    }
  if (include_y_ticks)
    {
      /* figure out how long the longest tick label will be and make room for it */
      /* values go from ap->y0 to ap->y1 */
      /* basic tick spacing is tick_spacing pixels */
      num_ticks = curdy/y_tick_spacing;
      /* ticks start and stop at 10-based locations (i.e. sloppy axis bounds) */
      /* so, given the current min..max, find the pretty min..max for ticks */
      y_range = ap->y1 - ap->y0;
      if (y_range <= 0.0)
	{
	  if (ap->y0 != 0.0)
	    {
	      ap->y1 = ap->y0*1.25;
	      y_range = ap->y0*.25;
	    }
	  else
	    {
	      ap->y1 = 1.0;
	      y_range = 1.0;
	    }
	}

      tdy=describe_ticks((tick_descriptor *)(ap->y_ticks),ap->y0,ap->y1,num_ticks);
      ap->y_ticks = tdy;
      if (include_y_tick_labels)
	{
	  if (tdy->min_label) {FREE(tdy->min_label); tdy->min_label = NULL;}
	  tdy->min_label = prettyf(tdy->mlo,tdy->tens);
	  tdy->min_label_width=number_width(ax,tdy->min_label);
	  if (tdy->max_label) {FREE(tdy->max_label); tdy->max_label = NULL;}
	  tdy->max_label = prettyf(tdy->mhi,tdy->tens);
	  tdy->max_label_width=number_width(ax,tdy->max_label);
	  tick_label_width = tdy->min_label_width;
	  if (tick_label_width < tdy->max_label_width) tick_label_width = tdy->max_label_width;
	  if (((curx+tick_label_width) > (int)(.61*width)) || ((4*x_number_height) > height))
	    {
	      include_y_tick_labels = 0;
	    }
	  else
	    {
	      curx+=tick_label_width;
	      non_label_room = (Float)(cury-y_label_height)/(3.0*height);
	      if (((y_label_height+3*x_number_height+major_tick_length+top_border_width+bottom_border_width+20) < cury) &&
		  (((tdy->mlo-tdy->flo)/y_range) < non_label_room) &&
		  (((tdy->fhi-tdy->mhi)/y_range) < non_label_room))
		/* last two checks are trying to keep number labels from colliding with the label */
		{
		  if (y_label_width < tick_label_width)
		    curx -=y_label_width;
		  else curx-=(y_label_width/2);
		}
	    }
	}
      
      curx+=major_tick_length;
      tdy->maj_tick_len = major_tick_length;
      tdy->min_tick_len = minor_tick_length;
      ap->y_axis_y1 = top_border_width;
    }
  else ap->y_axis_y1 = 0;

  ap->x_axis_x1 = width-right_border_width;
  ap->x_axis_x0 = curx;
  x_range = ap->x1 - ap->x0;
  if (x_range <= 0)
    {
      if (ap->x0 != 0.0)
	{
	  ap->x1 = ap->x0*1.25;
	  x_range = ap->x0*.25;
	}
      else
	{
	  ap->x1 = .1;
	  x_range = .1;
	}
    }
  if (include_x_ticks) 
    {
      num_ticks = (ap->x_axis_x1-curx)/x_tick_spacing;

      switch (x_style)
	{
	case X_IN_SECONDS: tdx=describe_ticks((tick_descriptor *)(ap->x_ticks),ap->x0,ap->x1,num_ticks); break;
	case X_IN_SAMPLES: tdx=describe_ticks((tick_descriptor *)(ap->x_ticks),ap->x0*srate,ap->x1*srate,num_ticks); break;
	case X_TO_ONE: tdx=describe_ticks((tick_descriptor *)(ap->x_ticks),ap->x0/ap->xmax,ap->x1/ap->xmax,num_ticks); break;
	default: tdx=describe_ticks((tick_descriptor *)(ap->x_ticks),ap->x0,ap->x1,num_ticks); break;
	}
      ap->x_ticks = tdx;
      if (include_x_tick_labels)
	{
	  if (tdx->min_label) {FREE(tdx->min_label); tdx->min_label = NULL;}
	  tdx->min_label = prettyf(tdx->mlo,tdx->tens);
	  tdx->min_label_width=number_width(ax,tdx->min_label);
	  if (tdx->max_label) {FREE(tdx->max_label); tdx->max_label = NULL;}
	  tdx->max_label = prettyf(tdx->mhi,tdx->tens);
	  tdx->max_label_width=number_width(ax,tdx->max_label);
	  tick_label_width = tdx->min_label_width;
	  if (tick_label_width < tdx->max_label_width) tick_label_width = tdx->max_label_width;
	  if ((curx+2*tick_label_width) > (int)(.61*width)) 
	    {
	      include_x_tick_labels = 0;
	    }
	}
      
      tdx->maj_tick_len = major_tick_length;
      tdx->min_tick_len = minor_tick_length;
    }

  if ((include_x_label) || (include_x_tick_labels))
    {
      ap->x_label_y = cury;
      ap->x_label_x = (curx+width-x_label_width)/2;
      cury -= (x_label_height+inner_border_width);
    }
  ap->y_axis_y0 = cury;
  ap->y_axis_x0 = curx;

  ap->y_axis_x0 += ap->graph_x0;
  ap->x_axis_x0 += ap->graph_x0;
  ap->x_axis_x1 += ap->graph_x0;
  ap->y_label_x += ap->graph_x0;
  ap->x_label_x += ap->graph_x0;
  if (axis_style == axis_x_bottom)
    {
      ap->x_axis_y0 = cury;
    }
  else
    {
      if (axis_style == axis_x_middle)
	{
	  ap->x_axis_y0 = (cury+ap->y_axis_y1)/2; /* in this case, we should be zero centered! */
	}
      else snd_error("impossible x axis style: %d\n",axis_style);
    }

  ap->x_scale = ((double)(ap->x_axis_x1 - ap->x_axis_x0))/((double)(ap->x1 - ap->x0));
  ap->y_scale = (Float)(ap->y_axis_y1 - ap->y_axis_y0)/(ap->y1 - ap->y0);

  /* now if y_offset is in use, apply global shift in y direction */
  ap->x_axis_y0 += ap->y_offset;
  ap->y_axis_y0 += ap->y_offset;
  ap->y_axis_y1 += ap->y_offset;
  ap->x_label_y += ap->y_offset;
  ap->y_label_y += ap->y_offset;

  /* finally ready to draw ... */
  if ((sp) && (cp->drawing) && (cp->ffting) && (cp->fft) && (sp->combining == CHANNELS_SUPERIMPOSED) && (ap == cp->fft->axis))
    erase_rectangle(cp,ax,ap->x_axis_x0,ap->x_axis_y0+2,ap->x_axis_x1 - ap->x_axis_x0,ap->x_label_y + 2 - ap->x_axis_y0);
  if (include_y_label)
    { /* used to use DrawVString here from sciplot, but it causes the display to flash */
      int col,row,len,i;
      len = snd_strlen(ap->ylabel);
      col = ap->y_label_x;
      row = ap->y_label_y-y_label_height;
      activate_label_font(ax);
      if (cp->printing) ps_set_label_font(cp);
      for (i=0;i<len;i++)
	{
	  draw_string(ax,col,row,&ap->ylabel[i],1);
	  if (cp->printing) ps_draw_string(cp,col,row,&ap->ylabel[i]);
	  row+=y_label_width;
	}
    }
  if (include_x_label)
    {
      activate_label_font(ax);
      draw_string(ax,ap->x_label_x,ap->x_label_y+7,ap->xlabel,snd_strlen(ap->xlabel));
      if (cp->printing) 
	{
	  ps_set_label_font(cp);
	  ps_draw_string(cp,ap->x_label_x,ap->x_label_y+7,ap->xlabel);
	}
    }
  if (!(cp->drawing)) return;

  if (show_x_axis)
    fill_rectangle(ax,ap->x_axis_x0,ap->x_axis_y0,(unsigned int)(ap->x_axis_x1-ap->x_axis_x0),axis_thickness);
  if ((sp == NULL) || (cp->show_axes != SHOW_X_AXIS))
    fill_rectangle(ax,ap->y_axis_x0,ap->y_axis_y1,axis_thickness,(unsigned int)(ap->y_axis_y0-ap->y_axis_y1));
  if ((include_y_tick_labels) || (include_x_tick_labels)) activate_numbers_font(ax);
  if (cp->printing) 
    {
      if (show_x_axis)
	ps_fill_rectangle(cp,ap->x_axis_x0,ap->x_axis_y0,(unsigned int)(ap->x_axis_x1-ap->x_axis_x0),1);
      if ((sp == NULL) || (cp->show_axes != SHOW_X_AXIS))
	ps_fill_rectangle(cp,ap->y_axis_x0,ap->y_axis_y1,1,(unsigned int)(ap->y_axis_y0-ap->y_axis_y1));
      if ((include_y_tick_labels) || (include_x_tick_labels)) ps_set_number_font(cp);
    }

  if (include_y_tick_labels)
    {
      draw_string(ax,
		  ap->y_axis_x0-tdy->maj_tick_len-tdy->min_label_width-inner_border_width,
		  (int)(grf_y(tdy->mlo,ap)+.25*x_number_height),
		  tdy->min_label,
		  strlen(tdy->min_label));
      draw_string(ax,
		  ap->y_axis_x0-tdy->maj_tick_len-tdy->max_label_width-inner_border_width,
		  (int)(grf_y(tdy->mhi,ap)+.5*x_number_height),
		  tdy->max_label,
		  strlen(tdy->max_label));
      if (cp->printing) 
	{
	  ps_draw_string(cp,
		      ap->y_axis_x0-tdy->maj_tick_len-tdy->min_label_width-inner_border_width,
		      (int)(grf_y(tdy->mlo,ap)+.25*x_number_height),
		      tdy->min_label);
	  ps_draw_string(cp,
		      ap->y_axis_x0-tdy->maj_tick_len-tdy->max_label_width-inner_border_width,
		      (int)(grf_y(tdy->mhi,ap)+.5*x_number_height),
		      tdy->max_label);
	}
    }
  if (include_x_tick_labels)
    {
      int lx0,lx1,tx0,tx1;
      /* the label is at ap->x_label_x to that plus x_label_width */
      /* the number label widths are tdx->max|min_label_width */
      lx0 = ap->x_label_x;
      lx1 = lx0 + x_label_width;
      tx0 = (int)(tick_grf_x(tdx->mlo,ap,x_style,srate)-.45*tdx->min_label_width);
      tx1 = tx0+tdx->min_label_width;
      if ((lx0 > tx1) || (lx1 < tx0))
	{
	  draw_string(ax,
		      tx0,
		      ap->x_label_y+2,
		      tdx->min_label,
		      strlen(tdx->min_label));
	  if (cp->printing) 
	    ps_draw_string(cp,
			   tx0,
			   ap->x_label_y+2,
			   tdx->min_label);
	}
      tx0 = (int)(tick_grf_x(tdx->mhi,ap,x_style,srate)-(.45 * tdx->max_label_width)); /* try centered label first */
      if ((tx0+tdx->max_label_width) > ap->x_axis_x1)
	tx0 = (int)(tick_grf_x(tdx->mhi,ap,x_style,srate)-tdx->max_label_width+.75*right_border_width);
      tx1 = tx0+tdx->max_label_width;
      if ((lx0 > tx1) || (lx1 < tx0))
	{
	  draw_string(ax,
		      tx0,
		      ap->x_label_y+2,
		      tdx->max_label,
		      strlen(tdx->max_label));
	  if (cp->printing) 
	    ps_draw_string(cp,
			   tx0,
			   ap->x_label_y+2,
			   tdx->max_label);
	}
    }
  if (include_y_ticks)
    {
      Float tens,y;
      int majx,minx,ty,x0,x;
      /* start ticks at flo, go to fhi by step, major ticks at mlo mhi and intervals of tenstep surrounding */

      x0=ap->y_axis_x0;
      majx=x0-tdy->maj_tick_len;
      minx=x0-tdy->min_tick_len;

      y=tdy->mlo;
      ty=grf_y(y,ap);
      draw_line(ax,majx,ty,x0,ty);
      if (cp->printing) ps_draw_line(cp,majx,ty,x0,ty);

      tens = 0.0;
      y -= tdy->step;
      while (y >= tdy->flo)
	{
	  tens += tdy->tenstep;
	  if (tens == 10.0)
	    {
	      tens = 0.0;
	      x=majx;
	    }
	  else x=minx;
	  ty=grf_y(y,ap);
	  draw_line(ax,x,ty,x0,ty);
	  if (cp->printing) ps_draw_line(cp,x,ty,x0,ty);
	  y -= tdy->step;
	}
      
      tens = 0.0;
      y=tdy->mlo;
      y += tdy->step;
      while (y <= tdy->fhi)
	{
	  tens += tdy->tenstep;
	  if (tens == 10.0)
	    {
	      tens = 0.0;
	      x=majx;
	    }
	  else x=minx;
	  ty=grf_y(y,ap);
	  draw_line(ax,x,ty,x0,ty);
	  if (cp->printing) ps_draw_line(cp,x,ty,x0,ty);
	  y += tdy->step;
	}
    }
  if (include_x_ticks)
    {
      Float tens,x;
      int majy,miny,tx,y0,y;
      /* start ticks at flo, go to fhi by step, major ticks at mlo mhi and intervals of tenstep surrounding */

      y0=ap->x_axis_y0;
      majy=y0+tdx->maj_tick_len;
      miny=y0+tdx->min_tick_len;

      x=tdx->mlo;
      tx=tick_grf_x(x,ap,x_style,srate);
      draw_line(ax,tx,majy,tx,y0);
      if (cp->printing) ps_draw_line(cp,tx,majy,tx,y0);

      tens = 0.0;
      x -= tdx->step;
      while (x >= tdx->flo)
	{
	  tens += tdx->tenstep;
	  if (tens == 10.0)
	    {
	      tens = 0.0;
	      y=majy;
	    }
	  else y=miny;
	  tx=tick_grf_x(x,ap,x_style,srate);
	  draw_line(ax,tx,y,tx,y0);
	  if (cp->printing) ps_draw_line(cp,tx,y,tx,y0);
	  x -= tdx->step;
	}
      
      tens = 0.0;
      x=tdx->mlo;
      x += tdx->step;
      while (x <= tdx->fhi)
	{
	  tens += tdx->tenstep;
	  if (tens == 10.0)
	    {
 	      tens = 0.0;
	      y=majy;
	    }
	  else y=miny;
	  tx=tick_grf_x(x,ap,x_style,srate);
	  draw_line(ax,tx,y,tx,y0);
	  if (cp->printing) ps_draw_line(cp,tx,y,tx,y0);
	  x += tdx->step;
	}
    }
}


axis_info *make_axis_info (chan_info *cp, Float xmin, Float xmax, Float ymin, Float ymax, 
			   char *xlabel, Float x0, Float x1, Float y0, Float y1, 
			   axis_info *old_ap)
{
  axis_info *ap;
  if (old_ap) 
    {
      ap=old_ap;
      if (ap->xlabel) {FREE(ap->xlabel); ap->xlabel = NULL;}
    }
  else
    {
      ap = (axis_info *)CALLOC(1,sizeof(axis_info));
      ap->ss = cp->state;
      ap->cp = cp;
    }
  ap->xmin = xmin;
  ap->xmax = xmax;
  if (ap->xmin == ap->xmax) ap->xmax+=.001;
  ap->ymin = ymin;
  ap->ymax = ymax;
  ap->xlabel = copy_string(xlabel);
  ap->x0 = x0;
  ap->x1 = x1;
  if (ap->x0 == ap->x1) ap->x1+=.001;
  if (ap->x1 > ap->xmax) ap->x1 = ap->xmax;
  ap->y0 = y0;
  ap->y1 = y1;
  ap->y_offset = 0;
  return(ap);
}
