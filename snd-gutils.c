#include "snd.h"

axis_context *free_axis_context(axis_context *ax)
{
  if (ax) FREE(ax);
  return(NULL);
}

int set_help_text_font(snd_state *ss, char *font)
{
  GdkFont *fs = NULL;
  state_context *sgx;
  sgx = ss->sgx;
  fs = gdk_font_load(font);
  if (fs)
    {
      in_set_help_text_font(ss,font);
      sgx->help_text_fnt = fs;
      return(TRUE);
    }
  return(FALSE);
}

int set_tiny_font(snd_state *ss, char *font)
{
  GdkFont *fs = NULL;
  state_context *sgx;
  sgx = ss->sgx;
  fs = gdk_font_load(font);
  if (fs)
    {
      in_set_tiny_font(ss,font);
      sgx->tiny_fnt = fs;
      return(TRUE);
    }
  return(FALSE);
}

int set_listener_font(snd_state *ss, char *font)
{
  GdkFont *fs = NULL;
  fs = gdk_font_load(font);
  if (fs)
    {
      in_set_listener_font(ss,font);
      (ss->sgx)->listener_fnt = fs;
      return(TRUE);
    }
  return(FALSE);
}

int set_button_font(snd_state *ss, char *font)
{
  GdkFont *fs = NULL;
  fs = gdk_font_load(font);
  if (fs)
    {
      in_set_button_font(ss,font);
      (ss->sgx)->button_fnt = fs;
      return(TRUE);
    }
  return(FALSE);
}

int set_bold_button_font(snd_state *ss, char *font)
{
  GdkFont *fs = NULL;
  fs = gdk_font_load(font);
  if (fs)
    {
      in_set_bold_button_font(ss,font);
      (ss->sgx)->bold_button_fnt = fs;
      return(TRUE);
    }
  return(FALSE);
}

int set_axis_label_font(snd_state *ss, char *font)
{
  GdkFont *fs = NULL;
  fs = gdk_font_load(font);
  if (fs)
    {
      in_set_axis_label_font(ss,font);
      (ss->sgx)->axis_label_fnt = fs;
      return(TRUE);
    }
  return(FALSE);
}

int set_axis_numbers_font(snd_state *ss, char *font)
{
  GdkFont *fs = NULL;
  fs = gdk_font_load(font);
  if (fs)
    {
      in_set_axis_numbers_font(ss,font);
      (ss->sgx)->axis_numbers_fnt = fs;
      return(TRUE);
    }
  return(FALSE);
}


void activate_numbers_font(axis_context *ax)
{
  gdk_gc_set_font(ax->gc,AXIS_NUMBERS_FONT(ax->ss));
  ax->current_font = AXIS_NUMBERS_FONT(ax->ss);
}
   
void activate_button_font(axis_context *ax, snd_state *ss)
{
  gdk_gc_set_font(ax->gc,(ss->sgx)->button_fnt);
  ax->current_font = (ss->sgx)->button_fnt;
}

void activate_label_font(axis_context *ax)
{
  gdk_gc_set_font(ax->gc,AXIS_LABEL_FONT(ax->ss));
  ax->current_font = AXIS_LABEL_FONT(ax->ss);
}

int label_width(axis_context *ax, char *txt)
{
  if (txt)
    return(gdk_text_width(AXIS_LABEL_FONT(ax->ss),(gchar *)txt,(gint)strlen(txt)));
  else return(0);
}

int mark_name_width(snd_state *ss, char *txt)
{
  if (txt)
    return(gdk_text_width((ss->sgx)->button_fnt,(gchar *)txt,(gint)strlen(txt)));
  return(0);
}


int number_width(axis_context *ax, char *num)
{
  if (num)
    return(gdk_text_width(AXIS_NUMBERS_FONT(ax->ss),(gchar *)num,(gint)strlen(num)));
  return(0);
}

int number_height(axis_context *ax)
{
  gint lb,rb,asc,des,wid;
  gdk_text_extents(AXIS_NUMBERS_FONT(ax->ss),"1",1,&lb,&rb,&wid,&asc,&des);
  return(asc+des);
}

int label_height(axis_context *ax)
{
  gint lb,rb,asc,des,wid;
  gdk_text_extents(AXIS_LABEL_FONT(ax->ss),"1",1,&lb,&rb,&wid,&asc,&des);
  return(asc+des);
}

void clear_window(axis_context *ax)
{
  if (ax) gdk_window_clear(ax->wn);
}

void map_over_children (GtkWidget *w, void (*func)(GtkWidget *w, gpointer ptr), void *userptr)
{
  /* apply func to each child in entire tree beneath top widget */
  /* used mostly to get colors right in non-scheme environments with "convenience" widgets */
  /* (also to make mix consoles handle key press correctly despite non-traversable widgets) */

  if (w)
    {
      (*func)(w,userptr);
      if (GTK_IS_CONTAINER(w))
	{
	  gtk_container_foreach(GTK_CONTAINER(w),func,(gpointer)userptr);
	}
    }
}

#if 0
static int unset_key_focus_1(GtkWidget *w, void *ptr)
{
  GTK_WIDGET_UNSET_FLAGS(w,GTK_CAN_FOCUS);
  return(0);
}

void unset_key_focus(GtkWidget *w)
{
  unset_key_focus_1(w,NULL);
  map_over_children(w,unset_key_focus_1,NULL);
}
#endif

void set_background(GtkWidget *w,GdkColor *col)
{ 
  GtkStyle *style;
  style = gtk_style_copy(gtk_widget_get_style(w));
  style->bg[GTK_STATE_NORMAL].pixel = col->pixel;
  style->bg[GTK_STATE_NORMAL].red = col->red;
  style->bg[GTK_STATE_NORMAL].green = col->green;
  style->bg[GTK_STATE_NORMAL].blue = col->blue;
  gtk_widget_set_style(w,style);
}

void set_backgrounds(GtkWidget *w,GdkColor *col)
{ 
  GtkStyle *style;
  int i;
  style = gtk_style_copy(gtk_widget_get_style(w));
  for (i=0;i<5;i++)
    {
      style->bg[i].pixel = col->pixel;
      style->bg[i].red = col->red;
      style->bg[i].green = col->green;
      style->bg[i].blue = col->blue;
    }
  gtk_widget_set_style(w,style);
}


void set_active_color(GtkWidget *w,GdkColor *col)
{ 
  GtkStyle *style;
  style = gtk_style_copy(gtk_widget_get_style(w));
  style->bg[GTK_STATE_ACTIVE].pixel = col->pixel;
  style->bg[GTK_STATE_ACTIVE].red = col->red;
  style->bg[GTK_STATE_ACTIVE].green = col->green;
  style->bg[GTK_STATE_ACTIVE].blue = col->blue;
  gtk_widget_set_style(w,style);
}

void set_background_and_redraw(GtkWidget *w,GdkColor *col) 
{
  set_background(w,col);
  gtk_widget_queue_draw(w);
}

void set_foreground(GtkWidget *w,GdkColor *col)
{ 
  GtkStyle *style;
  style = gtk_style_copy(gtk_widget_get_style(w));
  style->fg[GTK_STATE_NORMAL].pixel = col->pixel;
  style->fg[GTK_STATE_NORMAL].red = col->red;
  style->fg[GTK_STATE_NORMAL].green = col->green;
  style->fg[GTK_STATE_NORMAL].blue = col->blue;
  gtk_widget_set_style(w,style);
}

#if 0
void set_foreground_and_redraw(GtkWidget *w,GdkColor *col) 
{
  set_foreground(w,col);
  gtk_widget_queue_draw(w);
}
#endif

void set_text_background(GtkWidget *w, GdkColor *col)
{ 
  GtkStyle *style;
  style = gtk_style_copy(gtk_widget_get_style(w));
  style->base[GTK_STATE_NORMAL].pixel = col->pixel;
  style->base[GTK_STATE_NORMAL].red = col->red;
  style->base[GTK_STATE_NORMAL].green = col->green;
  style->base[GTK_STATE_NORMAL].blue = col->blue;
  gtk_widget_set_style(w,style);
}

#if 0
static void set_main_color_of_widget (GtkWidget *w, gpointer userptr)
{
  snd_state *ss = (snd_state *)userptr;
  if (GTK_IS_WIDGET(w))
    {
      if ((GTK_IS_HSCROLLBAR(w)) || (GTK_IS_VSCROLLBAR(w)))
	set_background(w,(ss->sgx)->position_color);
      else set_background(w,(ss->sgx)->basic_color);
      gtk_widget_queue_draw(w);
    }
}
#endif

void set_pushed_button_colors(GtkWidget *w, snd_state *ss)
{
  GtkStyle *style;
  style = gtk_style_copy(gtk_widget_get_style(w));
  style->bg[GTK_STATE_ACTIVE] = (*((ss->sgx)->pushed_button_color));
  style->bg[GTK_STATE_NORMAL] = (*((ss->sgx)->basic_color));
  gtk_widget_set_style(w,style);
}

void highlight_color(snd_state *ss, GtkWidget *w)
{
  set_background(w,(ss->sgx)->highlight_color);
}

void white_color(snd_state *ss, GtkWidget *w)
{
  set_background(w,(ss->sgx)->white);
}

void raise_dialog(GtkWidget *w)
{
  /* since we're using non-transient message dialogs, the dialog window can become completely
   * hidden behind other windows, with no easy way to raise it back to the top, so...
   */
  gtk_widget_show(w);
  gdk_window_raise(w->window);
}

void raise_widget(GtkWidget *w)
{
  gtk_widget_show(w);
  gdk_window_raise(w->window);
}

#if 0
void set_button_label_normal(GtkWidget *button,char *str)
{
  GtkStyle *style;
  snd_state *ss;
  ss = get_global_state();
  style = gtk_style_copy(gtk_widget_get_style(button));
  style->font = (ss->sgx)->button_fnt;
  gtk_widget_set_style(button,style);
  gtk_label_set_text(GTK_LABEL(GTK_BIN(button)->child),str);
  /* gtk_widget_queue_draw(button); */
}
#endif

void set_button_label_bold(GtkWidget *button,char *str)
{
  GtkStyle *style;
  snd_state *ss;
  ss = get_global_state();
  style = gtk_style_copy(gtk_widget_get_style(button));
  style->font = (ss->sgx)->bold_button_fnt;
  gtk_widget_set_style(button,style);
  gtk_label_set_text(GTK_LABEL(GTK_BIN(button)->child),str);
}

void set_button_label(GtkWidget *label,char *str)
{
  gtk_label_set_text(GTK_LABEL(GTK_BIN(label)->child),str);
}

void set_label(GtkWidget *label,char *str)
{
  gtk_label_set_text(GTK_LABEL(label),str);
}



void work_wait(snd_state *ss)
{
  /* intended for transform-samples where we want to force a background fft process to complete */
  gtk_main_iteration();
}

void check_for_event(snd_state *ss)
{
  /* this is needed to force label updates and provide interrupts for long computations */
  ss->checking_explicitly = 1;
  while (gtk_events_pending()) gtk_main_iteration();
  ss->checking_explicitly = 0;
}

void set_title(snd_state *ss, char *title)
{
#ifndef SND_AS_WIDGET
  gtk_window_set_title(GTK_WINDOW(MAIN_SHELL(ss)),title);
#endif
}

#if 0
static int get_window_height(GtkWidget *w) {return(w->allocation.height);}
static int get_window_width(GtkWidget *w) {return(w->allocation.width);}
#endif

void goto_window(GtkWidget *text)
{
  gtk_widget_grab_focus(text);
}

char *key_to_name(int keysym) {return(gdk_keyval_name(keysym));}

void gc_set_foreground_xor(GdkGC *gc,GdkColor *col1, GdkColor *col2)
{ 
  GdkColor newcol;
  newcol.pixel = XOR(col1->pixel,col2->pixel);
  newcol.red = XOR(col1->red,col2->red);
  newcol.green = XOR(col1->green,col2->green);
  newcol.blue = XOR(col1->blue,col2->blue);
  gdk_gc_set_foreground(gc,gdk_color_copy(&newcol));
}


void color_cursor(snd_state *ss, GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->cursor_color = color;
  gc_set_foreground_xor(sx->cursor_gc,color,sx->graph_color);
  gc_set_foreground_xor(sx->selected_cursor_gc,color,sx->selected_graph_color);
}

void color_marks(snd_state *ss, GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->mark_color = color;
  gc_set_foreground_xor(sx->mark_gc,color,sx->graph_color);
  gc_set_foreground_xor(sx->selected_mark_gc,color,sx->selected_graph_color);
}

void color_selection(snd_state *ss, GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->selection_color = color;
  gc_set_foreground_xor(sx->selection_gc,color,sx->graph_color);
  gc_set_foreground_xor(sx->selected_selection_gc,color,sx->selected_graph_color);
}

void color_graph(snd_state *ss, GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->graph_color = color;
  gdk_gc_set_background(sx->basic_gc,color);
  gdk_gc_set_foreground(sx->erase_gc,color);
  gc_set_foreground_xor(sx->selection_gc,sx->selection_color,color);
  gc_set_foreground_xor(sx->cursor_gc,sx->cursor_color,color);
  gc_set_foreground_xor(sx->mark_gc,sx->mark_color,color);
}

void color_selected_graph(snd_state *ss, GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->selected_graph_color = color;
  gdk_gc_set_background(sx->selected_basic_gc,color);
  gdk_gc_set_foreground(sx->selected_erase_gc,color);
  gc_set_foreground_xor(sx->selected_selection_gc,sx->selection_color,color);
  gc_set_foreground_xor(sx->selected_cursor_gc,sx->cursor_color,color);
  gc_set_foreground_xor(sx->selected_mark_gc,sx->mark_color,color);
}

void color_data(snd_state *ss, GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->data_color = color;
  gdk_gc_set_foreground(sx->basic_gc,color);
  gdk_gc_set_background(sx->erase_gc,color);
}

void color_mix_waveform(snd_state *ss, GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->mix_waveform_color = color;
  gdk_gc_set_foreground(sx->mix_gc,color);
}

void color_selected_data(snd_state *ss, GdkColor *color)
{
  state_context *sx;
  sx = ss->sgx;
  sx->selected_data_color = color;
  gdk_gc_set_foreground(sx->selected_basic_gc,color);
  gdk_gc_set_background(sx->selected_erase_gc,color);
}

void recolor_graph(chan_info *cp, int selected)
{
  snd_state *ss;
  state_context *sx;
  ss = cp->state;
  sx = ss->sgx;
  set_background(channel_graph(cp),(selected) ? sx->selected_graph_color : sx->graph_color);
}

TIME_TYPE main_time(snd_state *ss)
{
  return(GDK_CURRENT_TIME - ss->play_start_time);
}


void reflect_resize(snd_state *ss)
{
  gtk_window_set_policy(GTK_WINDOW(MAIN_SHELL(ss)),TRUE,TRUE,auto_resize(ss));
}





void set_sensitive(GtkWidget *wid, int val) {if (wid) gtk_widget_set_sensitive(wid,val);}

void set_toggle_button(GtkWidget *wid, int val, int passed, void *data) 
{
  if (!passed) gtk_signal_handler_block_by_data(GTK_OBJECT(wid),(gpointer)data);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(wid),val);
  if (!passed) gtk_signal_handler_unblock_by_data(GTK_OBJECT(wid),(gpointer)data);
}

#if 0
static int toggle_button_on(GtkWidget *w) {return(GTK_TOGGLE_BUTTON(w)->active);}
#endif

int widget_height(GtkWidget *w)
{
  if ((w) && (GTK_WIDGET_VISIBLE(w)) && (w->allocation.height < 10000))
    return(w->allocation.height);
  return(0);
}

int widget_width(GtkWidget *w)
{
  if ((w) && (GTK_WIDGET_VISIBLE(w)) && (w->allocation.width < 10000))
    return(w->allocation.width);
  return(0);
}

void set_widget_height(GtkWidget *w, int height)
{
  /* fprintf(stderr,"uheight: %d ",height); */
  gtk_widget_set_usize(w,w->allocation.width,height);
}

void set_widget_width(GtkWidget *w, int width)
{
  /* fprintf(stderr,"uwidth: %d ",width); */
  gtk_widget_set_usize(w,width,w->allocation.height);
}

int widget_x(GtkWidget *w)
{
  return(w->allocation.x);
}

int widget_y(GtkWidget *w)
{
  return(w->allocation.y);
}

void set_widget_x(GtkWidget *w, int x)
{
  gtk_widget_set_uposition(w,x,w->allocation.y);
}

void set_widget_y(GtkWidget *w, int y)
{
  gtk_widget_set_uposition(w,w->allocation.x,y);
}

void set_widget_size(GtkWidget *w, int width, int height)
{
  /* fprintf(stderr,"usize: %d %d ",width,height); */
  gtk_widget_set_usize(w,width,height);
}

void set_widget_position(GtkWidget *w, int x, int y)
{
  gtk_widget_set_uposition(w,x,y);
}

void set_pixmap(GtkWidget *w, GdkPixmap *pix, GdkBitmap *mask)
{
  gtk_pixmap_set(GTK_PIXMAP(w),pix,mask);
}

void fixup_axis_context(axis_context *ax, GtkWidget *w, GdkGC *gc)
{
  ax->wn = w->window;
  if (gc) ax->gc = gc;
  ax->current_font = AXIS_NUMBERS_FONT(ax->ss);
}
