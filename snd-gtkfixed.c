/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * Modified by the GTK+ Team and others 1997-1999.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/. 
 */

/* 
 * Modified by bil@ccrma to include one fixed child that fills the
 * whole widget automatically, plus the usual children.  The original
 * fixed widget would not allow me to set the child widget's size
 * to fill its window without causing endless problems.  The
 * drawing area widget itself (the child in this case) is not
 * a container, so could not receive mix consoles as children.
 */

#include <stdio.h>

#include "snd-gtkfixed.h"

static void gtk_snd_fixed_class_init    (GtkSnd_FixedClass    *klass);
static void gtk_snd_fixed_init          (GtkSnd_Fixed         *snd_fixed);
static void gtk_snd_fixed_map           (GtkWidget        *widget);
static void gtk_snd_fixed_realize       (GtkWidget        *widget);
static void gtk_snd_fixed_size_request  (GtkWidget        *widget,
				     GtkRequisition   *requisition);
static void gtk_snd_fixed_size_allocate (GtkWidget        *widget,
				     GtkAllocation    *allocation);
static void gtk_snd_fixed_paint         (GtkWidget        *widget,
				     GdkRectangle     *area);
static void gtk_snd_fixed_draw          (GtkWidget        *widget,
				     GdkRectangle     *area);
static gint gtk_snd_fixed_expose        (GtkWidget        *widget,
				     GdkEventExpose   *event);
static void gtk_snd_fixed_add           (GtkContainer     *container,
				     GtkWidget        *widget);
static void gtk_snd_fixed_remove        (GtkContainer     *container,
				     GtkWidget        *widget);
static void gtk_snd_fixed_forall        (GtkContainer     *container,
				     gboolean 	       include_internals,
				     GtkCallback       callback,
				     gpointer          callback_data);
static GtkType gtk_snd_fixed_child_type (GtkContainer     *container);


static GtkContainerClass *parent_class = NULL;


GtkType
gtk_snd_fixed_get_type (void)
{
  static GtkType snd_fixed_type = 0;

  if (!snd_fixed_type)
    {
      static const GtkTypeInfo snd_fixed_info =
      {
	"GtkSnd_Fixed",
	sizeof (GtkSnd_Fixed),
	sizeof (GtkSnd_FixedClass),
	(GtkClassInitFunc) gtk_snd_fixed_class_init,
	(GtkObjectInitFunc) gtk_snd_fixed_init,
	/* reserved_1 */ NULL,
        /* reserved_2 */ NULL,
        (GtkClassInitFunc) NULL,
      };

      snd_fixed_type = gtk_type_unique (GTK_TYPE_CONTAINER, &snd_fixed_info);
    }

  return snd_fixed_type;
}

static void
gtk_snd_fixed_class_init (GtkSnd_FixedClass *klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkContainerClass *container_class;

  object_class = (GtkObjectClass*) klass;
  widget_class = (GtkWidgetClass*) klass;
  container_class = (GtkContainerClass*) klass;

  parent_class = (GtkContainerClass *)gtk_type_class (GTK_TYPE_CONTAINER);

  widget_class->map = gtk_snd_fixed_map;
  widget_class->realize = gtk_snd_fixed_realize;
  widget_class->size_request = gtk_snd_fixed_size_request;
  widget_class->size_allocate = gtk_snd_fixed_size_allocate;
  widget_class->draw = gtk_snd_fixed_draw;
  widget_class->expose_event = gtk_snd_fixed_expose;

  container_class->add = gtk_snd_fixed_add;
  container_class->remove = gtk_snd_fixed_remove;
  container_class->forall = gtk_snd_fixed_forall;
  container_class->child_type = gtk_snd_fixed_child_type;
}

static GtkType
gtk_snd_fixed_child_type (GtkContainer     *container)
{
  return GTK_TYPE_WIDGET;
}

static void
gtk_snd_fixed_init (GtkSnd_Fixed *snd_fixed)
{
  GTK_WIDGET_UNSET_FLAGS (snd_fixed, GTK_NO_WINDOW);
 
  snd_fixed->children = NULL;
}

GtkWidget*
gtk_snd_fixed_new (GtkWidget *child)
{
  GtkSnd_Fixed *snd_fixed;

  snd_fixed = (GtkSnd_Fixed *)gtk_type_new (GTK_TYPE_SND_FIXED);
  snd_fixed->child = child;
  return GTK_WIDGET (snd_fixed);
}

void
gtk_snd_fixed_put (GtkSnd_Fixed       *snd_fixed,
               GtkWidget      *widget,
               gint16         x,
               gint16         y)
{
  GtkSnd_FixedChild *child_info;

  g_return_if_fail (snd_fixed != NULL);
  g_return_if_fail (GTK_IS_SND_FIXED (snd_fixed));
  g_return_if_fail (widget != NULL);

  child_info = g_new (GtkSnd_FixedChild, 1);
  child_info->widget = widget;
  child_info->x = x;
  child_info->y = y;

  gtk_widget_set_parent (widget, GTK_WIDGET (snd_fixed));

  snd_fixed->children = g_list_append (snd_fixed->children, child_info); 

  if (GTK_WIDGET_REALIZED (snd_fixed))
    gtk_widget_realize (widget);

  if (GTK_WIDGET_VISIBLE (snd_fixed) && GTK_WIDGET_VISIBLE (widget))
    {
      if (GTK_WIDGET_MAPPED (snd_fixed))
	gtk_widget_map (widget);
      
      gtk_widget_queue_resize (GTK_WIDGET (snd_fixed));
    }
}

void
gtk_snd_fixed_move (GtkSnd_Fixed       *snd_fixed,
                GtkWidget      *widget,
                gint16         x,
                gint16         y)
{
  GtkSnd_FixedChild *child;
  GList *children;

  g_return_if_fail (snd_fixed != NULL);
  g_return_if_fail (GTK_IS_SND_FIXED (snd_fixed));
  g_return_if_fail (widget != NULL);

  children = snd_fixed->children;
  while (children)
    {
      child = children->data;
      children = children->next;

      if (child->widget == widget)
        {
          child->x = x;
          child->y = y;

          if (GTK_WIDGET_VISIBLE (widget) && GTK_WIDGET_VISIBLE (snd_fixed))
            gtk_widget_queue_resize (GTK_WIDGET (snd_fixed));

          break;
        }
    }
}

static void
gtk_snd_fixed_map (GtkWidget *widget)
{
  GtkSnd_Fixed *snd_fixed;
  GtkSnd_FixedChild *child;
  GList *children;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_SND_FIXED (widget));

  GTK_WIDGET_SET_FLAGS (widget, GTK_MAPPED);
  snd_fixed = GTK_SND_FIXED (widget);

  gtk_widget_map(snd_fixed->child);

  children = snd_fixed->children;
  while (children)
    {
      child = children->data;
      children = children->next;

      if (GTK_WIDGET_VISIBLE (child->widget) &&
	  !GTK_WIDGET_MAPPED (child->widget))
	gtk_widget_map (child->widget);
    }

  gdk_window_show (widget->window);
}

static void
gtk_snd_fixed_realize (GtkWidget *widget)
{
  GdkWindowAttr attributes;
  gint attributes_mask;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_SND_FIXED (widget));

  GTK_WIDGET_SET_FLAGS (widget, GTK_REALIZED);

  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.x = widget->allocation.x;
  attributes.y = widget->allocation.y;
  attributes.width = widget->allocation.width;
  attributes.height = widget->allocation.height;
  attributes.wclass = GDK_INPUT_OUTPUT;
  attributes.visual = gtk_widget_get_visual (widget);
  attributes.colormap = gtk_widget_get_colormap (widget);
  attributes.event_mask = gtk_widget_get_events (widget);
  attributes.event_mask |= GDK_EXPOSURE_MASK | GDK_BUTTON_PRESS_MASK;

  attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;

  widget->window = gdk_window_new (gtk_widget_get_parent_window (widget), &attributes, 
				   attributes_mask);
  gdk_window_set_user_data (widget->window, widget);

  widget->style = gtk_style_attach (widget->style, widget->window);
  gtk_style_set_background (widget->style, widget->window, GTK_STATE_NORMAL);
}

static void
gtk_snd_fixed_size_request (GtkWidget      *widget,
			GtkRequisition *requisition)
{
  GtkSnd_Fixed *snd_fixed;  
  GtkSnd_FixedChild *child;
  GList *children;
  GtkRequisition child_requisition;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_SND_FIXED (widget));
  g_return_if_fail (requisition != NULL);

  snd_fixed = GTK_SND_FIXED (widget);
  requisition->width = 0;
  requisition->height = 0;

  children = snd_fixed->children;
  while (children)
    {
      child = children->data;
      children = children->next;

      if (GTK_WIDGET_VISIBLE (child->widget))
	{
          gtk_widget_size_request (child->widget, &child_requisition);

          requisition->height = MAX (requisition->height,
                                     child->y +
                                     child_requisition.height);
          requisition->width = MAX (requisition->width,
                                    child->x +
                                    child_requisition.width);
	}
    }

  requisition->height += GTK_CONTAINER (snd_fixed)->border_width * 2;
  requisition->width += GTK_CONTAINER (snd_fixed)->border_width * 2;
}

static void
gtk_snd_fixed_size_allocate (GtkWidget     *widget,
			 GtkAllocation *allocation)
{
  GtkSnd_Fixed *snd_fixed;
  GtkSnd_FixedChild *child;
  GtkAllocation child_allocation;
  GtkRequisition child_requisition;
  GList *children;
  guint16 border_width;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_SND_FIXED(widget));
  g_return_if_fail (allocation != NULL);

  snd_fixed = GTK_SND_FIXED (widget);

  widget->allocation = *allocation;
  if (GTK_WIDGET_REALIZED (widget))
    gdk_window_move_resize (widget->window,
			    allocation->x, 
			    allocation->y,
			    allocation->width, 
			    allocation->height);

  border_width = GTK_CONTAINER (snd_fixed)->border_width;
  
  /* gtk_widget_get_child_requisition (snd_fixed->child, &child_requisition); */
  child_allocation.x = border_width;
  child_allocation.y = border_width;
  child_allocation.width = allocation->width - 2*border_width;
  child_allocation.height = allocation->height - 2*border_width;
  gtk_widget_size_allocate (snd_fixed->child, &child_allocation);

  children = snd_fixed->children;
  while (children)
    {
      child = children->data;
      children = children->next;
      
      if (GTK_WIDGET_VISIBLE (child->widget))
	{
	  gtk_widget_get_child_requisition (child->widget, &child_requisition);
	  child_allocation.x = child->x + border_width;
	  child_allocation.y = child->y + border_width;
	  child_allocation.width = child_requisition.width;
	  child_allocation.height = child_requisition.height;
	  gtk_widget_size_allocate (child->widget, &child_allocation);
	}
    }
}

static void
gtk_snd_fixed_paint (GtkWidget    *widget,
		 GdkRectangle *area)
{
  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_SND_FIXED (widget));
  g_return_if_fail (area != NULL);

  if (GTK_WIDGET_DRAWABLE (widget))
    gdk_window_clear_area (widget->window,
			   area->x, area->y,
			   area->width, area->height);
}

static void
gtk_snd_fixed_draw (GtkWidget    *widget,
		GdkRectangle *area)
{
  GtkSnd_Fixed *snd_fixed;
  GtkSnd_FixedChild *child;
  GdkRectangle child_area;
  GList *children;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_SND_FIXED (widget));

  if (GTK_WIDGET_DRAWABLE (widget))
    {
      snd_fixed = GTK_SND_FIXED (widget);
      gtk_snd_fixed_paint (widget, area);

      if (gtk_widget_intersect (snd_fixed->child, area, &child_area))
	gtk_widget_draw(snd_fixed->child,&child_area);

      children = snd_fixed->children;
      while (children)
	{
	  child = children->data;
	  children = children->next;

	  if (gtk_widget_intersect (child->widget, area, &child_area))
	    gtk_widget_draw (child->widget, &child_area);
	}
    }
}

static gint
gtk_snd_fixed_expose (GtkWidget      *widget,
		  GdkEventExpose *event)
{
  GtkSnd_Fixed *snd_fixed;
  GtkSnd_FixedChild *child;
  GdkEventExpose child_event;
  GList *children;

  g_return_val_if_fail (widget != NULL, FALSE);
  g_return_val_if_fail (GTK_IS_SND_FIXED (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);

  if (GTK_WIDGET_DRAWABLE (widget))
    {
      snd_fixed = GTK_SND_FIXED (widget);

      child_event = *event;

      if (GTK_WIDGET_NO_WINDOW (snd_fixed->child) &&
	  gtk_widget_intersect (snd_fixed->child, &event->area, 
				&child_event.area))
	gtk_widget_event (snd_fixed->child, (GdkEvent*) &child_event);

      children = snd_fixed->children;
      while (children)
	{
	  child = children->data;
	  children = children->next;

	  if (GTK_WIDGET_NO_WINDOW (child->widget) &&
	      gtk_widget_intersect (child->widget, &event->area, 
				    &child_event.area))
	    gtk_widget_event (child->widget, (GdkEvent*) &child_event);
	}
    }

  return FALSE;
}

static void
gtk_snd_fixed_add (GtkContainer *container,
	       GtkWidget    *widget)
{
  g_return_if_fail (container != NULL);
  g_return_if_fail (GTK_IS_SND_FIXED (container));
  g_return_if_fail (widget != NULL);

  gtk_snd_fixed_put (GTK_SND_FIXED (container), widget, 0, 0);
}

static void
gtk_snd_fixed_remove (GtkContainer *container,
		  GtkWidget    *widget)
{
  GtkSnd_Fixed *snd_fixed;
  GtkSnd_FixedChild *child;
  GList *children;

  g_return_if_fail (container != NULL);
  g_return_if_fail (GTK_IS_SND_FIXED (container));
  g_return_if_fail (widget != NULL);

  snd_fixed = GTK_SND_FIXED (container);

  children = snd_fixed->children;
  while (children)
    {
      child = children->data;

      if (child->widget == widget)
	{
	  gboolean was_visible = GTK_WIDGET_VISIBLE (widget);
	  
	  gtk_widget_unparent (widget);

	  snd_fixed->children = g_list_remove_link (snd_fixed->children, children);
	  g_list_free (children);
	  g_free (child);

	  if (was_visible && GTK_WIDGET_VISIBLE (container))
	    gtk_widget_queue_resize (GTK_WIDGET (container));

	  break;
	}

      children = children->next;
    }
}

static void
gtk_snd_fixed_forall (GtkContainer *container,
		  gboolean	include_internals,
		  GtkCallback   callback,
		  gpointer      callback_data)
{
  GtkSnd_Fixed *snd_fixed;
  GtkSnd_FixedChild *child;
  GList *children;

  g_return_if_fail (container != NULL);
  g_return_if_fail (GTK_IS_SND_FIXED (container));
  g_return_if_fail (callback != NULL);

  snd_fixed = GTK_SND_FIXED (container);

  children = snd_fixed->children;
  while (children)
    {
      child = children->data;
      children = children->next;

      (* callback) (child->widget, callback_data);
    }
}
