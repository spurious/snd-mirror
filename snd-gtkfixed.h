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


#ifndef __GTK_SND_FIXED_H__
#define __GTK_SND_FIXED_H__


#include <gdk/gdk.h>
#include <gtk/gtkcontainer.h>


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#define GTK_TYPE_SND_FIXED                  (gtk_snd_fixed_get_type ())
#define GTK_SND_FIXED(obj)                  (GTK_CHECK_CAST ((obj), GTK_TYPE_SND_FIXED, GtkSnd_Fixed))
#define GTK_SND_FIXED_CLASS(klass)          (GTK_CHECK_CLASS_CAST ((klass), GTK_TYPE_SND_FIXED, GtkSnd_FixedClass))
#define GTK_IS_SND_FIXED(obj)               (GTK_CHECK_TYPE ((obj), GTK_TYPE_SND_FIXED))
#define GTK_IS_SND_FIXED_CLASS(klass)       (GTK_CHECK_CLASS_TYPE ((klass), GTK_TYPE_SND_FIXED))


typedef struct _GtkSnd_Fixed        GtkSnd_Fixed;
typedef struct _GtkSnd_FixedClass   GtkSnd_FixedClass;
typedef struct _GtkSnd_FixedChild   GtkSnd_FixedChild;

struct _GtkSnd_Fixed
{
  GtkContainer container;
  GtkWidget *child;
  GList *children;
};

struct _GtkSnd_FixedClass
{
  GtkContainerClass parent_class;
};

struct _GtkSnd_FixedChild
{
  GtkWidget *widget;
  gint16 x;
  gint16 y;
};


GtkType    gtk_snd_fixed_get_type          (void);
GtkWidget* gtk_snd_fixed_new               (GtkWidget *child);
void       gtk_snd_fixed_put               (GtkSnd_Fixed       *snd_fixed,
                                        GtkWidget      *widget,
                                        gint16         x,
                                        gint16         y);
void       gtk_snd_fixed_move              (GtkSnd_Fixed       *snd_fixed,
                                        GtkWidget      *widget,
                                        gint16         x,
                                        gint16         y);

#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_SND_FIXED_H__ */
