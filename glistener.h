#ifndef GLISTENER_H
#define GLISTENER_H

#include <ctype.h>
#include <stddef.h>
#include <math.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <limits.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <sys/types.h>
#include <stdbool.h>

#include <gtk/gtk.h>

/* #define HAVE_GTK_3 1 */

#if HAVE_GTK_3
  #include <gdk/gdk.h>
#else
  #include <gdk/gdkkeysyms.h>
#endif

typedef struct glistener glistener;

glistener *glistener_new(GtkWidget *parent, void (*initializations)(glistener *g, GtkWidget *new_listener));

void glistener_clear               (glistener *g);

bool glistener_write               (glistener *g, FILE *fp);
bool glistener_write_to_file       (glistener *g, const char *filename);

void glistener_scroll_to_end       (glistener *g);

void glistener_append_text         (glistener *g, const char *msg);
void glistener_insert_text         (glistener *g, const char *text);

void glistener_append_prompt       (glistener *g);
void glistener_set_prompt          (glistener *g, const char *str);
void glistener_set_prompt_tag      (glistener *g, GtkTextTag *m);

void glistener_set_cursor_shape    (glistener *g, GdkCursor *cursor_shape);
int glistener_cursor_position      (glistener *g);
void glistener_set_cursor_position (glistener *g, int position);

void glistener_post_status         (glistener *g, const char *msg);
void glistener_clear_status        (glistener *g);

void glistener_key_bindings        (glistener *g, gpointer cls);
void glistener_filters_expression  (glistener *g, bool filtering);

void glistener_set_font            (glistener *g, PangoFontDescription *font);
#if (!HAVE_GTK_3)
void glistener_set_text_color      (glistener *g, GdkColor *p);
void glistener_set_background_color(glistener *g, GdkColor *p);
#else
void glistener_set_text_color      (glistener *g, GdkRGBA *p);
void glistener_set_background_color(glistener *g, GdkRGBA *p);
#endif

void glistener_set_completer       (glistener *g, void (*completer)(glistener *g, bool (*symbol_func)(const char *symbol_name, void *data), void *data));
void glistener_set_helper          (glistener *g, const char *(*help)(glistener *g, const char *text));
void glistener_set_evaluator       (glistener *g, void (*eval)(glistener *g, const char *text));

/* these are for regression testing */
char *glistener_evaluate           (glistener *g);
char *glistener_complete           (glistener *g);
int glistener_prompt_position      (glistener *g);
char *glistener_text               (glistener *g, int start, int end);

#endif
