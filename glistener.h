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

/* #define HAVE_GTK_2 0 */
/* define this flag if you have gtk version 2, rather than version 3.
 */

#if (!HAVE_GTK_2)
  #include <gdk/gdk.h>
#else
  #include <gdk/gdkkeysyms.h>
#endif

typedef struct glistener glistener;

glistener *glistener_new(GtkWidget *parent, void (*initializations)(glistener *g, GtkWidget *new_listener));

void glistener_append_text         (glistener *g, const char *msg);
void glistener_insert_text         (glistener *g, const char *text);
char *glistener_text               (glistener *g, int start, int end);

void glistener_append_prompt       (glistener *g);
void glistener_set_prompt          (glistener *g, const char *str);
void glistener_set_prompt_tag      (glistener *g, GtkTextTag *m);
int glistener_prompt_position      (glistener *g);

void glistener_set_cursor_shape    (glistener *g, GdkCursor *cursor_shape);
int glistener_cursor_position      (glistener *g);
void glistener_set_cursor_position (glistener *g, int position);
void glistener_scroll_to_end       (glistener *g);

void glistener_post_status         (glistener *g, const char *msg);
void glistener_clear_status        (glistener *g);

void glistener_clear               (glistener *g);
bool glistener_write               (glistener *g, FILE *fp);

void glistener_set_highlight_tag   (glistener *g, GtkTextTag *m);
void glistener_set_font            (glistener *g, PangoFontDescription *font);
#if (HAVE_GTK_2)
void glistener_set_text_color      (glistener *g, GdkColor *p);
void glistener_set_background_color(glistener *g, GdkColor *p);
#else
void glistener_set_text_color      (glistener *g, GdkRGBA *p);
void glistener_set_background_color(glistener *g, GdkRGBA *p);
#endif

void glistener_key_bindings        (glistener *g, gpointer cls);
void glistener_is_schemish         (glistener *g, bool filtering);

void glistener_set_completer       (glistener *g, void (*completer)(glistener *g, bool (*symbol_func)(const char *symbol_name, void *data), void *data));
void glistener_set_helper          (glistener *g, const char *(*help)(glistener *g, const char *text));
void glistener_set_evaluator       (glistener *g, void (*eval)(glistener *g, const char *text));

/* these are for regression testing */
char *glistener_evaluate           (glistener *g);
char *glistener_complete           (glistener *g);


/* -------------------------------------------------------------------------------- */
/* an annotated version of the same info:
 *
 * typedef struct glistener glistener;
 *
 *    our opaque handle on the info for each listener.  You can run any number of listeners
 *    at the same time.  For a simple example, see s7.html#glistener.  Another is snd-glistener.c
 *    in the Snd package, and tools/gcall.c.
 *
 *
 * --------
 * glistener *glistener_new(GtkWidget *parent, void (*initializations)(glistener *g, GtkWidget *new_listener));
 *
 *   This creates a new listener.  If parent is not NULL, the listener is added to it
 *   via gtk_container_add.  The second argument is an optional function that will be called
 *   during the listener initialization, just before the signals are connected.  It
 *   can add its own signal connections or set defaults.  The "new_listener" argument
 *   passed to it is the new GtkTextView widget.  Its parent is a GtkScrolledWindow,
 *   which is placed in a grid (or table in gtk-2) along with the listener's statusbar.
 *
 *
 * --------
 * void glistener_append_text(glistener *g, const char *msg);
 *
 *   This appends "msg" to the end of the listener text.
 *
 *
 * --------
 * void glistener_insert_text(glistener *g, const char *text);
 *
 *   This inserts "text" at the cursor.
 *
 *
 * --------
 * char *glistener_text(glistener *g, int start, int end);
 *
 *  This returns the text in the listener between the offsets (unicode positions)
 *  "start" and "end".
 *
 *
 * --------
 * void glistener_append_prompt(glistener *g);
 *  
 *  This appends a prompt at the bottom of the listener text.  Everything
 *  depends on the prompt, so you need to send one out at the end of each
 *  evaluation.  The usual sequence is:
 *    user types in an expression followed by <cr> -> listener-text
 *    we_eval(listener-text) -> some string
 *    glistener_append_text(g, that string);
 *    glistener_append_prompt(g);
 *
 *
 * --------
 * void glistener_set_prompt(glistener *g, const char *str);
 *
 *  This sets the text of the prompt.  It defaults to ">" but can be anything.
 *  One way to get, for example, the unicode lower-case lambda followed by ">" as the prompt is:
 *    unsigned char prompt[4] = {0xce, 0xbb, '>', '\0'}; 
 *    glistener_set_prompt(g, prompt);
 *  UTF8 lambda is 0xce 0xbb.
 *
 *
 * --------
 * void glistener_set_prompt_tag(glistener *g, GtkTextTag *m);
 *
 *  This sets the GtkTextTag for the prompt.  It defaults to NULL, but it's nicer to
 *  set off the prompt in some way (like bold-face).  Here's a tag that makes our prompt
 *  bold red:   
 *    glistener_set_prompt_tag(g, 
 *      gtk_text_buffer_create_tag(buffer, "glistener_prompt_tag", "weight", PANGO_WEIGHT_BOLD, "foreground", "red", NULL));
 *
 *   
 * --------
 * int glistener_prompt_position(glistener *g);
 *
 *  This returns the current (active) prompt offset in the listener text buffer.
 *
 * 
 * --------
 * void glistener_set_cursor_shape(glistener *g, GdkCursor *cursor_shape);
 *
 *  This sets the current cursor shape.  It is normally an arrow, but changes to
 *  a watch or hour-glass during evaluation:
 *    gdk_cursor_new(GDK_WATCH) or gdk_cursor_new(GDK_LEFT_PTR)
 *
 *
 * --------
 * int glistener_cursor_position(glistener *g);
 *
 *  This returns the cursor offset in the listener.
 *
 *
 * --------
 * void glistener_set_cursor_position(glistener *g, int position);
 *
 *  This moves the listener's cursor to the offset "position".
 *
 *				      
 * --------
 * void glistener_scroll_to_end(glistener *g);
 *
 *  This scrolls the view of the listener's text to the end of the text.
 *
 *				      
 * --------
 * void glistener_post_status(glistener *g, const char *msg);
 *
 *  This places "msg" in the listener's statusbar.  The previous text, if any
 *  is removed, so you don't need to worry about gtk_statusbar_pop.
 *
 *
 * --------
 * void glistener_clear_status(glistener *g);
 *
 *  This removes all messages from the statusbar.
 *
 *
 * --------
 * void glistener_clear(glistener *g);
 *
 *  This deletes all the text in the listener, leaving only the initial prompt.
 *
 *
 * --------
 * bool glistener_write(glistener *g, FILE *fp);
 *
 *  This writes the current listener text contents to the file "fp".
 *
 *
 * --------
 * void glistener_set_highlight_tag(glistener *g, GtkTextTag *m);
 *
 *  This sets the GtkTexTag for highlighted text, normally marking an unmatched
 *  open parenthesis.  The default is bold face, red foreground, but I may change
 *  that to red backgound.  See glistener_set_prompt_tag above for an example.
 *				      
 *
 * --------
 * void glistener_set_font(glistener *g, PangoFontDescription *font);
 *
 *  This sets the listener text font.  It defaults to "Monospace 11".  Indentation
 *  code assumes fixed-width spacing, but that's not a big deal.  
 *    glistener_set_font(g, pango_font_description_from_string("Monospace 10"));
 *
 *				      
 * --------
 * void glistener_set_text_color(glistener *g, GdkRGBA *p);
 *
 *  This sets the text foreground color, normally black.  In gtk 2, the color
 *  is GdkColor*. This snippet makes the text green:
 *    GdkRGBA color;
 *    color.red = 0.0; color.green = 0.97; color.blue = 0.0; color.alpha = 1.0;
 *    glistener_set_text_color(g1, &color);
 *
 *				      
 * --------
 * void glistener_set_background_color(glistener *g, GdkRGBA *p);
 *
 *  This sets the listener background color, normally white.  In gtk 2, use
 *  a GdkColor* instead.  The following code gives the listener a light blue
 *  background:
 *    GdkRGBA color;
 *    color.red = 0.94; color.green = 0.97; color.blue = 1.0; color.alpha = 1.0;
 *    glistener_set_background_color(g1, &color);
 *
 *				      
 * --------
 * void glistener_key_bindings(glistener *g, gpointer cls);
 *
 *  This establishes the listener's emacs-inspired keybindings in the gtk
 *  widget class "cls" (normally gtk_entry).  In Snd I use this to make sure
 *  all the text-oriented widgets share the same basic set:
 *    glistener_key_bindings(g, GTK_ENTRY_GET_CLASS(GTK_ENTRY(text)));
 *
 *				      
 * --------
 * void glistener_is_schemish(glistener *g, bool filtering);
 *
 *  This determines whether the listener tries to parse the text before sending it to
 *  the evaluator.  It defaults to true, expecting more-or-less lispish syntax.  In other
 *  languages (Ruby and Forth in Snd, for example), set this to false.
 *				      
 *
 *
 * --------
 * void glistener_set_completer(glistener *g, void (*completer)(glistener *g, bool (*symbol_func)(const char *symbol_name, void *data), void *data));
 *
 *  The completer is called whenever the user types TAB after what appears to be a
 *  partial symbol name (in a string, the filename completer is called instead).
 *  To find a plausible completion, the listener needs access to the table of currently
 *  known symbols.  In s7, s7_for_each_symbol_name runs through the symbol table,
 *  calling a function on each symbol, and stopping when the function returns true,
 *  so symbol completion in s7 is:
 *
 *    s7_for_each_symbol_name(s7, symbol_func, data);
 *
 *  The completer function in s7 is:
 *
 *    static void completer(glistener *g, bool (*symbol_func)(const char *symbol_name, void *data), void *data)
 *    {
 *      s7_for_each_symbol_name(s7, symbol_func, data);
 *    }
 *
 *  and it is tied into the listener via:
 *
 *    glistener_set_completer(g1, completer);
 *
 *
 *				      
 * --------
 * void glistener_set_helper(glistener *g, const char *(*help)(glistener *g, const char *text));
 *
 *  The helper is called whenever the listener thinks a help string is in order.  It is passed 
 *  a string ("text") about which it hopes to get help.  If the helper returns NULL, nothing 
 *  happens, but otherwise, the returned help is posted in the status area.  In s7, it is:
 *
 *    static const char *helper(glistener *g, const char *text)
 *    {
 *      s7_pointer sym;
 *      sym = s7_symbol_table_find_name(s7, text);
 *      if (sym)
 *        return(s7_help(s7, sym));
 *      glistener_clear_status(g);
 *      return(NULL);
 *    }
 *
 *  and it is tied into the listener via:
 *
 *    glistener_set_helper(g1, helper);
 *  

 *				      
 * --------
 * void glistener_set_evaluator(glistener *g, void (*eval)(glistener *g, const char *text));
 *
 *  This is the heart of the listener.  It gets a string ("text") and
 *  does something debonair.  In s7, it calls s7_eval_c_string, and then
 *  displays the result.  Although error handling makes the code complicated,
 *  the basic idea is:
 *
 *    static void evaluator(glistener *g, const char *text)
 *    {
 *      s7_pointer result;
 *      char *msg;
 *      result = s7_eval_c_string(s7, text);
 *      glistener_append_text(g, "\n");
 *      msg = s7_object_to_c_string(s7, result);
 *      glistener_append_text(g, msg);
 *      if (msg) free(msg);
 *      glistener_append_prompt(g);
 *    }
 *    
 *  tied into the listener via:
 *
 *    glistener_set_evaluator(g1, evaluator);
 *
 *				      
 *
 * --------
 * char *glistener_evaluate(glistener *g);
 * char *glistener_complete(glistener *g);
 *
 *  These two functions are intended for regression testing.  glistener_evaluate
 *  simulates typing <cr>, and glistener_complete simulates <tab>.
 */


#endif
