#ifndef SND_G0_H_LOADED
#define SND_G0_H_LOADED

#if defined(HAVE_CONFIG_H)
  #include "config.h"
#endif

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#ifndef HAVE_GTK2
  #if (GTK_MAJOR_VERSION != 1) || (GTK_MINOR_VERSION > 2)
    #define HAVE_GTK2 1
  #endif
#endif

#define HAVE_XPM 1
#define HAVE_CLICK_FOR_HELP 0

#define LOTSA_PIXELS 10000

#define POINT_BUFFER_SIZE 4096

#ifdef WITH_BIG_COLORMAP
  #define COLORMAP_SIZE 512
#else
  #define COLORMAP_SIZE 64
#endif

#define ACTIVATABLE 1
#define NOT_ACTIVATABLE 0
#define CLOSED_CTRLS_HEIGHT 0

#define NUM_GLASSES 15
#define NUM_BOMBS 15
#define NUM_COLORMAPS 16

#define BACKGROUND_TYPE gint
#define BACKGROUND_QUIT FALSE
#define BACKGROUND_CONTINUE TRUE
#define BACKGROUND_FUNCTION_TYPE gint
#define BACKGROUND_REMOVE(func) gtk_idle_remove(func)
#define BACKGROUND_ADD(ss, func, data) gtk_idle_add(func, (gpointer)data)

#define GUI_POINTER gpointer
#define GUI_WIDGET GtkWidget*
#define GUI_PIXEL GdkColor*

#if DEBUGGING
  #define ASSERT_WIDGET_TYPE(Cond, Wid) if (!(Cond)) fprintf(stderr,"%s:[%s %d] widget arg is wrong type", __FUNCTION__, __FILE__, __LINE__)
  /* GTK_IS_TOGGLE_BUTTON(obj) etc -- not currently used */
#else
  #define ASSERT_WIDGET_TYPE(Cond, Wid)
#endif

#define Locus gint16
#define Latus guint16

typedef struct {
  GdkGC *gc;
  GdkDrawable *wn;
  GdkFont *current_font;
  void *ss;
} axis_context;

typedef struct {
  /* we need two versions of each GC because the selected channel's colors can be different from the unselected channels' */
  GtkWidget **chan_widgets;
  GtkObject **chan_adjs;
  BACKGROUND_FUNCTION_TYPE fft_in_progress;
  BACKGROUND_FUNCTION_TYPE amp_env_in_progress;
  void *amp_env_state;
  axis_context *ax;
  int selected;
} chan_context;

typedef struct {
  GdkPixmap *file_pix;
  GdkBitmap *file_mask;
  BACKGROUND_FUNCTION_TYPE apply_in_progress;
  GtkWidget **snd_widgets;
  GtkObject **snd_adjs;
  GtkWidget *tab;
  void *flt;
  GtkWidget *dialog;
  int controls_fixed, page, mini_active;
} snd_context;

typedef struct {
  GtkWidget *header_list, *format_list, *srate_text, *chans_text, *comment_text, *location_text;
  int current_type, current_format, formats, header_pos, format_pos;
} file_data;

typedef struct {
  GtkWidget *mainshell;
  GtkWidget *mainpane;
  GtkWidget *soundpane;
  GtkWidget *soundpanebox;
  GtkWidget *listener_pane;
  GdkWindow *mainwindow;

  GdkFont *button_fnt;
  GdkFont *bold_button_fnt; 
  GdkFont *listener_fnt;
  GdkFont *axis_label_fnt;
  GdkFont *axis_numbers_fnt;
  GdkFont *help_text_fnt;
  GdkFont *tiny_fnt;

  GdkColor *white, *black, *red, *yellow, *green, *light_blue, *lighter_blue;
  GdkColor *data_color, *selected_data_color, *mark_color, *graph_color, *selected_graph_color, *listener_color, *listener_text_color, *cursor_color;
  GdkColor *basic_color, *selection_color, *zoom_color, *position_color, *highlight_color, *enved_waveform_color;
  GdkColor *selected_mix_color, *text_focus_color, *filter_waveform_color, *mix_color, *pushed_button_color, *sash_color;

  GdkGC *basic_gc, *selected_basic_gc, *combined_basic_gc;        
  GdkGC *cursor_gc, *selected_cursor_gc;      
  GdkGC *selection_gc, *selected_selection_gc;
  GdkGC *erase_gc, *selected_erase_gc;        
  GdkGC *mark_gc, *selected_mark_gc;          
  GdkGC *mix_gc, *selected_mix_gc;    
  GdkGC *fltenv_basic_gc, *fltenv_data_gc, *speed_gc;

  GtkWidget **dialogs;
  int graph_is_active;
  
  GdkCursor *arrow_cursor, *wait_cursor, *mix_cursor, *graph_cursor;
} state_context;

typedef struct {
  GtkWidget *graph;
  GdkPoint *p0, *p1;
  int lastpj;
  GdkColor *color;
} mix_context;

typedef struct {
  GdkPixmap *off_label;
  GdkPixmap *on_label;
  GdkPixmap *clip_label;
  GdkBitmap *off_label_mask;
  GdkBitmap *on_label_mask;
  GdkBitmap *clip_label_mask;
  GdkFont *label_font;
  Float size;
} vu_label;

#define WITHOUT_PANED_WINDOW 0
#define WITH_PANED_WINDOW 1
#define DONT_PAD_TITLE 0
#define PAD_TITLE_ON_RIGHT 1
#define PAD_TITLE_ON_LEFT 2
#define WITHOUT_SORT_BUTTON 0
#define WITH_SORT_BUTTON 1

typedef struct {
  GtkWidget *ww;
  GtkWidget *list;
  GtkWidget *plw;
  GtkWidget *svw;
  GtkWidget *dbline;
  GtkWidget *bydate;
  GtkWidget *bysize;
  GtkWidget *byname;
  GtkWidget *byentry;
  GtkWidget *byproc;
  GtkWidget *panes, *toppane, *tophbox;
} ww_info;

#define snd_ShiftMask GDK_SHIFT_MASK
#define snd_ControlMask GDK_CONTROL_MASK
#ifndef SUN
  #define snd_MetaMask GDK_MOD1_MASK
#else
  #define snd_MetaMask (GDK_MOD1_MASK | GDK_MOD4_MASK)
#endif

#define TIME_TYPE guint32
#define BUTTON_1 1
#define BUTTON_2 2
#define BUTTON_3 3

#define MAIN_SHELL(a) (a->sgx)->mainshell
#define MAIN_WINDOW(a) (a->sgx)->mainwindow
#define MAIN_PANE(a) (a->sgx)->mainpane
#define SOUND_PANE(a) (a->sgx)->soundpane
#define SOUND_PANE_BOX(a) (a->sgx)->soundpanebox
#define AXIS_NUMBERS_FONT(a) ((state_context *)((snd_state *)a)->sgx)->axis_numbers_fnt
#define AXIS_LABEL_FONT(a) ((state_context *)((snd_state *)a)->sgx)->axis_label_fnt
#define COLOR_TYPE GdkColor *
#define KEY_TO_NAME(key) gdk_keyval_name(key)

/* #define GUI_CURRENT_TIME(ss) GDK_CURRENT_TIME */


/* now pull in the key names (gdk/gdkkeysyms.h) */
#define snd_K_Shift_L GDK_Shift_L	 
#define snd_K_space GDK_space 
#define snd_K_openparen GDK_parenleft 
#define snd_K_closeparen GDK_parenright 
#define snd_K_plus GDK_plus 
#define snd_K_minus GDK_minus 
#define snd_K_period GDK_period 
#define snd_K_slash GDK_slash 
#define snd_K_0 GDK_0 
#define snd_K_1 GDK_1 
#define snd_K_2 GDK_2 
#define snd_K_3 GDK_3 
#define snd_K_4 GDK_4 
#define snd_K_5 GDK_5 
#define snd_K_6 GDK_6 
#define snd_K_7 GDK_7 
#define snd_K_8 GDK_8 
#define snd_K_9 GDK_9 
#define snd_K_less GDK_less 
#define snd_K_equal GDK_equal 
#define snd_K_greater GDK_greater 
#define snd_K_A GDK_A 
#define snd_K_B GDK_B 
#define snd_K_C GDK_C 
#define snd_K_D GDK_D 
#define snd_K_E GDK_E 
#define snd_K_F GDK_F 
#define snd_K_G GDK_G 
#define snd_K_H GDK_H 
#define snd_K_I GDK_I 
#define snd_K_J GDK_J 
#define snd_K_K GDK_K 
#define snd_K_L GDK_L 
#define snd_K_M GDK_M 
#define snd_K_N GDK_N 
#define snd_K_O GDK_O 
#define snd_K_P GDK_P 
#define snd_K_Q GDK_Q 
#define snd_K_R GDK_R 
#define snd_K_S GDK_S 
#define snd_K_T GDK_T 
#define snd_K_U GDK_U 
#define snd_K_V GDK_V 
#define snd_K_W GDK_W 
#define snd_K_X GDK_X 
#define snd_K_Y GDK_Y 
#define snd_K_Z GDK_Z 
#define snd_K_underscore GDK_underscore 
#define snd_K_a GDK_a 
#define snd_K_b GDK_b 
#define snd_K_c GDK_c 
#define snd_K_d GDK_d 
#define snd_K_e GDK_e 
#define snd_K_f GDK_f 
#define snd_K_g GDK_g 
#define snd_K_h GDK_h 
#define snd_K_i GDK_i 
#define snd_K_j GDK_j 
#define snd_K_k GDK_k 
#define snd_K_l GDK_l 
#define snd_K_m GDK_m 
#define snd_K_n GDK_n 
#define snd_K_o GDK_o 
#define snd_K_p GDK_p 
#define snd_K_q GDK_q 
#define snd_K_r GDK_r 
#define snd_K_s GDK_s 
#define snd_K_t GDK_t 
#define snd_K_u GDK_u 
#define snd_K_v GDK_v 
#define snd_K_w GDK_w 
#define snd_K_x GDK_x 
#define snd_K_y GDK_y 
#define snd_K_z GDK_z 
#define snd_K_Home GDK_Home		 
#define snd_K_Left GDK_Left		 
#define snd_K_Up GDK_Up		 
#define snd_K_Right GDK_Right	 
#define snd_K_Down GDK_Down		 

#define snd_keypad_PageUp GDK_KP_Page_Up
#define snd_keypad_PageDown GDK_KP_Page_Down
#define snd_keypad_Insert GDK_KP_Insert
#define snd_keypad_Delete GDK_KP_Delete
#if !defined(UW2)
#define snd_keypad_Left GDK_KP_Left
#define snd_keypad_Up GDK_KP_Up
#define snd_keypad_Right GDK_KP_Right
#define snd_keypad_Down GDK_KP_Down
#endif
#define snd_keypad_Multiply GDK_KP_Multiply
#define snd_keypad_Add GDK_KP_Add
#define snd_keypad_Subtract GDK_KP_Subtract
#define snd_keypad_Divide GDK_KP_Divide
#define snd_keypad_Decimal GDK_KP_Decimal
#define snd_keypad_Enter GDK_KP_Enter

#define snd_keypad_0 GDK_KP_0
#define snd_keypad_1 GDK_KP_1
#define snd_keypad_2 GDK_KP_2
#define snd_keypad_3 GDK_KP_3
#define snd_keypad_4 GDK_KP_4
#define snd_keypad_5 GDK_KP_5
#define snd_keypad_6 GDK_KP_6
#define snd_keypad_7 GDK_KP_7
#define snd_keypad_8 GDK_KP_8
#define snd_keypad_9 GDK_KP_9

/* now gtk 2.0 compatibility stuff (work in progress)
 *
 *   besides the usual pointless name changes:
 *    GtkText, pixmap, clist support gone, GdkFont
 *    Text -> TextView and TextBuffer
 *    CList -> TreeView?
 *    gtk_pixmap -> gdk_pixmap?
 *    gdkfont -> pango? 
 *    gdk_color_alloc|free?
 *    gdk_input_add? gdk_color_white|black?
 *    gdk_gc_set_font?
 *
 *    gtk_entry_set_text changed res type
 *    gtk_paned_set_handle_size and gtk_paned_set_gutter_size removed
 *    gtk_drawing_area_size? gdk_draw_string? gdk_text_width|extents
 */

#if HAVE_GTK2

  #define SG_MAKE_RESIZABLE(Widget) gtk_widget_set_size_request(Widget, 0, 0); gtk_window_set_resizable(GTK_WINDOW(Widget), TRUE)
  #define SG_SET_RESIZABLE(Window, Val) gtk_window_set_resizable(Window, Val)
  #define SG_SET_SIZE(Widget, Width, Height) gtk_widget_set_size_request(Widget, Width, Height)
  #define SG_SET_POSITION(Widget, X, Y) gtk_window_move(GTK_WINDOW(Widget), X, Y)
  #define SG_LABEL_TEXT(Widget) (char *)gtk_label_get_text(Widget)
  #define SG_SET_GUTTER_SIZE(Widget, Size)
  #define SG_SET_HANDLE_SIZE(Widget, Size)
  #define SG_SET_DRAWING_AREA_SIZE(Widget, Width, Height)

  #ifndef gdk_window_get_size
    #define gdk_window_get_size gdk_drawable_get_size
  #endif
  #ifndef gdk_draw_pixmap
    #define gdk_draw_pixmap gdk_draw_drawable
  #endif
  #ifndef gtk_menu_append
    #define gtk_menu_append(Menu, Child) gtk_menu_shell_append((GtkMenuShell *)(Menu), (Child))
  #endif
  #ifndef gtk_menu_insert
    #define gtk_menu_insert(Menu, Child, Pos) gtk_menu_shell_insert((GtkMenuShell *)(Menu), (Child), (Pos))
  #endif
  #ifndef gtk_menu_bar_append
    #define gtk_menu_bar_append(Menu, Child) gtk_menu_shell_append((GtkMenuShell *)(Menu),(Child))
  #endif
  #ifndef gtk_menu_item_right_justify
    #define gtk_menu_item_right_justify(Menu_item) gtk_menu_item_set_right_justified((Menu_item), TRUE)
  #endif
  #ifndef gtk_radio_button_group
    #define gtk_radio_button_group gtk_radio_button_get_group
  #endif

  #define SG_TEXT_LENGTH(Widget) gtk_text_buffer_get_char_count(gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget)))
  #define SG_TEXT_CHARS(Widget, Start, End) sg_get_text(Widget, Start, End)
  #define SG_TEXT_CLEAR(Widget) gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget)), "", 0)
  #define SG_TEXT_FREEZE(Widget)
  #define SG_TEXT_THAW(Widget)

  /* TODO: gtk2 macros for text cursor selection insertion deletion */
  #define SG_TEXT_SET_POINT(Widget, Point) sg_set_cursor(Widget, Point)
  #define SG_TEXT_GET_POINT(Widget) 0
  #define SG_TEXT_UNSELECT(Widget)
  #define SG_TEXT_SELECT(Widget, Start, End)
  #define SG_TEXT_INSERT(Widget, Font, ForeColor, BackColor, Text, Length) sg_text_insert(Widget, Text)
  #define SG_TEXT_BACKWARD_DELETE(Widget, Number)

#if 0
  /* TODO: macros for lists pixmaps fonts */
  #define SG_LIST_SELECT_ROW(Widget, Row)
  #define SG_LIST_MOVETO(Widget, Row)
  #define SG_LIST_CLEAR(Widget)
  #define SG_LIST_APPEND(Widget, Str)
  #define SG_LIST_INSERT(Widget, Position, Str)
  #define SG_LIST_SET_TEXT(Widget, Row, Str) 
  #define SG_PIXMAP_NEW(Map, Mask) NULL
  /* perhaps returns drawingarea?? widget (will hold the pixmap and others) */
  #define SG_PIXMAP_SET(Holder, Map, Mask)
  /* now holder is a widget and we set its pixmap -- how to tell this? */
  #define SG_FONT_LOAD(Font) NULL
#else
  /* use deprecated forms for testing */
  #define SG_LIST_SELECT_ROW(Widget, Row)    gtk_clist_select_row(GTK_CLIST(Widget), Row, 0)
  #define SG_LIST_MOVETO(Widget, Row)        gtk_clist_moveto(GTK_CLIST(Widget), Row, 0, 0.5, 0.5);
  #define SG_LIST_CLEAR(Widget)              gtk_clist_clear(GTK_CLIST(Widget))
  #define SG_LIST_APPEND(Widget, Str)        gtk_clist_append(GTK_CLIST(Widget), &Str)
  #define SG_LIST_INSERT(Widget, Pos, Str)   gtk_clist_insert(GTK_CLIST(Widget), Pos, &Str)
  #define SG_LIST_SET_TEXT(Widget, Row, Str) gtk_clist_set_text(GTK_CLIST(Widget), Row, 0, Str)
  #define SG_PIXMAP_NEW(Map, Mask)           gtk_pixmap_new(Map, Mask)
  #define SG_PIXMAP_SET(Holder, Map, Mask)   gtk_pixmap_set(GTK_PIXMAP(Holder), Map, Mask)
  #define SG_FONT_LOAD(Font)                 gdk_font_load(Font)
#endif

#else

  #define SG_MAKE_RESIZABLE(Widget)          gtk_window_set_policy(GTK_WINDOW(Widget), TRUE, TRUE, FALSE)
  #define SG_SET_RESIZABLE(Window, Val)      gtk_window_set_policy(Window, TRUE, TRUE, Val)
  #define SG_SET_SIZE(Widget, Width, Height) gtk_widget_set_usize(Widget, Width, Height)
  #define SG_SET_POSITION(Widget, X, Y)      gtk_widget_set_uposition(Widget, X, Y)
  #define SG_LABEL_TEXT(Widget)              sg_label_text(Widget)
  #define SG_SET_GUTTER_SIZE(Widget, Size)   gtk_paned_set_gutter_size(Widget,Size)
  #define SG_SET_HANDLE_SIZE(Widget, Size)   gtk_paned_set_handle_size(Widget,Size)
  #define SG_SET_DRAWING_AREA_SIZE(Widget, Width, Height) gtk_drawing_area_size(Widget, Width, Height)
  #define SG_TEXT_LENGTH(Widget)             gtk_text_get_length(GTK_TEXT(Widget))
  #define SG_TEXT_CHARS(Widget, Start, End)  gtk_editable_get_chars(GTK_EDITABLE(Widget), Start, End)
  #define SG_TEXT_CLEAR(Widget)              if (SG_TEXT_LENGTH(Widget) > 0) gtk_editable_delete_text(GTK_EDITABLE(Widget), 0, -1)
  #define SG_TEXT_FREEZE(Widget)             gtk_text_freeze(GTK_TEXT(Widget))
  #define SG_TEXT_THAW(Widget)               gtk_text_freeze(GTK_TEXT(Widget))
  #define SG_TEXT_SET_POINT(Widget, Point)   gtk_text_set_point(GTK_TEXT(Widget), Point); gtk_editable_set_position(GTK_EDITABLE(Widget), Point)
  #define SG_TEXT_GET_POINT(Widget)          gtk_editable_get_position(GTK_EDITABLE(Widget))
  #define SG_TEXT_UNSELECT(Widget)           gtk_editable_select_region(GTK_EDITABLE(Widget), 0, 0)
  #define SG_TEXT_SELECT(Widget, Start, End) gtk_editable_select_region(GTK_EDITABLE(Widget), Start, End)
  #define SG_TEXT_INSERT(Widget, Font, FG, BG, Text, Length) gtk_text_insert(GTK_TEXT(Widget), Font, FG, BG, Text, Length)
  #define SG_TEXT_BACKWARD_DELETE(Wid, Num)  gtk_text_backward_delete(GTK_TEXT(Wid), Num)
  #define SG_LIST_SELECT_ROW(Widget, Row)    gtk_clist_select_row(GTK_CLIST(Widget), Row, 0)
  #define SG_LIST_MOVETO(Widget, Row)        gtk_clist_moveto(GTK_CLIST(Widget), Row, 0, 0.5, 0.5);
  #define SG_LIST_CLEAR(Widget)              gtk_clist_clear(GTK_CLIST(Widget))
  #define SG_LIST_APPEND(Widget, Str)        gtk_clist_append(GTK_CLIST(Widget), &Str)
  #define SG_LIST_INSERT(Widget, Pos, Str)   gtk_clist_insert(GTK_CLIST(Widget), Pos, &Str)
  #define SG_LIST_SET_TEXT(Widget, Row, Str) gtk_clist_set_text(GTK_CLIST(Widget), Row, 0, Str)
  #define SG_PIXMAP_NEW(Map, Mask)           gtk_pixmap_new(Map, Mask)
  #define SG_PIXMAP_SET(Holder, Map, Mask)   gtk_pixmap_set(GTK_PIXMAP(Holder), Map, Mask)
  #define SG_FONT_LOAD(Font)                 gdk_font_load(Font)

#endif

#endif
