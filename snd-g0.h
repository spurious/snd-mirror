#ifndef SND_G0_H_LOADED
#define SND_G0_H_LOADED

#if defined(HAVE_CONFIG_H)
  #include <config.h>
#endif

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#if HAVE_GL
#include <gtk/gtkgl.h>
#include <GL/gl.h>
#include <GL/glu.h>
#endif

#ifndef HAVE_GTK2
  #if (GTK_MAJOR_VERSION != 1) || (GTK_MINOR_VERSION > 2)
    #define HAVE_GTK2 1
  #endif
#endif

#define HAVE_XPM 1
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
#define XEN_WRAP_WIDGET(Value)   ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GtkWidget_"), C_TO_XEN_ULONG((unsigned long)Value)) : XEN_FALSE)
#define XEN_WRAP_WINDOW(Value)   ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GdkWindow_"), C_TO_XEN_ULONG((unsigned long)Value)) : XEN_FALSE)
#define XEN_WRAP_GC(Value)       ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GdkGC_"), C_TO_XEN_ULONG((unsigned long)Value)) : XEN_FALSE)
#define XEN_WRAP_PIXEL(Value)    ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GdkColor_"), C_TO_XEN_ULONG((unsigned long)Value)) : XEN_FALSE)
#define XEN_UNWRAP_WIDGET(Value) (XEN_LIST_P(Value) ? XEN_TO_C_ULONG(XEN_CADR(Value)) : 0)
#define XEN_UNWRAP_WINDOW(Value) (XEN_LIST_P(Value) ? XEN_TO_C_ULONG(XEN_CADR(Value)) : 0)
#define XEN_UNWRAP_GC(Value)     (XEN_LIST_P(Value) ? XEN_TO_C_ULONG(XEN_CADR(Value)) : 0)
#define XEN_UNWRAP_PIXEL(Value)  (XEN_LIST_P(Value) ? XEN_TO_C_ULONG(XEN_CADR(Value)) : 0)
#define XEN_WIDGET_P(Value) (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                            (strcmp("GtkWidget_", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))

#define Locus gint16
#define Latus guint16

#if HAVE_GTK2
  #define SG_FONT PangoFontDescription
  #define SG_PIXMAP GdkPixmap
  #define SG_BITMAP GdkBitmap
#else
  #define SG_FONT GdkFont
  #define SG_PIXMAP GdkPixmap
  #define SG_BITMAP GdkBitmap
#endif

typedef struct {
  GdkGC *gc;
  GdkDrawable *wn;
  SG_FONT *current_font;
  GtkWidget *w;
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
  SG_PIXMAP *file_pix;
  SG_BITMAP *file_mask;
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

  SG_FONT *button_fnt;
  SG_FONT *bold_button_fnt; 
  SG_FONT *listener_fnt;
  SG_FONT *axis_label_fnt;
  SG_FONT *axis_numbers_fnt;
  SG_FONT *help_text_fnt;
  SG_FONT *tiny_fnt;

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
  SG_PIXMAP *off_label;
  SG_PIXMAP *on_label;
  SG_PIXMAP *clip_label;
  SG_BITMAP *off_label_mask;
  SG_BITMAP *on_label_mask;
  SG_BITMAP *clip_label_mask;
  SG_FONT *label_font;
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

#if HAVE_GTK2
  #define SG_WHITE_COLOR(Col)                gdk_color_parse("white", &Col)
  #define SG_BLACK_COLOR(Col)                gdk_color_parse("black", &Col)
  #define SG_MAKE_RESIZABLE(Widget)          if (GTK_IS_DIALOG(Widget)) {\
                                               gtk_window_set_default_size(GTK_WINDOW(Widget), -1, -1); \
                                               gtk_window_set_resizable(GTK_WINDOW(Widget), TRUE); }
  #define SG_SET_RESIZABLE(Window, Val)      gtk_window_set_resizable(Window, Val)
  #define SG_SET_SIZE(Widget, Width, Height) if (GTK_IS_DIALOG(Widget)) gtk_window_set_default_size(GTK_WINDOW(Widget), Width, Height)
  #define SG_SET_POSITION(Widget, X, Y)      if (GTK_IS_DIALOG(Widget)) gtk_window_move(GTK_WINDOW(Widget), X, Y)
  #define SG_LABEL_TEXT(Widget)              (char *)gtk_label_get_text(Widget)
  #define SG_SET_GUTTER_SIZE(Widget, Size)
  #define SG_SET_HANDLE_SIZE(Widget, Size)
  #define SG_SET_DRAWING_AREA_SIZE(Widget, Width, Height)
  #define SG_WINDOW_SIZE                     gdk_drawable_get_size
  #define SG_DRAW_PIXMAP                     gdk_draw_drawable
  #define SG_MENU_APPEND(Menu, Child)        gtk_menu_shell_append(GTK_MENU_SHELL(Menu), Child)
  #define SG_MENU_INSERT(Menu, Child, Pos)   gtk_menu_shell_insert(GTK_MENU_SHELL(Menu), Child, Pos)
  #define SG_MENU_BAR_APPEND(Menu, Child)    gtk_menu_shell_append(GTK_MENU_SHELL(Menu), Child)
  #define SG_MENU_ITEM_RIGHT_JUSTIFY(Item)   gtk_menu_item_set_right_justified(GTK_MENU_ITEM(Item), TRUE)
  #define SG_RADIO_BUTTON_GROUP(Button)      gtk_radio_button_get_group(GTK_RADIO_BUTTON(Button))
  #define SG_TOGGLE_BUTTON_SET_STATE(Button, State) gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(Button), State)
  #define SG_TEXT_LENGTH(Widget)              gtk_text_buffer_get_char_count(gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget)))
  #define SG_TEXT_CHARS(Widget, Start, End)   sg_get_text(Widget, Start, End)
  #define SG_TEXT_CLEAR(Widget)               gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(Widget)), "", 0)
  /*  or  gtk_text_buffer_get_bounds (buffer, &start, &end);  gtk_text_buffer_delete (buffer, &start, &end); */
  #define SG_TEXT_FREEZE(Widget)
  #define SG_TEXT_THAW(Widget)
  #define SG_TEXT_DELETE(Widget, Start, End) sg_text_delete(Widget, Start, End)
  #define SG_TEXT_SET_POINT(Widget, Point)   sg_set_cursor(Widget, Point)
  #define SG_TEXT_GET_POINT(Widget)          sg_cursor_position(Widget)
  #define SG_TEXT_UNSELECT(Widget)           sg_unselect_text(Widget)
  #define SG_TEXT_SELECT(Widget, Start, End) sg_select_text(Widget, Start, End)
  #define SG_TEXT_INSERT(Widget, Font, FG, BG, Text, Length) sg_text_insert(Widget, Text)
  #define SG_TEXT_BACKWARD_DELETE(Wid, Num)  sg_text_delete(Wid, SG_TEXT_GET_POINT(Wid) - Num, Num)
  #define SG_SET_FONT(Ax, Font)              gtk_widget_modify_font(Ax->w, Font)
  #define SG_LIST_SELECT_ROW(Widget, Row)    sg_list_select(Widget, Row)
  #define SG_LIST_MOVETO(Widget, Row)        sg_list_moveto(Widget, Row)
  #define SG_LIST_CLEAR(Widget)              gtk_tree_store_clear(GTK_TREE_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(Widget))))
  #define SG_LIST_APPEND(Widget, Str)        sg_list_append(Widget, Str)
  #define SG_LIST_INSERT(Widget, Pos, Str)   sg_list_insert(Widget, Pos, Str)
  #define SG_LIST_SET_TEXT(Widget, Row, Str) sg_list_set_text(Widget, Row, Str)
  #define SG_COLOR_ALLOC(CMap, Color)        gdk_rgb_find_color(CMap, Color)
  #define SG_PIXMAP_NEW(Map, Mask)           sg_pixmap_new(Map, Mask)
  #define SG_PIXMAP_NEW_XYD(Window, Width, Height, Depth) gdk_pixmap_new(Window, Width, Height, Depth)
  #define SG_XPM_TO_PIXMAP(Window, Bits, Mask) gdk_pixmap_create_from_xpm_d(Window, &Mask, NULL, Bits)
  #define SG_PIXMAP_SET(Holder, Map, Mask)   sg_pixmap_set(Holder, Map, Mask)
  #define SG_FONT_LOAD(Font)                 pango_font_description_from_string(Font)
  #define SG_SIGNAL_CONNECT(Object, Name, Func, Func_Data) \
	    g_signal_connect_closure_by_id(Object, g_signal_lookup(Name, G_OBJECT_TYPE(Object)), 0, g_cclosure_new(Func, Func_Data, 0), 0)
  #define SG_SIGNAL_CONNECT_OBJECT(Object, Name, Func, Func_Data) \
            g_signal_connect_closure_by_id(Object, g_signal_lookup(Name, G_OBJECT_TYPE(Object)), 0, g_cclosure_new_swap(Func, Func_Data, 0), 0)
  #define SG_SIGNAL_CONNECT_AFTER(Object, Name, Func, Func_Data) \
	    g_signal_connect_closure_by_id(Object, g_signal_lookup(Name, G_OBJECT_TYPE(Object)), 0, g_cclosure_new(Func, Func_Data, 0), 1)
  #define SG_SIGNAL_HANDLER_BLOCK_BY_DATA(object,data) g_signal_handlers_block_matched(object, G_SIGNAL_MATCH_DATA, 0, 0, NULL, 0, data)
  #define SG_SIGNAL_HANDLER_UNBLOCK_BY_DATA(object,data) g_signal_handlers_unblock_matched(object, G_SIGNAL_MATCH_DATA, 0, 0, NULL, 0, data)
  #define SG_SIGNAL_EMIT_STOP_BY_NAME(object, name) g_signal_stop_emission(object, g_signal_lookup(name, G_OBJECT_TYPE(object)), 0)
#else
  #define SG_WHITE_COLOR(Col) 	             gdk_color_white(gdk_colormap_get_system(), &Col)
  #define SG_BLACK_COLOR(Col) 	             gdk_color_black(gdk_colormap_get_system(), &Col)
  #define SG_WINDOW_SIZE                     gdk_window_get_size
  #define SG_DRAW_PIXMAP                     gdk_draw_pixmap
  #define SG_MENU_APPEND(Menu, Child)        gtk_menu_append(GTK_MENU(Menu), Child)
  #define SG_MENU_INSERT(Menu, Child, Pos)   gtk_menu_insert(GTK_MENU(Menu), Child, Pos)
  #define SG_MENU_BAR_APPEND(Menu, Child)    gtk_menu_bar_append(GTK_MENU_BAR(Menu), Child)
  #define SG_MENU_ITEM_RIGHT_JUSTIFY(Item)   gtk_menu_item_right_justify(GTK_MENU_ITEM(Item))
  #define SG_RADIO_BUTTON_GROUP(Button)      gtk_radio_button_group(GTK_RADIO_BUTTON(Button))
  #define SG_TOGGLE_BUTTON_SET_STATE(Button, State) gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(Button), State)
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
  #define SG_TEXT_DELETE(Widget, Start, End) sg_text_delete(Widget, Start, End)
  #define SG_LIST_SELECT_ROW(Widget, Row)    gtk_clist_select_row(GTK_CLIST(Widget), Row, 0)
  #define SG_LIST_MOVETO(Widget, Row)        gtk_clist_moveto(GTK_CLIST(Widget), Row, 0, 0.5, 0.5);
  #define SG_LIST_CLEAR(Widget)              gtk_clist_clear(GTK_CLIST(Widget))
  #define SG_LIST_APPEND(Widget, Str)        gtk_clist_append(GTK_CLIST(Widget), &Str)
  #define SG_LIST_INSERT(Widget, Pos, Str)   gtk_clist_insert(GTK_CLIST(Widget), Pos, &Str)
  #define SG_LIST_SET_TEXT(Widget, Row, Str) gtk_clist_set_text(GTK_CLIST(Widget), Row, 0, Str)
  #define SG_PIXMAP_NEW(Map, Mask)           gtk_pixmap_new(Map, Mask)
  #define SG_PIXMAP_NEW_XYD(Window, Width, Height, Depth) gdk_pixmap_new(Window, Width, Height, Depth)
  #define SG_PIXMAP_SET(Holder, Map, Mask)   gtk_pixmap_set(GTK_PIXMAP(Holder), Map, Mask)
  #define SG_XPM_TO_PIXMAP(Window, Bits, Mask) gdk_pixmap_create_from_xpm_d(Window, &Mask, NULL, Bits)
  #define SG_COLOR_ALLOC(CMap, Color)        gdk_color_alloc(CMap, Color)
  #define SG_FONT_LOAD(Font)                 gdk_font_load(Font)
  #define SG_SET_FONT(Ax, Font)              gdk_gc_set_font(Ax->gc, Font)
  #define SG_SIGNAL_CONNECT(Object, Name, Func, Func_Data) gtk_signal_connect(Object, Name, Func, Func_Data)
  #define SG_SIGNAL_CONNECT_AFTER(Object, Name, Func, Func_Data) gtk_signal_connect_after(Object, Name, Func, Func_Data)
  #define SG_SIGNAL_CONNECT_OBJECT(Object, Name, Func, Func_Data) gtk_signal_connect_object(Object, Name, Func, Func_Data)
  #define SG_SIGNAL_HANDLER_BLOCK_BY_DATA(object,data) gtk_signal_handler_block_by_data(object,data) 
  #define SG_SIGNAL_HANDLER_UNBLOCK_BY_DATA(object,data) gtk_signal_handler_unblock_by_data(object,data)
  #define SG_SIGNAL_EMIT_STOP_BY_NAME(object, name) gtk_signal_emit_stop_by_name(object, name)
#endif

#endif
  
