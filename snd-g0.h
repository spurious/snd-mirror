#ifndef SND_G0_H
#define SND_G0_H

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

#define LOTSA_PIXELS 10000
#define POINT_BUFFER_SIZE 4096

#ifndef COLORMAP_SIZE
  #define COLORMAP_SIZE 512
#endif

#define ACTIVATABLE 1
#define NOT_ACTIVATABLE 0
#define CLOSED_CTRLS_HEIGHT 0

#define NUM_COLORMAPS 16

#define BACKGROUND_QUIT false
#define BACKGROUND_CONTINUE true
#define BACKGROUND_REMOVE(func) gtk_idle_remove(func)
#define BACKGROUND_ADD(ss, func, data) add_work_proc(ss, func, (gpointer)data)

#define widget_t GtkWidget*
#define XEN_WRAP_WIDGET(Value)   ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GtkWidget_"), C_TO_XEN_ULONG((unsigned long)Value)) : XEN_FALSE)
#define XEN_WRAP_WINDOW(Value)   ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GdkWindow_"), C_TO_XEN_ULONG((unsigned long)Value)) : XEN_FALSE)
#define XEN_WRAP_GC(Value)       XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GdkGC_"), C_TO_XEN_ULONG((unsigned long)Value))
#define XEN_WRAP_PIXEL(Value)    XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GdkColor_"), C_TO_XEN_ULONG((unsigned long)Value))
#define XEN_UNWRAP_WIDGET(Value) (XEN_LIST_P(Value) ? (GtkWidget*)(XEN_TO_C_ULONG(XEN_CADR(Value))) : NULL)
#define XEN_UNWRAP_GC(Value)     (GdkGC*)(XEN_TO_C_ULONG(XEN_CADR(Value)))
#define XEN_UNWRAP_PIXEL(Value)  (GdkColor*)(XEN_TO_C_ULONG(XEN_CADR(Value)))
#define XEN_WIDGET_P(Value)      (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                                  (strcmp("GtkWidget_", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))
#define XEN_PIXEL_P(Value)       (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                                  (strcmp("GdkColor_", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))
#define XEN_WRAP_EVENT(Value)    ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GdkEvent_"), C_TO_XEN_ULONG((unsigned long)Value)) : XEN_FALSE)

#define Cessator gint
#define Cessate gint
#define Indicium gpointer
#define Locus gint16
#define Latus guint16
#define Tempus guint32

typedef struct {
  GdkGC *gc;
  GdkDrawable *wn;
  PangoFontDescription *current_font;
  GtkWidget *w;
} axis_context;

typedef struct {
  /* we need two versions of each GC because the selected channel's colors can be different from the unselected channels' */
  GtkWidget **chan_widgets;
  GtkObject **chan_adjs;
  Cessator fft_in_progress;
  Cessator amp_env_in_progress;
  void *amp_env_state;
  axis_context *ax;
  bool selected;
} chan_context;

typedef struct {
  GdkPixmap *file_pix;
  Cessator apply_in_progress;
  GtkWidget **snd_widgets;
  GtkObject **snd_adjs;
  GtkWidget *tab;
  void *flt;
  GtkWidget *dialog;
  bool controls_fixed;
  int page;
  bool mini_active;
} snd_context;

typedef struct {
  GtkWidget *header_list, *format_list, *srate_text, *chans_text, *comment_text, *location_text, *samples_text;
  int current_type, current_format, formats, header_pos, format_pos;
} file_data;

typedef struct {
  GtkWidget *mainshell;
  GtkWidget *mainpane;
  GtkWidget *soundpane;
  GtkWidget *soundpanebox;
  GtkWidget *listener_pane;
  GdkWindow *mainwindow;

  PangoFontDescription *bold_button_fnt; 
  PangoFontDescription *listener_fnt;
  PangoFontDescription *axis_label_fnt;
  PangoFontDescription *axis_numbers_fnt;
  PangoFontDescription *tiny_fnt;
  PangoFontDescription *peaks_fnt;
  PangoFontDescription *bold_peaks_fnt; 

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
  GdkGC *fltenv_basic_gc, *fltenv_data_gc;

  GtkWidget **dialogs;
  bool graph_is_active;
  
  GdkCursor *arrow_cursor, *wait_cursor, *graph_cursor;
} state_context;

typedef struct {
  GtkWidget *graph;
  GdkPoint *p0, *p1;
  int lastpj;
  GdkColor *color;
} mix_context;

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

#define MAIN_SHELL(a) (a->sgx)->mainshell
#define MAIN_WINDOW(a) (a->sgx)->mainwindow
#define MAIN_PANE(a) (a->sgx)->mainpane
#define SOUND_PANE(a) (a->sgx)->soundpane
#define SOUND_PANE_BOX(a) (a->sgx)->soundpanebox
#define AXIS_NUMBERS_FONT(a) ((state_context *)((snd_state *)a)->sgx)->axis_numbers_fnt
#define AXIS_LABEL_FONT(a) ((state_context *)((snd_state *)a)->sgx)->axis_label_fnt
#define color_t GdkColor *
#define KEY_TO_NAME(key) gdk_keyval_name(key)

/* #define GUI_CURRENT_TIME(ss) GDK_CURRENT_TIME */

enum {CONTAINER_ADD, PANED_ADD, BOX_PACK, TABLE_ATTACH};

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

#endif
  
