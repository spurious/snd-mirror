#ifndef SND_G0_H
#define SND_G0_H

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#if USE_CAIRO
  #include <cairo/cairo.h>
#endif

#if HAVE_GL
  #include <gtk/gtkgl.h>
  #include <GL/gl.h>
#endif

#define LOTSA_PIXELS 10000

#define BACKGROUND_QUIT false
#define BACKGROUND_CONTINUE true
#define BACKGROUND_REMOVE(func) g_source_remove(func)
#define BACKGROUND_ADD(func, data) add_work_proc(func, (gpointer)data)

typedef enum {WITH_DEFAULT_BACKGROUND, WITH_WHITE_BACKGROUND} snd_entry_bg_t;

#define widget_t GtkWidget*
#define widget_is_active(Wid) GTK_WIDGET_VISIBLE(Wid)
#define activate_widget(Wid) gtk_widget_show(Wid)
#define deactivate_widget(Wid) gtk_widget_hide(Wid)

#define XEN_WRAP_WIDGET(Value)   ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GtkWidget_"), C_TO_XEN_ULONG((unsigned long)Value)) : XEN_FALSE)
#define XEN_WRAP_WINDOW(Value)   ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GdkWindow_"), C_TO_XEN_ULONG((unsigned long)Value)) : XEN_FALSE)
#define XEN_UNWRAP_WIDGET(Value) (XEN_LIST_P(Value) ? (GtkWidget*)(XEN_TO_C_ULONG(XEN_CADR(Value))) : NULL)
#define XEN_WIDGET_P(Value)      (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                                  (strcmp("GtkWidget_", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))
#define XEN_WRAP_EVENT(Value)    ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GdkEvent_"), C_TO_XEN_ULONG((unsigned long)Value)) : XEN_FALSE)
#define NULL_WIDGET NULL

#if MUS_DEBUGGING
#define ASSERT_WIDGET_TYPE(Cond, Wid) if (!(Cond)) fprintf(stderr, "%s:[%s %d] widget is wrong type", c__FUNCTION__, __FILE__, __LINE__)
#else
  #define ASSERT_WIDGET_TYPE(Cond, Wid)
#endif

#define SG_SIGNAL_CONNECT(Widget, Signal, Function, Data) g_signal_connect(G_OBJECT(Widget), Signal, G_CALLBACK(Function), (gpointer)Data)

#define idle_t guint
#define idle_func_t gboolean
#define any_pointer_t gpointer
#define oclock_t guint32
#define point_t GdkPoint

#if USE_CAIRO
  #define rgb_t double
  #define RGB_MAX 1.0
  #define FLOAT_TO_RGB(Val) (rgb_t)(Val)
  #define RGB_TO_FLOAT(Val) Val

  typedef struct {
    rgb_t red, green, blue;
  } color_info;

  typedef color_info* color_t;

  typedef struct {
    color_t fg_color, bg_color;
  } gc_t;

  #define picture_t GdkPixmap

  #define XEN_WRAP_PIXEL(Value)    XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("color_t"), C_TO_XEN_ULONG((unsigned long)Value))
  #define XEN_UNWRAP_PIXEL(Value)  (color_t)(XEN_TO_C_ULONG(XEN_CADR(Value)))
  #define XEN_PIXEL_P(Value)       (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                                    (strcmp("color_t", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))

#else

  #define rgb_t unsigned short
  #define RGB_MAX 65535
  #define FLOAT_TO_RGB(Val) (rgb_t)(RGB_MAX * (Val))
  #define RGB_TO_FLOAT(Val) (float)((float)(Val) / (float)RGB_MAX)

  #define gc_t GdkGC
  #define picture_t GdkPixmap
  typedef GdkColor* color_t;
  #define color_info GdkColor

  #define XEN_WRAP_PIXEL(Value)    XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GdkColor_"), C_TO_XEN_ULONG((unsigned long)Value))
  #define XEN_UNWRAP_PIXEL(Value)  (GdkColor*)(XEN_TO_C_ULONG(XEN_CADR(Value)))
  #define XEN_PIXEL_P(Value)       (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                                    (strcmp("GdkColor_", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))
#endif

typedef struct {
  gc_t *gc;
  GdkDrawable *wn;
  PangoFontDescription *current_font;
  GtkWidget *w;
#if USE_CAIRO
  cairo_t *cr;
#endif
} axis_context;

typedef struct slist {
  GtkWidget *scroller, *topics, *label, *box;
  GtkWidget **items;
  int num_items, items_size, selected_item;
  void (*select_callback)(const char *name, int row, void *data);
  void *select_callback_data;
  bool (*button_press_callback)(GdkEventButton *event, void *data);
  void *button_press_callback_data;
} slist;

#define SLIST_NO_ITEM_SELECTED -1

typedef struct {
  /* we need two versions of each GC because the selected channel's colors can be different from the unselected channels' */
  GtkWidget **chan_widgets;
  GtkObject **chan_adjs;
  idle_t fft_in_progress;
  idle_t amp_env_in_progress;
  struct env_state *amp_env_state;
  axis_context *ax;
  bool selected;
  slist *edhist_list;
  GdkPixbuf *fft_pix;
  unsigned int fft_pix_width, fft_pix_height;
  int fft_pix_x0, fft_pix_y0;
  bool fft_pix_ready;
#if USE_CAIRO
  GdkPixbuf *cursor_pix;
  unsigned int cursor_pix_width, cursor_pix_height;
  int cursor_pix_x0, cursor_pix_y0;
  bool cursor_pix_ready;
  GdkPixbuf *sono_cursor_pix;
  unsigned int sono_cursor_pix_width, sono_cursor_pix_height;
  int sono_cursor_pix_x0, sono_cursor_pix_y0;
  bool sono_cursor_pix_ready;
#endif
} chan_context;

typedef struct {
  picture_t *file_pix;
  GtkWidget **snd_widgets;
  GtkObject **snd_adjs;
  struct env_editor *flt;
  GtkWidget *dialog;
  int page;
  bool mini_active;
  gulong minibuffer_watcher;
  axis_context *name_pix_ax, *stop_pix_ax, *speed_arrow_ax;
} snd_context;

typedef enum {NOT_A_SCANF_WIDGET, SRATE_WIDGET, CHANS_WIDGET, DATA_LOCATION_WIDGET, SAMPLES_WIDGET} scanf_widget_t;

typedef struct {
  GtkWidget *srate_text, *chans_text, *comment_text, *location_text, *samples_text, *error_text,*dialog, *smenu;
  int current_type, current_format, formats, header_pos, format_pos;
  scanf_widget_t scanf_widget, error_widget;
  bool extracting;
  gulong *reflection_ids;
  slist *header_list, *format_list;
  GtkWidget **srates;
  int num_srates, srates_size;
} file_data;

typedef struct {
  GtkWidget *mainshell;
  GtkWidget *mainpane;
  GtkWidget *soundpane;
  GtkWidget *soundpanebox;
  GtkWidget *listener_pane;
  GdkWindow *mainwindow;

  PangoFontDescription *listener_fnt;
  PangoFontDescription *axis_label_fnt;
  PangoFontDescription *axis_numbers_fnt;
  PangoFontDescription *tiny_fnt;
  PangoFontDescription *peaks_fnt;
  PangoFontDescription *bold_peaks_fnt; 

  color_info *white, *black, *red, *yellow, *green, *light_blue, *lighter_blue;
  color_info *data_color, *selected_data_color, *mark_color, *graph_color, *selected_graph_color, *listener_color, *listener_text_color, *cursor_color;
  color_info *basic_color, *selection_color, *zoom_color, *position_color, *highlight_color, *enved_waveform_color;
  color_info *text_focus_color, *filter_control_waveform_color, *mix_color, *pushed_button_color, *sash_color;
  color_info *help_button_color, *doit_again_button_color, *doit_button_color, *quit_button_color, *reset_button_color;
  color_info *selected_grid_color, *grid_color;

  color_info *orig_data_color, *orig_selected_data_color, *orig_mark_color, *orig_mix_color;
  color_info *orig_graph_color, *orig_selected_graph_color, *orig_listener_color, *orig_listener_text_color, *orig_cursor_color;
  color_info *orig_basic_color, *orig_selection_color, *orig_zoom_color, *orig_position_color, *orig_highlight_color;

  gc_t *basic_gc, *selected_basic_gc, *combined_basic_gc;        
  gc_t *cursor_gc, *selected_cursor_gc;      
  gc_t *selection_gc, *selected_selection_gc;
  gc_t *erase_gc, *selected_erase_gc;        
  gc_t *mark_gc, *selected_mark_gc;          
  gc_t *mix_gc;
  gc_t *fltenv_basic_gc, *fltenv_data_gc;

  GtkWidget **dialogs;
  int num_dialogs, dialogs_size;
  bool graph_is_active;
  GtkWidget *requestor_dialog;
  
  GdkCursor *arrow_cursor, *wait_cursor, *graph_cursor;
  gint fam_port;
  GtkWidget **mw, **pw;
} state_context;

#define DEFAULT_TINY_FONT "Monospace 8"
#define DEFAULT_PEAKS_FONT "Serif 8"
#define DEFAULT_BOLD_PEAKS_FONT "Serif Bold 8"
#define DEFAULT_AXIS_NUMBERS_FONT "Monospace 10"
#define DEFAULT_AXIS_LABEL_FONT "Serif 12"

typedef enum {CONTAINER_ADD, PANED_ADD1, BOX_PACK, TABLE_ATTACH, PANED_ADD2} widget_add_t;
typedef enum {WITHOUT_CHANNELS_FIELD, WITH_CHANNELS_FIELD, WITH_EXTRACT_CHANNELS_FIELD} dialog_channels_t;
typedef enum {WITHOUT_SAMPLES_FIELD, WITH_SAMPLES_FIELD} dialog_samples_t;
typedef enum {WITHOUT_DATA_LOCATION_FIELD, WITH_DATA_LOCATION_FIELD} dialog_data_location_t;
typedef enum {WITHOUT_ERROR_FIELD, WITH_ERROR_FIELD} dialog_error_t;
typedef enum {WITHOUT_HEADER_TYPE_FIELD, WITH_HEADER_TYPE_FIELD} dialog_header_type_t;
typedef enum {WITHOUT_COMMENT_FIELD, WITH_COMMENT_FIELD} dialog_comment_t;

#define snd_ShiftMask GDK_SHIFT_MASK
#define snd_ControlMask GDK_CONTROL_MASK
#ifndef MUS_SUN
  #define snd_MetaMask GDK_MOD1_MASK
#else
  #define snd_MetaMask (GDK_MOD1_MASK | GDK_MOD4_MASK)
#endif

#define NO_BUCKY_BITS_P(State) (((State) & (GDK_SHIFT_MASK | GDK_CONTROL_MASK | GDK_MOD1_MASK)) == 0)
/* in some cases, numlock = GDK_MOD2_MASK for example, and we want to completely ignore that setting */

#define BUTTON1_PRESSED(State) ((State) & GDK_BUTTON1_MASK)

#define MAIN_SHELL(a) (a->sgx)->mainshell
#define MAIN_WINDOW(a) (a->sgx)->mainwindow
#define MAIN_PANE(a) (a->sgx)->mainpane
#define SOUND_PANE(a) (a->sgx)->soundpane
#define SOUND_PANE_BOX(a) (a->sgx)->soundpanebox
#define AXIS_NUMBERS_FONT(a) (a->sgx)->axis_numbers_fnt
#define AXIS_LABEL_FONT(a) (a->sgx)->axis_label_fnt
#define LISTENER_FONT(a) (a->sgx)->listener_fnt
#define TINY_FONT(a) (a->sgx)->tiny_fnt
#define PEAKS_FONT(a) (a->sgx)->peaks_fnt
#define BOLD_PEAKS_FONT(a) (a->sgx)->bold_peaks_fnt
#define KEY_TO_NAME(key) gdk_keyval_name(key)

#define DEFAULT_GRAPH_CURSOR GDK_CROSSHAIR
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
#define snd_keypad_Left GDK_KP_Left
#define snd_keypad_Up GDK_KP_Up
#define snd_keypad_Right GDK_KP_Right
#define snd_keypad_Down GDK_KP_Down
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
  
