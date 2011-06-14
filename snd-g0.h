#ifndef SND_G0_H
#define SND_G0_H

#include <gtk/gtk.h>

#if HAVE_GTK_3
  #include <gdk/gdk.h>
#else
  #include <gdk/gdkkeysyms.h>
#endif

#define HAVE_GTK true
/* I keep using this form (not USE_GTK) by mistake */

#include <cairo/cairo.h>

#define LOTSA_PIXELS 10000

#define BACKGROUND_QUIT false
#define BACKGROUND_CONTINUE true
#define BACKGROUND_REMOVE(func) g_source_remove(func)
#define BACKGROUND_ADD(func, data) add_work_proc(func, (gpointer)data)

#define TIMEOUT_ARGS                   gpointer context
#define TIMEOUT_TYPE                   gint
#define TIMEOUT_RESULT                 return(0);
#define CALL_TIMEOUT(Func, Wait, Data) g_timeout_add_full(0, Wait, Func, (gpointer)Data, NULL)
#define timeout_result_t               guint
#define TIMEOUT_REMOVE(Id)             g_source_remove(Id)

typedef enum {WITH_DEFAULT_BACKGROUND, WITH_WHITE_BACKGROUND} snd_entry_bg_t;

#define widget_t GtkWidget*

#if (!HAVE_GTK_WIDGET_GET_VISIBLE)
  /* 2.17 -- actually 2.14 also complains but it doesn't provide gtk_widget_get_visible! */
  #define widget_is_active(Wid) GTK_WIDGET_VISIBLE(Wid)
#else
  #define widget_is_active(Wid) gtk_widget_get_visible(Wid)
#endif
#define activate_widget(Wid) gtk_widget_show(Wid)
#define deactivate_widget(Wid) gtk_widget_hide(Wid)

#ifndef GTK_IS_VBOX
#define GTK_IS_VBOX(Obj) GTK_IS_BOX(Obj)
#define GTK_IS_HBOX(Obj) GTK_IS_BOX(Obj)
#define GTK_IS_VPANED(Obj) GTK_IS_PANED(Obj)
#define GTK_IS_HPANED(Obj) GTK_IS_PANED(Obj)
#define GTK_IS_VSCALE(Obj) GTK_IS_SCALE(Obj)
#define GTK_IS_HSCALE(Obj) GTK_IS_SCALE(Obj)

/* TODO: vbox looks like crap because it doesn't change size when its parent does! */
#define gtk_vbox_new(H, S) gtk_box_new(GTK_ORIENTATION_VERTICAL, S)
#define gtk_hbox_new(H, S) gtk_box_new(GTK_ORIENTATION_HORIZONTAL, S)
#define gtk_vscrollbar_new(S) gtk_scrollbar_new(GTK_ORIENTATION_VERTICAL, S)
#define gtk_hscrollbar_new(S) gtk_scrollbar_new(GTK_ORIENTATION_HORIZONTAL, S)
#define gtk_vscale_new(S) gtk_scale_new(GTK_ORIENTATION_VERTICAL, S)
#define gtk_hscale_new(S) gtk_scale_new(GTK_ORIENTATION_HORIZONTAL, S)
#define gtk_vpaned_new() gtk_paned_new(GTK_ORIENTATION_VERTICAL)
#define gtk_hpaned_new() gtk_paned_new(GTK_ORIENTATION_HORIZONTAL)
#define gtk_vseparator_new() gtk_separator_new(GTK_ORIENTATION_VERTICAL)
#define gtk_hseparator_new() gtk_separator_new(GTK_ORIENTATION_HORIZONTAL)
#endif


#define XEN_WRAP_WIDGET(Value)   ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GtkWidget_"), XEN_WRAP_C_POINTER(Value)) : XEN_FALSE)
#define XEN_WRAP_WINDOW(Value)   ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL("GdkWindow_"), XEN_WRAP_C_POINTER(Value)) : XEN_FALSE)
#define XEN_UNWRAP_WIDGET(Value) (XEN_LIST_P(Value) ? (GtkWidget *)(XEN_UNWRAP_C_POINTER(XEN_CADR(Value))) : NULL)
#define XEN_WIDGET_P(Value)      (XEN_LIST_P(Value) && (XEN_LIST_LENGTH(Value) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                                  (strcmp("GtkWidget_", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))
#define NULL_WIDGET NULL
#define SG_SIGNAL_CONNECT(Widget, Signal, Function, Data) g_signal_connect(G_OBJECT(Widget), Signal, G_CALLBACK(Function), (gpointer)Data)


/* GtkType -> GType, GtkSignalFunc -> GCallback version 2.13.4 7-Jul-08 */

#if HAVE_GTK_ADJUSTMENT_GET_UPPER
  /* 2.13.6 */
  #define WIDGET_TO_WINDOW(Widget)                gtk_widget_get_window(Widget)
  #define DIALOG_ACTION_AREA(Dialog)              gtk_dialog_get_action_area(GTK_DIALOG(Dialog))
  #define DIALOG_CONTENT_AREA(Dialog)             gtk_dialog_get_content_area(GTK_DIALOG(Dialog))
  #define ADJUSTMENT_VALUE(Adjust)                gtk_adjustment_get_value(GTK_ADJUSTMENT(Adjust))
  #define ADJUSTMENT_LOWER(Adjust)                gtk_adjustment_get_lower(GTK_ADJUSTMENT(Adjust))
  #define ADJUSTMENT_UPPER(Adjust)                gtk_adjustment_get_upper(GTK_ADJUSTMENT(Adjust))
  #define ADJUSTMENT_SET_UPPER(Adjust, Value)     gtk_adjustment_set_upper(GTK_ADJUSTMENT(Adjust), Value)
  #define ADJUSTMENT_PAGE_SIZE(Adjust)            gtk_adjustment_get_page_size(GTK_ADJUSTMENT(Adjust))
  #define ADJUSTMENT_SET_PAGE_SIZE(Adjust, Value) gtk_adjustment_set_page_size(GTK_ADJUSTMENT(Adjust), Value)
#else
  #define WIDGET_TO_WINDOW(Widget)                ((Widget)->window)
  #define DIALOG_ACTION_AREA(Dialog)              ((GTK_DIALOG(Dialog))->action_area)
  #define DIALOG_CONTENT_AREA(Dialog)             ((GTK_DIALOG(Dialog))->vbox)
  #define ADJUSTMENT_VALUE(Adjust)                ((GTK_ADJUSTMENT(Adjust))->value)
  #define ADJUSTMENT_LOWER(Adjust)                ((GTK_ADJUSTMENT(Adjust))->lower)
  #define ADJUSTMENT_UPPER(Adjust)                ((GTK_ADJUSTMENT(Adjust))->upper)
  #define ADJUSTMENT_SET_UPPER(Adjust, Value)     (GTK_ADJUSTMENT(Adjust))->upper = Value
  #define ADJUSTMENT_PAGE_SIZE(Adjust)            ((GTK_ADJUSTMENT(Adjust))->page_size)
  #define ADJUSTMENT_SET_PAGE_SIZE(Adjust, Value) (GTK_ADJUSTMENT(Adjust))->page_size = Value
#endif

#define ADJUSTMENT_SET_VALUE(Adjust, Value) gtk_adjustment_set_value(GTK_ADJUSTMENT(Adjust), (gdouble)(Value))
/* this is different from setting the value field directly because it calls gtk_adjustment_value_changed
 *   which itself is different (generates a different signal) from gtk_adjustment_changed.
 */
#define TOGGLE_BUTTON_ACTIVE(Button) gtk_toggle_button_get_active((GTK_TOGGLE_BUTTON(Button)))
#define BIN_CHILD(Bin) gtk_bin_get_child(GTK_BIN(Bin))

#if (HAVE_GTK_3) && defined(__GNUC__) && (!(defined(__cplusplus)))
  #define EVENT_STATE(Ev) ({ GdkModifierType Type;  gdk_event_get_state((GdkEvent *)Ev, &Type); Type; })
  #define EVENT_TIME(Ev) gdk_event_get_time((GdkEvent *)Ev)
  #define EVENT_X(Ev) ({ gdouble x, y; gdk_event_get_coords((GdkEvent *)Ev, &x, &y); x; })
  #define EVENT_Y(Ev) ({ gdouble x, y; gdk_event_get_coords((GdkEvent *)Ev, &x, &y); y; })
  /* there's also gtk_get_event_widget */
#else
  #define EVENT_STATE(Ev) (Ev)->state
  #define EVENT_TIME(Ev) (Ev)->time
  #define EVENT_X(Ev) (Ev)->x
  #define EVENT_Y(Ev) (Ev)->y
#endif

#if HAVE_GTK_3
  #define DRAW_SIGNAL "draw"
#else
  #define DRAW_SIGNAL "expose_event"
#endif

#if HAVE_GTK_STATUS_ICON_GET_TITLE
  /* 2.17... */
  #define SET_CAN_FOCUS(Wid) gtk_widget_set_can_focus(Wid, true)
  #define UNSET_CAN_FOCUS(Wid) gtk_widget_set_can_focus(Wid, false)
#else
  #define SET_CAN_FOCUS(Wid) GTK_WIDGET_SET_FLAGS(Wid, GTK_CAN_FOCUS)
  #define UNSET_CAN_FOCUS(Wid) GTK_WIDGET_UNSET_FLAGS(Wid, GTK_CAN_FOCUS)
#endif

/* no accessors: */
#define EVENT_WINDOW(Ev)      (Ev)->window
#define EVENT_BUTTON(Ev)      (Ev)->button
#define EVENT_TYPE(Ev)        (Ev)->type
#define EVENT_KEYVAL(Ev)      (Ev)->keyval
#define EVENT_IS_HINT(Ev)     (Ev)->is_hint

#if (!HAVE_GTK_3)
#define EVENT_AREA_WIDTH(Ev)  (Ev)->area.width
#define EVENT_AREA_HEIGHT(Ev) (Ev)->area.height
#endif

#define idle_t guint
#define idle_func_t gboolean
#define any_pointer_t gpointer
#define oclock_t guint32

typedef struct {
  int x, y;
} point_t;

#define rgb_t gdouble
#define RGB_MAX 1.0
#define FLOAT_TO_RGB(Val) (rgb_t)(Val)
#define RGB_TO_FLOAT(Val) Val

typedef struct {
  rgb_t red, green, blue, alpha;
} color_info;

typedef color_info* color_t;

typedef struct {
  color_t fg_color, bg_color;
} gc_t;

#define picture_t cairo_surface_t

#if HAVE_GTK_3
  typedef GdkWindow Drawable;
  #define DRAWABLE(Widget) GDK_WINDOW(Widget)
  #define IS_DRAWABLE(Widget) GDK_IS_WINDOW(Widget)

  /* as far as I can see, UPDATE_CONTINUOUS is now built-in */
  #define gtk_range_get_update_policy(W) 0
  #define gtk_range_set_update_policy(W, V)
  #define GTK_UPDATE_CONTINUOUS 0
#else
  typedef GdkDrawable Drawable;
  #define DRAWABLE(Widget) GDK_DRAWABLE(Widget)
  #define IS_DRAWABLE(Widget) GDK_IS_DRAWABLE(Widget)
#endif

typedef struct {
  gc_t *gc;
  Drawable *wn;
  PangoFontDescription *current_font;
  GtkWidget *w;
} graphics_context;

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

typedef enum {NOT_A_SCANF_WIDGET, SRATE_WIDGET, CHANS_WIDGET, DATA_LOCATION_WIDGET, SAMPLES_WIDGET} scanf_widget_t;

typedef struct {
  GtkWidget *srate_text, *chans_text, *comment_text, *location_text, *samples_text, *error_text;
  GtkWidget *dialog, *src_button, *auto_comment_button;
  int current_type, current_format, formats, header_pos, format_pos;
  scanf_widget_t scanf_widget, error_widget;
  bool src, auto_comment;
  gulong *reflection_ids;
  slist *header_list, *format_list;
  char *saved_comment;
} file_data;

#define DEFAULT_TINY_FONT "Sans 8"
#define DEFAULT_PEAKS_FONT "Times Medium 10"
#define DEFAULT_BOLD_PEAKS_FONT "Times Bold 10"
#define DEFAULT_AXIS_NUMBERS_FONT "Sans 10"
/* changed 31-May-08 for Gtk+-2.13.1 -- apparently Fixed 10 is no longer defined (or maybe this is Fedora 9's fault?) */
#define DEFAULT_AXIS_LABEL_FONT "Times Medium 14"

typedef enum {CONTAINER_ADD, PANED_ADD1, BOX_PACK, TABLE_ATTACH, PANED_ADD2, BOX_PACK_END} widget_add_t;
typedef enum {WITHOUT_CHANNELS_FIELD, WITH_CHANNELS_FIELD, WITH_EXTRACT_CHANNELS_FIELD} dialog_channels_t;
typedef enum {WITHOUT_SAMPLES_FIELD, WITH_SAMPLES_FIELD} dialog_samples_t;
typedef enum {WITHOUT_DATA_LOCATION_FIELD, WITH_DATA_LOCATION_FIELD} dialog_data_location_t;
typedef enum {WITHOUT_HEADER_TYPE_FIELD, WITH_HEADER_TYPE_FIELD} dialog_header_type_t;
typedef enum {WITHOUT_COMMENT_FIELD, WITH_COMMENT_FIELD} dialog_comment_t;

#define MAIN_SHELL(a) (a)->mainshell
#define MAIN_WINDOW(a) (a)->mainwindow
#define MAIN_PANE(a) (a)->mainpane
#define SOUND_PANE(a) (a)->soundpane
#define SOUND_PANE_BOX(a) (a)->soundpanebox
#define AXIS_NUMBERS_FONT(a) (a)->axis_numbers_fnt
#define AXIS_LABEL_FONT(a) (a)->axis_label_fnt
#define LISTENER_FONT(a) (a)->listener_fnt
#define TINY_FONT(a) (a)->tiny_fnt
#define PEAKS_FONT(a) (a)->peaks_fnt
#define BOLD_PEAKS_FONT(a) (a)->bold_peaks_fnt
#define KEY_TO_NAME(key) gdk_keyval_name(key)

#define DEFAULT_GRAPH_CURSOR GDK_CROSSHAIR
/* #define GUI_CURRENT_TIME(ss) GDK_CURRENT_TIME */

#define snd_ShiftMask GDK_SHIFT_MASK
#define snd_ControlMask GDK_CONTROL_MASK
#if (!HAVE_SUN)
  #define snd_MetaMask GDK_MOD1_MASK
#else
  #define snd_MetaMask (GDK_MOD1_MASK | GDK_MOD4_MASK)
#endif

#define NO_BUCKY_BITS_P(State) (((State) & (GDK_SHIFT_MASK | GDK_CONTROL_MASK | GDK_MOD1_MASK)) == 0)
/* in some cases, numlock = GDK_MOD2_MASK for example, and we want to completely ignore that setting */

#define BUTTON1_PRESSED(State) ((State) & GDK_BUTTON1_MASK)

/* now pull in the key names (gdk/gdkkeysyms.h) 
 * KEY_ added to all these names in gtk 2.90.7
 */
#if defined(GDK_KEY_Shift_L)
#define snd_K_Shift_L GDK_KEY_Shift_L	 
#define snd_K_space GDK_KEY_space 
#define snd_K_openparen GDK_KEY_parenleft 
#define snd_K_closeparen GDK_KEY_parenright 
#define snd_K_plus GDK_KEY_plus 
#define snd_K_minus GDK_KEY_minus 
#define snd_K_period GDK_KEY_period 
#define snd_K_slash GDK_KEY_slash 
#define snd_K_0 GDK_KEY_0 
#define snd_K_1 GDK_KEY_1 
#define snd_K_2 GDK_KEY_2 
#define snd_K_3 GDK_KEY_3 
#define snd_K_4 GDK_KEY_4 
#define snd_K_5 GDK_KEY_5 
#define snd_K_6 GDK_KEY_6 
#define snd_K_7 GDK_KEY_7 
#define snd_K_8 GDK_KEY_8 
#define snd_K_9 GDK_KEY_9 
#define snd_K_less GDK_KEY_less 
#define snd_K_greater GDK_KEY_greater 
#define snd_K_A GDK_KEY_A 
#define snd_K_B GDK_KEY_B 
#define snd_K_C GDK_KEY_C 
#define snd_K_D GDK_KEY_D 
#define snd_K_E GDK_KEY_E 
#define snd_K_F GDK_KEY_F 
#define snd_K_G GDK_KEY_G 
#define snd_K_H GDK_KEY_H 
#define snd_K_I GDK_KEY_I 
#define snd_K_J GDK_KEY_J 
#define snd_K_K GDK_KEY_K 
#define snd_K_L GDK_KEY_L 
#define snd_K_M GDK_KEY_M 
#define snd_K_N GDK_KEY_N 
#define snd_K_O GDK_KEY_O 
#define snd_K_P GDK_KEY_P 
#define snd_K_Q GDK_KEY_Q 
#define snd_K_R GDK_KEY_R 
#define snd_K_S GDK_KEY_S 
#define snd_K_T GDK_KEY_T 
#define snd_K_U GDK_KEY_U 
#define snd_K_V GDK_KEY_V 
#define snd_K_W GDK_KEY_W 
#define snd_K_X GDK_KEY_X 
#define snd_K_Y GDK_KEY_Y 
#define snd_K_Z GDK_KEY_Z 
#define snd_K_underscore GDK_KEY_underscore 
#define snd_K_a GDK_KEY_a 
#define snd_K_b GDK_KEY_b 
#define snd_K_c GDK_KEY_c 
#define snd_K_d GDK_KEY_d 
#define snd_K_e GDK_KEY_e 
#define snd_K_f GDK_KEY_f 
#define snd_K_g GDK_KEY_g 
#define snd_K_h GDK_KEY_h 
#define snd_K_i GDK_KEY_i 
#define snd_K_j GDK_KEY_j 
#define snd_K_k GDK_KEY_k 
#define snd_K_l GDK_KEY_l 
#define snd_K_m GDK_KEY_m 
#define snd_K_n GDK_KEY_n 
#define snd_K_o GDK_KEY_o 
#define snd_K_p GDK_KEY_p 
#define snd_K_q GDK_KEY_q 
#define snd_K_r GDK_KEY_r 
#define snd_K_s GDK_KEY_s 
#define snd_K_t GDK_KEY_t 
#define snd_K_u GDK_KEY_u 
#define snd_K_v GDK_KEY_v 
#define snd_K_w GDK_KEY_w 
#define snd_K_x GDK_KEY_x 
#define snd_K_y GDK_KEY_y 
#define snd_K_z GDK_KEY_z 
#define snd_K_Home  GDK_KEY_Home		 
#define snd_K_Left  GDK_KEY_Left		 
#define snd_K_Up    GDK_KEY_Up		 
#define snd_K_Right GDK_KEY_Right	 
#define snd_K_Down  GDK_KEY_Down

#define snd_keypad_Insert   GDK_KEY_KP_Insert
#define snd_keypad_Delete   GDK_KEY_KP_Delete
#define snd_keypad_Multiply GDK_KEY_KP_Multiply
#define snd_keypad_Add      GDK_KEY_KP_Add
#define snd_keypad_Subtract GDK_KEY_KP_Subtract
#define snd_keypad_Divide   GDK_KEY_KP_Divide
#define snd_keypad_Decimal  GDK_KEY_KP_Decimal
#define snd_keypad_Enter    GDK_KEY_KP_Enter
#define snd_keypad_Up       GDK_KEY_KP_Up
#define snd_keypad_Down     GDK_KEY_KP_Down
#define snd_keypad_Left     GDK_KEY_KP_Left
#define snd_keypad_Right    GDK_KEY_KP_Right

#define snd_keypad_0 GDK_KEY_KP_0
#define snd_keypad_1 GDK_KEY_KP_1
#define snd_keypad_2 GDK_KEY_KP_2
#define snd_keypad_3 GDK_KEY_KP_3
#define snd_keypad_4 GDK_KEY_KP_4
#define snd_keypad_5 GDK_KEY_KP_5
#define snd_keypad_6 GDK_KEY_KP_6
#define snd_keypad_7 GDK_KEY_KP_7
#define snd_keypad_8 GDK_KEY_KP_8
#define snd_keypad_9 GDK_KEY_KP_9

#define snd_K_Return    GDK_KEY_Return
#define snd_K_Tab       GDK_KEY_Tab
#define snd_K_BackSpace GDK_KEY_BackSpace
#define snd_K_Delete    GDK_KEY_Delete
#define snd_K_question  GDK_KEY_question

#else
/* ---------------- old version ---------------- */

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
#define snd_K_Home  GDK_Home		 
#define snd_K_Left  GDK_Left		 
#define snd_K_Up    GDK_Up		 
#define snd_K_Right GDK_Right	 
#define snd_K_Down  GDK_Down

#define snd_keypad_Insert   GDK_KP_Insert
#define snd_keypad_Delete   GDK_KP_Delete
#define snd_keypad_Multiply GDK_KP_Multiply
#define snd_keypad_Add      GDK_KP_Add
#define snd_keypad_Subtract GDK_KP_Subtract
#define snd_keypad_Divide   GDK_KP_Divide
#define snd_keypad_Decimal  GDK_KP_Decimal
#define snd_keypad_Enter    GDK_KP_Enter
#define snd_keypad_Up       GDK_KP_Up
#define snd_keypad_Down     GDK_KP_Down
#define snd_keypad_Left     GDK_KP_Left
#define snd_keypad_Right    GDK_KP_Right

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

#define snd_K_Return    GDK_Return
#define snd_K_Tab       GDK_Tab
#define snd_K_BackSpace GDK_BackSpace
#define snd_K_Delete    GDK_Delete
#define snd_K_question  GDK_question

#endif

#endif
