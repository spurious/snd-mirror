#ifndef SND_NOGUI0_H
#define SND_NOGUI0_H

#define POINT_BUFFER_SIZE 4096
#define BACKGROUND_QUIT true
#define BACKGROUND_CONTINUE false
#define BACKGROUND_REMOVE(func)
#define BACKGROUND_ADD(func, data) func(data)

#define widget_t int
#define g_adj_t int
#define gc_t int
#define widget_is_active(Wid) 0
#define activate_widget(Wid) Wid = 0
#define deactivate_widget(Wid) Wid = 0

#define XEN_WRAP_WIDGET(Value) XEN_FALSE
#define XEN_WRAP_WINDOW(Value) XEN_FALSE
#define XEN_WRAP_GC(Value) XEN_FALSE
#define XEN_WRAP_PIXEL(Value) XEN_FALSE
#define XEN_UNWRAP_WIDGET(Value) 0
#define XEN_UNWRAP_GC(Value) 0
#define XEN_UNWRAP_PIXEL(Value) 0
#define XEN_WIDGET_P(Value) 0
#define NULL_WIDGET 0
#define LOTSA_PIXELS 10000

#define DEFAULT_TINY_FONT "Monospace 8"
#define DEFAULT_PEAKS_FONT "Serif 10"
#define DEFAULT_BOLD_PEAKS_FONT "Serif Bold 10"
#define DEFAULT_AXIS_NUMBERS_FONT "Monospace 10"
#define DEFAULT_AXIS_LABEL_FONT "Serif 14"

#define Cessator int
#define Cessate int
#define Indicium void *
#define Tempus int
#define Locus int
#define Latus int

typedef struct {
  void *ss;
  int wn;
  int gc;
} axis_context;

typedef struct {
  struct env_state *amp_env_state;
  int amp_env_in_progress;
  axis_context *ax;
  int selected;
  int fft_in_progress;
  int fft_pix;
  unsigned int fft_pix_width, fft_pix_height;
  int fft_pix_x0, fft_pix_y0;
  bool fft_pix_ready;
} chan_context;

typedef struct {
  struct env_editor *flt;
} snd_context;

typedef struct {
  int current_type, current_format, formats, header_pos, format_pos, scanf_widget, error_widget;
} file_data;

typedef enum {NOT_A_SCANF_WIDGET} scanf_widget_t;

typedef struct {
  int data_color, selected_data_color, mix_color, basic_color, grid_color, selected_grid_color;
  int white, black, red, yellow, green, light_blue;
  int fltenv_basic_gc, fltenv_data_gc;
  int basic_gc, selected_basic_gc, combined_basic_gc;        
  int cursor_gc, selected_cursor_gc;      
  int selection_gc, selected_selection_gc;
  int erase_gc, selected_erase_gc;        
  int mark_gc, selected_mark_gc;          
  int mix_gc;           
  struct dialog_play_info *ignore_me; /* for the compiler's benefit */
  int requestor_dialog;
} state_context;

typedef struct {
  int *p0, *p1;
  int lastpj, graph, color;
} mix_context;


#define snd_ShiftMask 0
#define snd_ControlMask 0
#define snd_MetaMask 0

#define MAIN_SHELL(a) 0
#define MAIN_PANE(a) 0
#define SOUND_PANE(a) 0
#define color_t int
#define KEY_TO_NAME(key) "?"
/* not NULL here because that causes a segfault in solaris (key name null -> strlen of null in vsprintf) */

enum {snd_K_Shift_L, snd_K_space, snd_K_openparen, snd_K_closeparen, snd_K_plus, snd_K_minus, snd_K_period, snd_K_slash, snd_K_0, snd_K_1, snd_K_2, snd_K_3, snd_K_4, snd_K_5, snd_K_6, snd_K_7, snd_K_8, snd_K_9, snd_K_less, snd_K_greater, snd_K_A, snd_K_B, snd_K_C, snd_K_D, snd_K_E, snd_K_F, snd_K_G, snd_K_H, snd_K_I, snd_K_J, snd_K_K, snd_K_L, snd_K_M, snd_K_N, snd_K_O, snd_K_P, snd_K_Q, snd_K_R, snd_K_S, snd_K_T, snd_K_U, snd_K_V, snd_K_W, snd_K_X, snd_K_Y, snd_K_Z, snd_K_underscore, snd_K_a, snd_K_b, snd_K_c, snd_K_d, snd_K_e, snd_K_f, snd_K_g, snd_K_h, snd_K_i, snd_K_j, snd_K_k, snd_K_l, snd_K_m, snd_K_n, snd_K_o, snd_K_p, snd_K_q, snd_K_r, snd_K_s, snd_K_t, snd_K_u, snd_K_v, snd_K_w, snd_K_x, snd_K_y, snd_K_z, snd_K_Home, snd_K_Left, snd_K_Up, snd_K_Right, snd_K_Down, snd_keypad_PageUp, snd_keypad_PageDown, snd_keypad_Insert, snd_keypad_Delete, snd_keypad_Left, snd_keypad_Up, snd_keypad_Right, snd_keypad_Down, snd_keypad_Multiply, snd_keypad_Add, snd_keypad_Subtract, snd_keypad_Divide, snd_keypad_Decimal, snd_keypad_Enter, snd_keypad_0, snd_keypad_1, snd_keypad_2, snd_keypad_3, snd_keypad_4, snd_keypad_5, snd_keypad_6, snd_keypad_7, snd_keypad_8, snd_keypad_9};

#define XEN_PIXEL_P(Value) (XEN_LIST_P(Value) && \
                           (XEN_LIST_LENGTH(Value) >= 2) && \
                           (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                           (strcmp("Pixel", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))

#endif
