#ifndef SND_NOGUI0_H_LOADED
#define SND_NOGUI0_H_LOADED

#if defined(HAVE_CONFIG_H)
  #include <config.h>
#endif

#define TIME_TYPE int
#define POINT_BUFFER_SIZE 4096
#define COLORMAP_SIZE 1

#define BACKGROUND_TYPE int
#define BACKGROUND_QUIT TRUE
#define BACKGROUND_CONTINUE FALSE
#define BACKGROUND_FUNCTION_TYPE int
#define BACKGROUND_REMOVE(func)
#define BACKGROUND_ADD(ss, func, data) func(data)

#define GUI_POINTER void *
#define GUI_WIDGET int
#define XEN_WRAP_WIDGET(Value) 0
#define XEN_WRAP_WINDOW(Value) 0
#define XEN_WRAP_GC(Value) 0
#define XEN_WRAP_PIXEL(Value) 0
#define XEN_UNWRAP_WIDGET(Value) 0
#define XEN_UNWRAP_GC(Value) 0
#define XEN_UNWRAP_PIXEL(Value) 0
#define XEN_WIDGET_P(Value) 0

#define BUTTON_1 0
#define BUTTON_2 1
#define BUTTON_3 2
#define NUM_COLORMAPS 0

#define Locus int
#define Latus int

typedef struct {
  void *ss;
  int wn;
  int gc;
} axis_context;

typedef struct {
  void *amp_env_state;
  int amp_env_in_progress;
  axis_context *ax;
  int selected;
  int fft_in_progress;
} chan_context;

typedef struct {
  int apply_in_progress;
  void *flt;
} snd_context;

typedef struct {
  int current_type, current_format, formats, header_pos, format_pos;
} file_data;

typedef struct {
  int data_color, selected_data_color, mix_color;
  int white, black, red, yellow, green, light_blue, lighter_blue;
  int fltenv_basic_gc, fltenv_data_gc;
  int basic_gc, selected_basic_gc, combined_basic_gc;        
  int cursor_gc, selected_cursor_gc;      
  int selection_gc, selected_selection_gc;
  int erase_gc, selected_erase_gc;        
  int mark_gc, selected_mark_gc;          
  int mix_gc, selected_mix_gc;           
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
#define COLOR_TYPE int
#define KEY_TO_NAME(key) NULL

enum {snd_K_Shift_L, snd_K_space, snd_K_openparen, snd_K_closeparen, snd_K_plus, snd_K_minus, snd_K_period, snd_K_slash, snd_K_0, snd_K_1, snd_K_2, snd_K_3, snd_K_4, snd_K_5, snd_K_6, snd_K_7, snd_K_8, snd_K_9, snd_K_less, snd_K_greater, snd_K_A, snd_K_B, snd_K_C, snd_K_D, snd_K_E, snd_K_F, snd_K_G, snd_K_H, snd_K_I, snd_K_J, snd_K_K, snd_K_L, snd_K_M, snd_K_N, snd_K_O, snd_K_P, snd_K_Q, snd_K_R, snd_K_S, snd_K_T, snd_K_U, snd_K_V, snd_K_W, snd_K_X, snd_K_Y, snd_K_Z, snd_K_underscore, snd_K_a, snd_K_b, snd_K_c, snd_K_d, snd_K_e, snd_K_f, snd_K_g, snd_K_h, snd_K_i, snd_K_j, snd_K_k, snd_K_l, snd_K_m, snd_K_n, snd_K_o, snd_K_p, snd_K_q, snd_K_r, snd_K_s, snd_K_t, snd_K_u, snd_K_v, snd_K_w, snd_K_x, snd_K_y, snd_K_z, snd_K_Home, snd_K_Left, snd_K_Up, snd_K_Right, snd_K_Down, snd_keypad_PageUp, snd_keypad_PageDown, snd_keypad_Insert, snd_keypad_Delete, snd_keypad_Left, snd_keypad_Up, snd_keypad_Right, snd_keypad_Down, snd_keypad_Multiply, snd_keypad_Add, snd_keypad_Subtract, snd_keypad_Divide, snd_keypad_Decimal, snd_keypad_Enter, snd_keypad_0, snd_keypad_1, snd_keypad_2, snd_keypad_3, snd_keypad_4, snd_keypad_5, snd_keypad_6, snd_keypad_7, snd_keypad_8, snd_keypad_9};

typedef struct {
  int nop;
} regrow;

  #define XEN_PIXEL_P(Value) (XEN_LIST_P(Value) && \
                             (XEN_LIST_LENGTH(Value) >= 2) && \
                             (XEN_SYMBOL_P(XEN_CAR(Value))) && \
                             (strcmp("Pixel", XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))

#endif
